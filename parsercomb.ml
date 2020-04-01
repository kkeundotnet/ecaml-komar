(* This file is part of ecaml.
 *
 * ecaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ecaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ecaml.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2012-2013 Alexander Markov *)

open ExtString

type 'a parse_result = Parsed of { v : 'a; st : string * int } | Failed

let return x st = Parsed { v = x; st }

let p_pred p (s, n) =
  try
    let c = s.[n] in
    if p c then Parsed { v = c; st = (s, succ n) } else Failed
  with Invalid_argument _ -> Failed

let p_pred2 p (s, n) =
  try
    let c = s.[n] in
    match p c with
    | Some r -> Parsed { v = r; st = (s, succ n) }
    | None -> Failed
  with Invalid_argument _ -> Failed

let p_char c = p_pred (( = ) c)

(** just returns current position; doesn't jump *)
let current_pos pos = Parsed { v = pos; st = pos }

(** parsed if current position = p; doesn't jump *)
let p_pos p (s, pos) = if p = pos then return p (s, pos) else Failed

(** sets position *)
let set_pos n (s, _) = Parsed { v = (); st = (s, n) }

let dont_jump p (s, n) =
  match p (s, n) with
  | Parsed { v = r; _ } -> Parsed { v = r; st = (s, n) }
  | Failed -> Failed

let p_begin (s, n) = if n = 0 then Parsed { v = (); st = (s, n) } else Failed

let p_end (s, n) =
  if String.length s = n then Parsed { v = (); st = (s, n) } else Failed

let p_somechar (s, n) =
  try
    let c = s.[n] in
    Parsed { v = c; st = (s, succ n) }
  with Invalid_argument _ -> Failed

let p_manyf_arg prs f v0 =
  let rec loop v st =
    match prs v st with
    | Parsed { v = x; st = s' } -> loop (f v x) s'
    | Failed -> Parsed { v; st }
  in
  loop v0

let p_many prs =
  let rec loop st =
    match prs st with
    | Parsed { st = s'; _ } -> loop s'
    | Failed -> Parsed { v = (); st }
  in
  loop

let p_upto_timesf times prs f v0 =
  let rec loop t v st =
    if t < times then
      match prs st with
      | Parsed { v = x; st = s' } -> loop (succ t) (f v x) s'
      | Failed -> Parsed { v; st }
    else Parsed { v; st }
  in
  loop 0 v0

let p_opt defval p s =
  match p s with
  | Parsed _ as ok -> ok
  | Failed -> Parsed { v = defval; st = s }

let ( ||| ) p1 p2 s = match p1 s with Parsed _ as ok -> ok | Failed -> p2 s

let ( >>= ) p1 f s =
  match p1 s with Parsed { v = x; st = s2 } -> f x s2 | Failed -> Failed

let ( >>> ) p1 p2 s =
  match p1 s with Parsed { st = s2; _ } -> p2 s2 | Failed -> Failed

let p_plus prs = prs >>> p_many prs

let p_manyf prs f v0 =
  let rec loop v st =
    match prs st with
    | Parsed { v = x; st = s' } -> loop (f v x) s'
    | Failed -> Parsed { v; st }
  in
  loop v0

let p_manyf_ends_with prs f v0 e =
  let rec loop v st =
    match prs st with
    | Parsed { v = x; st = s' } -> loop (f v x) s'
    | Failed -> (
        match e st with
        | Parsed { st; _ } -> Parsed { v; st }
        | Failed -> Failed )
  in
  loop v0

let p_plusf prs f v0 = prs >>= fun x -> return (f v0 x) >>= p_manyf prs f

let isdigit c = c >= '0' && c <= '9'

let p_digit = p_pred isdigit

let mkInt v x = (v * 10) + int_of_char x - 48

let p_unsign_int = p_plusf p_digit mkInt 0

let p_int (s, pos) =
  try
    let c = s.[pos] in
    let t = (s, succ pos) in
    match c with
    | '-' -> (
        match p_manyf p_digit mkInt 0 t with
        | Parsed { v = x; st = s' } -> Parsed { v = -x; st = s' }
        | Failed -> Failed )
    | '0' .. '9' -> p_manyf p_digit mkInt 0 (s, pos)
    | _ -> Failed
  with Invalid_argument _ -> Failed

let p_str str = String.fold_left (fun p c -> p >>> p_char c) (return '!') str

(** sequence of something *)
let p_seq prs =
  p_manyf prs (fun acc x -> x :: acc) [] >>= fun rl -> return (List.rev rl)

(* too slow, disabled *)
(*let rec p_seq1 prs = (* sequence of something *)
  prs >>= fun x ->
  p_opt [x] (p_seq1 prs >>= fun lst -> return (x::lst))*)

let p_seq1 prs =
  prs >>= fun v0 ->
  p_manyf prs (fun acc x -> x :: acc) [ v0 ] >>= fun rl -> return (List.rev rl)

let rec p_list prs psep =
  (* list of something, separated by given separator parser *)
  prs >>= fun x ->
  p_opt [ x ] (psep >>> p_list prs psep >>= fun lst -> return (x :: lst))

let rec p_listch prs sep =
  (* list of something, separated by given char *)
  prs >>= fun x ->
  p_opt [ x ] (p_char sep >>> p_listch prs sep >>= fun lst -> return (x :: lst))

let p_intlist = p_listch p_int

let p_void prs s =
  match prs s with
  | Parsed { st = s'; _ } -> Parsed { v = (); st = s' }
  | Failed -> Failed

let mkFloat (fv, fr) c =
  (fv +. (float_of_int (int_of_char c - 48) *. fr), fr *. 0.1)

let p_float =
  p_opt 1.0 (p_char '-' >>> return (-1.0)) >>= fun sign ->
  p_manyf p_digit mkInt 0 >>= fun n ->
  p_char '.' >>> p_manyf p_digit mkFloat (0.0, 0.1) >>= fun (fv, _) ->
  return (sign *. (float_of_int n +. fv))

let p_str_until until (s, pos) =
  let beg = pos in
  let rec loop (s, pos) =
    match until (s, pos) with
    | Parsed { v = until_r; st = s, new_p } ->
        Parsed
          {
            v = (String.slice ~first:beg ~last:pos s, until_r);
            st = (s, new_p);
          }
    | Failed ->
        (* FIXME *)
        if pos = String.length s then Failed else loop (s, succ pos)
  in
  loop (s, pos)

(* seems like a replacement for parsec's manyTill *)
let p_until p until (s, pos) =
  let beg = pos in
  let rec loop (s, pos) =
    match until (s, pos) with
    | Parsed { v = until_r; st = s, new_p } ->
        Parsed
          {
            v = (String.slice ~first:beg ~last:pos s, until_r);
            st = (s, new_p);
          }
    | Failed -> (
        match p (s, pos) with
        | Parsed _ -> loop (s, succ pos)
        | Failed -> Failed )
  in
  loop (s, pos)

(* checks previous char; doesn't jump *)
let check_prev p (s, pos) =
  let prev_pos = pos - 1 in
  (p >>= fun r _ -> Parsed { v = r; st = (s, pos) }) (s, prev_pos)
