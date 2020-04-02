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

open ExtLib
open Printf

type elem =
  | Text of string
  | BlockOfCode of string
  | BlockOfExpr of string
  | BlockOfRawExpr of string
  | LineOfCode of string

type t = elem list

let print ~printer ~esc_printer ch =
  List.iter (function
    | LineOfCode s -> fprintf ch "%s\n" s
    | BlockOfCode s -> fprintf ch "%s" s
    | BlockOfExpr s -> fprintf ch "%s (%s);\n" esc_printer s
    | BlockOfRawExpr s -> fprintf ch "%s (%s);\n" printer s
    | Text s -> fprintf ch "%s %S;\n" printer s)

(* Continuation *)

type 'a parsed = Parsed of { v : 'a; st : string * int } | Failed

let parsed v st = Parsed { v; st }

let ( ||| ) p1 p2 x =
  match p1 x with Parsed _ as parsed -> parsed | Failed -> p2 x

let ( >>| ) p f x =
  match p x with Parsed { v; st } -> Parsed { v = f v; st } | Failed -> Failed

let ( >>> ) p1 p2 x =
  match p1 x with Parsed { v = (); st } -> p2 st | Failed -> Failed

(* Parse *)

let p_pred p (s, n) =
  assert (n >= 0);
  if n < String.length s && p s.[n] then Parsed { v = (); st = (s, succ n) }
  else Failed

(** parsed if current position = p; doesn't jump *)
let p_pos x ((s, pos) as st) = if x = pos then Parsed { v = (); st } else Failed

let p_whitespace = p_pred (function ' ' | '\t' -> true | _ -> false)

let p_char c = p_pred (( = ) c)

(* checks previous char; doesn't jump *)
let p_prev_char c (s, pos) = if pos <= 0 then Failed else p_char c (s, pred pos)

let p_str str = String.fold_left (fun p c -> p >>> p_char c) (parsed ()) str

let p_until_parsed p ((s, pos) as st) =
  let len = String.length s in
  let first = pos in
  let rec loop ((s, pos) as st) =
    if pos >= len then Failed
    else
      match p st with
      | Parsed { v; st = (s, _) as st } ->
          Parsed { v = (String.slice ~first ~last:pos s, v); st }
      | Failed -> loop (s, succ pos)
  in
  loop st

let p_until_char_parsed c = p_until_parsed (p_char c) >>| fun (v, _c) -> v

let p_until_str_parsed str = p_until_parsed (p_str str) >>| fun (v, _str) -> v

let p_until_failed p st =
  let rec loop st =
    match p st with
    | Parsed { st; _ } -> loop st
    | Failed -> Parsed { v = (); st }
  in
  loop st

let code_fragment =
  p_str "<%="
  >>> ( p_str "raw" >>> p_until_str_parsed "%>"
      >>| (fun expr -> BlockOfRawExpr expr)
      ||| (p_until_str_parsed "%>" >>| fun expr -> BlockOfExpr expr) )
  ||| (p_str "<%" >>> p_until_str_parsed "%>" >>| fun code -> BlockOfCode code)
  ||| ( p_prev_char '\n' ||| p_pos 0
      >>> p_until_failed p_whitespace
      >>> p_char '%' >>> p_until_char_parsed '\n'
      >>| fun code -> LineOfCode code )

let template_of_string s =
  let cons_text text acc = if text = "" then acc else Text text :: acc in
  let rec loop ((s, pos) as st) acc =
    match p_until_parsed code_fragment st with
    | Parsed { v = text, code; st } -> loop st (code :: cons_text text acc)
    | Failed -> cons_text (String.slice ~first:pos s) acc |> List.rev
  in
  loop (s, 0) []

let () =
  let source = ref "" in
  let dest = ref "" in
  let printer = ref "" in
  (* TODO: no esc_printer *)
  let esc_printer = ref "" in
  let header = ref "" in
  let footer = ref "" in
  let add_directive = ref false in
  let help =
    "ecaml - a simple template tool for OCaml\n"
    ^ "Usage: ecaml [OPTIONS] template.eml"
  in
  let l =
    [
      ( "-o",
        Arg.Set_string dest,
        "FILE\tdestination file to output an OCaml code; default is \
         sourcename.ml" );
      ( "-p",
        Set_string printer,
        "STR\tprinter function to apply to strings; default is print_string" );
      ( "-esc-p",
        Set_string esc_printer,
        "STR\tfunction to apply to <%= %> parts but not to <%=raw %>; default \
         is the same as -p" );
      ( "-d",
        Set add_directive,
        "\t\twrite a directive with original file name for more impressive \
         error messages" );
      ("-header", Set_string header, "STR\theader to write before the output");
      ("-footer", Set_string footer, "STR\tfooter to write after the output");
    ]
  in
  Arg.parse l (fun a -> source := a) help;
  if !source = "" then (
    eprintf "you must specify a source template to parse\n";
    exit 1 );
  ( if !dest = "" then
    let l = String.nsplit !source "." in
    let l =
      match List.rev l with "eml" :: tl -> "ml" :: tl | l -> "ml" :: l
    in
    dest := String.concat "." (List.rev l) );
  if !printer = "" then printer := "print_string";
  if !esc_printer = "" then esc_printer := !printer;

  let s = Std.input_file !source in
  let t = template_of_string s in
  let chan = open_out !dest in
  if !header <> "" then (
    if !add_directive then output_string chan "# 1 \"_ecaml_header_\"\n";
    output_string chan !header;
    output_char chan '\n' );
  if !add_directive then fprintf chan "# 1 %S\n" (Filename.basename !source);
  print !printer !esc_printer chan t;
  if !footer <> "" then (
    if !add_directive then (
      output_char chan '\n';
      output_string chan "# 1 \"_ecaml_footer_\"\n" );
    output_string chan !footer );
  close_out chan
