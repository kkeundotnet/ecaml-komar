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

open Core
module F = Format

(* Type *)

type elem =
  | Text of string
  | BlockOfCode of string
  | BlockOfExpr of string
  | BlockOfRawExpr of string
  | LineOfCode of string

type t = elem list

let print f ~printer ~esc_printer x =
  List.iter x ~f:(function
    | LineOfCode s -> F.fprintf f "%s\n" s
    | BlockOfCode s -> F.pp_print_string f s
    | BlockOfExpr s -> F.fprintf f "%s (%s);\n" esc_printer s
    | BlockOfRawExpr s -> F.fprintf f "%s (%s);\n" printer s
    | Text s -> F.fprintf f "%s %S;\n" printer s)

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
let p_pos x ((_s, pos) as st) =
  if x = pos then Parsed { v = (); st } else Failed

let p_whitespace = p_pred (function ' ' | '\t' -> true | _ -> false)

let p_char c = p_pred (fun c' -> Char.equal c c')

(* checks previous char; doesn't jump *)
let p_prev_char c (s, pos) = if pos <= 0 then Failed else p_char c (s, pred pos)

let p_str str = String.fold str ~init:(parsed ()) ~f:(fun p c -> p >>> p_char c)

let p_until_parsed p ((s, pos) as st) =
  let len = String.length s in
  let first = pos in
  let rec loop ((s, pos) as st) =
    if pos >= len then Failed
    else
      match p st with
      | Parsed { v; st = (s, _) as st } ->
          let text = if first < pos then String.slice s first pos else "" in
          Parsed { v = (text, v); st }
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
  let cons_text text acc =
    if String.is_empty text then acc else Text text :: acc
  in
  let rec loop ((s, pos) as st) acc =
    match p_until_parsed code_fragment st with
    | Parsed { v = text, code; st } -> loop st (code :: cons_text text acc)
    | Failed -> cons_text (String.slice s pos (String.length s)) acc |> List.rev
  in
  loop (s, 0) []

(* Main *)

type env = {
  source : string;
  dest : string;
  printer : string;
  esc_printer : string;
  header : string;
  footer : string;
}

let read_arg () =
  let source = ref [] in
  let dest = ref None in
  let printer = ref "Printers.printer Format.std_formatter" in
  let esc_printer = ref "Printers.esc_printer Format.std_formatter" in
  let header = ref None in
  let footer = ref None in
  let add_directive = ref false in
  let help =
    {|ECaml - A simple template tool for OCaml
Usage: ecaml-komar [OPTIONS...] <.eml file>
|}
  in
  Arg.parse
    [
      ( "-o",
        Arg.String (fun v -> dest := Some v),
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
      ( "-header",
        String (fun v -> header := Some v),
        "STR\theader to write before the output" );
      ( "-footer",
        String (fun v -> footer := Some v),
        "STR\tfooter to write after the output" );
    ]
    (fun a -> source := a :: !source)
    help;
  let source =
    match !source with
    | [ source ] -> source
    | _ ->
        prerr_endline "You must specify a source template to parse.";
        exit 1
  in
  let dest =
    match !dest with
    | None -> Filename.chop_extension source ^ ".ml"
    | Some dest -> dest
  in
  let printer = !printer in
  let esc_printer = !esc_printer in
  let add_directive = !add_directive in
  let header =
    (if add_directive then {|# 1 "_ecaml_header_"
|} else "")
    ^ Option.value_map !header ~default:"" ~f:(F.asprintf "%s\n")
    ^
    if add_directive then F.asprintf "# 1 %S\n" (Filename.basename source)
    else ""
  in
  let footer =
    Option.value_map !footer ~default:"" ~f:(F.asprintf "%s\n")
    ^ if add_directive then F.asprintf {|# 1 "_ecaml_footer_"
|} else ""
  in
  { source; dest; printer; esc_printer; header; footer }

let () =
  let { source; dest; printer; esc_printer; header; footer } = read_arg () in
  let v = In_channel.read_all source |> template_of_string in
  Out_channel.with_file ~binary:false dest ~f:(fun ch ->
      let f = F.formatter_of_out_channel ch in
      F.pp_print_string f header;
      print f ~printer ~esc_printer v;
      F.pp_print_string f footer;
      F.pp_print_flush f ())
