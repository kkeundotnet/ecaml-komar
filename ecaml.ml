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
open Parsercomb


type template_content =
  | Text of string
  | BlockOfCode of string
  | BlockOfExpr of string
  | BlockOfRawExpr of string
  | LineOfCode of string

type template = template_content list


let whitespace = function ' ' | '\t' -> true | _ -> false
let p_whitespace = p_pred whitespace

let template_of_string s =
  let code_fragment =
    (p_str "<%=" >>> (
      (p_str "raw" >>>
      p_str_until (p_str "%>") >>= fun (expr, _) ->
      return (BlockOfRawExpr expr))
      |||
      (p_str_until (p_str "%>") >>= fun (expr, _) ->
      return (BlockOfExpr expr))))
    |||
    (p_str "<%" >>>
    p_str_until (p_str "%>") >>= fun (code, _) ->
    return (BlockOfCode code))
    |||
    (((check_prev (p_char '\n') >>> return ()) ||| (p_pos 0 >>> return ())) >>>
    p_many p_whitespace >>>
    p_char '%' >>>
    p_str_until (p_str "\n") >>= fun (code, _) ->
    return (LineOfCode code)) in
  let rec loop acc =
    let text_then_code =
      p_str_until code_fragment >>= fun (text, code) ->
      (* FIXME *)
      if text = ""
      then loop (code::acc)
      else loop (code::(Text text)::acc) in
    (* no more code - return the rest *)
    let rest (s, pos) =
      let text = String.slice ~first:pos s in
      let rev_r =
        if text = ""
        then acc
        else (Text text)::acc in
      return (List.rev rev_r) (s, String.length s) in
    text_then_code ||| rest in
  match loop [] (s, 0) with
  | Failed -> assert false
  | Parsed (r, _) -> r

let print_template printer esc_printer chan =
  List.iter (function
      | LineOfCode s -> fprintf chan "%s\n" s
      | BlockOfCode s -> fprintf chan "%s" s
      | BlockOfExpr s -> fprintf chan "%s (%s);" esc_printer s
      | BlockOfRawExpr s -> fprintf chan "%s (%s);" printer s
      | Text s -> (
          let l = String.nsplit s "\n" in
          let rec loop =
            function
            | [""] -> ()
            | h::[] -> fprintf chan "%s %S;" printer h
            | h::t  -> fprintf chan "%s %S;\n" printer (h ^ "\n"); loop t
            | [] -> () in
          loop l)
    )

open Arg

let () =
  let source = ref "" in
  let dest   = ref "" in
  let printer = ref "" in
  let esc_printer = ref "" in
  let header = ref "" in
  let footer = ref "" in
  let add_directive = ref false in
  let help =
    "ecaml - a simple template tool for OCaml\n" ^
    "Usage: ecaml [OPTIONS] template.eml" in
  let l = [
    "-o", Set_string dest, "FILE\tdestination file to output an OCaml code; default is sourcename.ml";
    "-p", Set_string printer, "STR\tprinter function to apply to strings; default is print_string";
    "-esc-p", Set_string esc_printer, "STR\tfunction to apply to <%= %> parts but not to <%=raw %>; default is the same as -p";
    "-d", Set add_directive, "\t\twrite a directive with original file name for more impressive error messages";
    "-header", Set_string header, "STR\theader to write before the output";
    "-footer", Set_string footer, "STR\tfooter to write after the output";
  ] in
  Arg.parse l (fun a -> source := a) help;
  if !source = "" then (eprintf "you must specify a source template to parse\n"; exit 1);
  if !dest = ""; then (
    let l = String.nsplit !source "." in
    let l =
      match List.rev l with
      | "eml"::tl -> "ml"::tl
      | l -> "ml"::l in
    dest := (String.concat "." (List.rev l))
  );
  if !printer = "" then printer := "print_string";
  if !esc_printer = "" then esc_printer := !printer;

  let s = Std.input_file !source in
  let t = template_of_string s in
  let chan = open_out !dest in
  if !header <> "" then begin
    if !add_directive then begin
      output_string chan "# 1 \"_ecaml_header_\"\n";
    end;
    output_string chan !header;
    output_char chan '\n';
  end;
  if !add_directive then
    fprintf chan"# 1 %S\n" (Filename.basename !source);
  print_template !printer !esc_printer chan t;
  if !footer <> "" then begin
    if !add_directive then begin
      output_char chan '\n';
      output_string chan "# 1 \"_ecaml_footer_\"\n";
    end;
    output_string chan !footer;
  end;
  close_out chan

