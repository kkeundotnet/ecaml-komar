(* ecaml is free software: you can redistribute it and/or modify
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
 * Copyright 2020 kkeun.net *)

module F = Format

let printer f s = F.pp_print_string f s

let esc_char f = function
  | '&' -> F.pp_print_string f "&amp;"
  | '<' -> F.pp_print_string f "&lt;"
  | '>' -> F.pp_print_string f "&gt;"
  | '"' -> F.pp_print_string f "&quot;"
  | '\'' -> F.pp_print_string f "&apos;"
  | c -> F.pp_print_char f c

let esc_printer f s = String.iter (esc_char f) s
