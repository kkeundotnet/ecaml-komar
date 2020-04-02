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

module P = Printf

let printer ch s = P.fprintf ch "%s" s

let esc_char ch = function
  | '&' -> P.fprintf ch "&amp;"
  | '<' -> P.fprintf ch "&lt;"
  | '>' -> P.fprintf ch "&gt;"
  | '"' -> P.fprintf ch "&quot;"
  | '\'' -> P.fprintf ch "&apos;"
  | c -> P.fprintf ch "%c" c

let esc_printer ch s = String.iter (esc_char ch) s
