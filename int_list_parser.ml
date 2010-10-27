#use "topfind";;
#thread;;
#camlp4o;;
#require "core_extended";;
#require "seq";;

open Core_extended;;
open Core_extended.Std;;
open Ort;;
open Ort.Function;;

let string_of_list l =
  let b = Buffer.create (List.length l) in
  l |> List.iter ~f:(Buffer.add_char b);
  Buffer.contents b;;

let whitespace = 
  let is_whitespace = List.mem ~set:[' '; '\t'; '\n'] in
  Seq.drop_while ~f:is_whitespace;;

let ints = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'];;

let integer s = s |> Seq.take_while ~f:(List.mem ~set:ints) |> Seq.to_list |> string_of_list |> int_of_string;;

let rec items = parser
  | [< _ = whitespace; i = integer; _ = whitespace; is = items_aux >] -> i::is
and items_aux = parser
  | [< '','; _ = whitespace; i = integer; _ = whitespace; is = items_aux >] -> i::is
  | [<  >] -> [];;

let list = parser
  | [< ''['; l = items; '']' >] -> l;;


let s = "[  22  , 23 ]" |> Seq.of_string;;
list s;;

