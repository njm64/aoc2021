open Base

type input = (string list * string list) list

(* Parse each line into a tuple of two lists *)
let parse_line s =
  let a, b = String.lsplit2_exn s ~on:'|' in
  ( String.strip a |> String.split ~on:' ',
    String.strip b |> String.split ~on:' ' )

(* Parse input into a list of records *)
let parse_input lines = List.map lines ~f:parse_line

(* Part 1: given a record, count all the codes with length 2, 3, 4, or 7 *)
let count_unique (_, b) =
  List.count b ~f:(fun s ->
      match String.length s with
      | 2 | 3 | 4 | 7 -> true
      | _ -> false)

(* Helper function for permuations *)
let rec interleave e lst =
  match lst with
  | [] -> [ [ e ] ]
  | hd :: tl -> (e :: lst) :: List.map ~f:(fun e -> hd :: e) (interleave e tl)

(* Calculate all possible permuations of a list *)
let rec permutations lst =
  match lst with
  | [] -> [ lst ]
  | hd :: tl -> List.concat (List.map ~f:(interleave hd) (permutations tl))

(* Generate all possible keys  *)
let all_keys =
  [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ]
  |> permutations |> List.map ~f:String.concat

(* Convert a code to a digit. Assumes the code is sorted. *)
let code_to_digit = function
  | "abcefg" -> '0'
  | "cf" -> '1'
  | "acdeg" -> '2'
  | "acdfg" -> '3'
  | "bcdf" -> '4'
  | "abdfg" -> '5'
  | "abdefg" -> '6'
  | "acf" -> '7'
  | "abcdefg" -> '8'
  | "abcdfg" -> '9'
  | _ -> '?'

(* Decode a code using the given key *)
let decode key code =
  let f c = String.get key (Char.to_int c - Char.to_int 'a') in
  String.map code ~f |> String.to_list
  |> List.sort ~compare:Char.compare
  |> String.of_char_list |> code_to_digit

(* Find a key using the codes on the left hand side of an input line.
   For each possible key, we try to decode the code, and then sort the
   decoded digits. If it matches "0123456789", we have the correct key. *)
let find_key codes =
  List.find_exn all_keys ~f:(fun key ->
      List.map codes ~f:(decode key)
      |> List.sort ~compare:Char.compare
      |> String.of_char_list |> String.equal "0123456789")

(* Find the key using the left hand side of a record, and use it to decode
   the digits on the right hand side. *)
let decode_record (a, b) =
  let key = find_key a in
  List.map b ~f:(decode key) |> String.of_char_list |> Int.of_string

let sum_list = List.reduce_exn ~f:( + )

(* Part 1: Just count the codes with unique segment counts *)
let part1 records = List.map records ~f:count_unique |> sum_list

(* Part 2: Decode all records, and find the sum *)
let part2 records = List.map records ~f:decode_record |> sum_list
