open Base

type input = int list

let num_bits = 12
let parse_binary s = Int.of_string ("0b" ^ s)
let parse_input = List.map ~f:parse_binary

(* Get the bit at index i *)
let get_bit n i = (n lsr i) land 1

(* Return the counts of 0 and 1 bits as a tuple *)
let count_bits lst i =
  let mask = 1 lsl i in
  let c1 = List.count lst ~f:(fun n -> n land mask <> 0) in
  let c0 = List.length lst - c1 in
  (c0, c1)

(* Return the most common bit at index i, defaulting to 1 if they are equal *)
let most_common_bit lst i =
  let c0, c1 = count_bits lst i in
  if c1 >= c0 then 1 else 0

(* Return the least common bit at index i, defaulting to 0 if they are equal *)
let least_common_bit lst i =
  let c0, c1 = count_bits lst i in
  if c0 <= c1 then 0 else 1

let calc_gamma nums =
  List.range 0 num_bits
  |> List.map ~f:(most_common_bit nums)
  |> List.foldi ~init:0 ~f:(fun i acc b -> acc lor (b lsl i))

let calc_epsilon nums =
  List.range 0 num_bits
  |> List.map ~f:(least_common_bit nums)
  |> List.foldi ~init:0 ~f:(fun i acc b -> acc lor (b lsl i))

let calc_oxygen nums =
  let rec step nums i =
    let b = most_common_bit nums i in
    match List.filter nums ~f:(fun x -> get_bit x i = b) with
    | [ x ] -> x
    | xs -> step xs (i - 1)
  in
  step nums (num_bits - 1)

let calc_co2 nums =
  let rec step nums i =
    let b = least_common_bit nums i in
    match List.filter nums ~f:(fun x -> get_bit x i = b) with
    | [ x ] -> x
    | xs -> step xs (i - 1)
  in
  step nums (num_bits - 1)

let part1 nums = calc_gamma nums * calc_epsilon nums
let part2 nums = calc_oxygen nums * calc_co2 nums
