open Base

let max_int = List.reduce_exn ~f:Int.max

type input = int list

(* Parse input into a list of numbers *)
let parse_input lines =
  List.hd_exn lines |> String.split ~on:',' |> List.map ~f:Int.of_string

(* Build a table with the cost for travelling distance i at each index *)
let make_fuel_table max =
  let arr = Array.create ~len:(max + 1) 0 in
  for i = 1 to max do
    arr.(i) <- arr.(i - 1) + i
  done;
  arr

(* Calculate the fuel to move all crabs to a given position *)
let calc_fuel crabs cost_fn pos =
  List.map crabs ~f:(fun p -> Int.abs (pos - p) |> cost_fn)
  |> List.fold ~init:0 ~f:( + )

(* Calculate minimum fuel using a cost function f *)
let calc_min_fuel crabs f =
  List.range 0 (max_int crabs + 1)
  |> List.map ~f:(calc_fuel crabs f)
  |> List.reduce_exn ~f:Int.min

(* Part 1: Cost is just the identity function *)
let part1 crabs = calc_min_fuel crabs Fn.id

(* Part 2: Build a lookup table with fuel costs *)
let part2 crabs =
  let fuel_table = make_fuel_table (max_int crabs) in
  calc_min_fuel crabs (fun n -> fuel_table.(n))
