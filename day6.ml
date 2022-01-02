open Base

type input = (int * int) list

(* Parse input into a list of fish, represented as (timer, count) tuples *)
let parse_input lines =
  List.hd_exn lines |> String.split ~on:','
  |> List.map ~f:(fun s -> (Int.of_string s, 1))

(* Count the total number of fish in a list *)
let count fish = List.map fish ~f:snd |> List.fold ~init:0 ~f:( + )

(* Count the number of fish with a given timer *)
let count_time fish t = List.filter fish ~f:(fun (t', _) -> t = t') |> count

(* Group fish with the same timer value together *)
let group fish = List.range 0 9 |> List.map ~f:(fun t -> (t, count_time fish t))

(* Perform one iteration on a list of fish *)
let iterate fish =
  List.fold fish ~init:[] ~f:(fun acc t ->
      match t with
      | 0, count -> (6, count) :: (8, count) :: acc
      | t, count -> (t - 1, count) :: acc)

let run_simulation fish n =
  let f = Fn.compose iterate group in
  Fn.apply_n_times ~n f fish |> count

let part1 fish = run_simulation fish 80
let part2 fish = run_simulation fish 256
