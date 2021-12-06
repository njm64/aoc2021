open Base
open Stdio

(* Parse input into a list of fish, represented as (timer, count) tuples *)
let parse_input lines =
  List.hd_exn lines
    |> String.split ~on:',' 
    |> List.map ~f:(fun s -> (Int.of_string s, 1))

(* Group fish with the same timer value together *)
let group fish = 
  let count_with_time t =
    List.filter fish ~f:(fun (t',_) -> t = t')
    |> List.map ~f:snd
    |> List.fold ~init:0 ~f:(+)
  in
  List.range 0 9 |> List.map ~f:(fun t -> t, count_with_time t)

(* Perform one iteration on a list of fish *)
let iterate fish =
  List.fold fish ~init:[] ~f:(fun acc t ->
    match t with
    | (0, count) -> (6, count) :: (8, count) :: acc
    | (t, count) -> ((t - 1), count) :: acc)

(* Count the total number of fish *)
let count fish = List.map fish ~f:snd |> List.fold ~init:0 ~f:(+)

let run_simulation fish n = 
  let f = Fn.compose iterate group in
  let total = Fn.apply_n_times ~n f fish |> count in
  printf "After %d iterations: %d\n" n total

let run () =
  let fish = In_channel.read_lines "input/day6.txt" |> parse_input in
  run_simulation fish 80;
  run_simulation fish 256;
  
