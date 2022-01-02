open Base
open Stdio

module type Day = sig
  type input

  val parse_input : string list -> input
  val part1 : input -> int
  val part2 : input -> int
end

let module_for_day d =
  match d with
  | 1 -> Some (module Day1 : Day)
  | 2 -> Some (module Day2 : Day)
  | 3 -> Some (module Day3 : Day)
  | 4 -> Some (module Day4 : Day)
  | 5 -> Some (module Day5 : Day)
  | 6 -> Some (module Day6 : Day)
  | 7 -> Some (module Day7 : Day)
  | 8 -> Some (module Day8 : Day)
  | 9 -> Some (module Day9 : Day)
  | 10 -> Some (module Day10 : Day)
  | 11 -> Some (module Day11 : Day)
  | 12 -> Some (module Day12 : Day)
  | 13 -> Some (module Day13 : Day)
  | 14 -> Some (module Day14 : Day)
  | 15 -> Some (module Day15 : Day)
  | 16 -> Some (module Day16 : Day)
  | 17 -> Some (module Day17 : Day)
  | 18 -> Some (module Day18 : Day)
  | 19 -> Some (module Day19 : Day)
  | 20 -> Some (module Day20 : Day)
  | 21 -> Some (module Day21 : Day)
  | 22 -> Some (module Day22 : Day)
  | 23 -> Some (module Day23 : Day)
  | 24 -> Some (module Day24 : Day)
  | 25 -> Some (module Day25 : Day)
  | _ -> None

let run_day d =
  match module_for_day d with
  | Some m ->
      let module M = (val m : Day) in
      let filename = Printf.sprintf "../input/day%d.txt" d in
      let input = In_channel.read_lines filename |> M.parse_input in
      for i = 1 to 2 do
        let start = Caml.Sys.time () in
        let f = if i = 1 then M.part1 else M.part2 in
        let result = f input in
        let elapsed = Caml.Sys.time () -. start in
        printf "Day %02d Part %d: %-20d %fs\n%!" d i result elapsed
      done
  | None -> printf "No module for day %d\n" d

let () =
  let days =
    try
      let s = Caml.Sys.argv.(1) in
      if String.equal s "all" then List.range 1 25 else [ Int.of_string s ]
    with _ ->
      printf "Usage: aoc <day_number | all>\n";
      Caml.exit 1
  in
  List.iter days ~f:run_day
