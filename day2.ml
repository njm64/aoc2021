open Base
open Stdio

let parse_cmd s =
  let op, n = String.lsplit2_exn s ~on:' ' in
  (op, Int.of_string n)

(* Part 1 *)

let apply_cmd (pos, depth) = function
  | "forward", x -> (pos + x, depth)
  | "down", x -> (pos, depth + x)
  | "up", x -> (pos, depth - x)
  | cmd, _ -> failwith ("Unknown command " ^ cmd)

let part1 () =
  let pos, depth =
    In_channel.read_lines "input/day2.txt"
    |> List.map ~f:parse_cmd
    |> List.fold_left ~init:(0, 0) ~f:apply_cmd
  in
  printf "%d\n" (pos * depth)

(* Part 2 *)

let apply_cmd2 (pos, depth, aim) = function
  | "forward", x -> (pos + x, depth + (aim * x), aim)
  | "down", x -> (pos, depth, aim + x)
  | "up", x -> (pos, depth, aim - x)
  | cmd, _ -> failwith ("Unknown command " ^ cmd)

let part2 () =
  let pos, depth, _ =
    In_channel.read_lines "input/day2.txt"
    |> List.map ~f:parse_cmd
    |> List.fold_left ~init:(0, 0, 0) ~f:apply_cmd2
  in
  printf "%d\n" (pos * depth)
