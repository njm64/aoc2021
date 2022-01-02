open Base
open Stdio

let parse_cmd s =
  let op, n = String.lsplit2_exn s ~on:' ' in
  (op, Int.of_string n)

let parse_input lines = List.map ~f:parse_cmd lines

let apply_cmd (pos, depth) = function
  | "forward", x -> (pos + x, depth)
  | "down", x -> (pos, depth + x)
  | "up", x -> (pos, depth - x)
  | cmd, _ -> failwith ("Unknown command " ^ cmd)

let run_part1 cmds =
  let pos, depth = List.fold_left ~init:(0, 0) ~f:apply_cmd cmds in
  pos * depth

let apply_cmd2 (pos, depth, aim) = function
  | "forward", x -> (pos + x, depth + (aim * x), aim)
  | "down", x -> (pos, depth, aim + x)
  | "up", x -> (pos, depth, aim - x)
  | cmd, _ -> failwith ("Unknown command " ^ cmd)

let run_part2 cmds =
  let pos, depth, _ = List.fold_left ~init:(0, 0, 0) ~f:apply_cmd2 cmds in
  pos * depth

let run () =
  let cmds = In_channel.read_lines "input/day2.txt" |> parse_input in
  printf "Part 1: %d\n" (run_part1 cmds);
  printf "Part 2: %d\n" (run_part2 cmds)
