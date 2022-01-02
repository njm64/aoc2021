open Base
open Stdio

let map_size = 1000

type input = (int * int * int * int) list

let parse_cmd s =
  Caml.Scanf.sscanf s "%d,%d -> %d,%d" (fun x1 y1 x2 y2 -> (x1, y1, x2, y2))

let parse_input lines = List.map lines ~f:parse_cmd

let draw_point m x y =
  let i = (y * map_size) + x in
  m.(i) <- m.(i) + 1

let draw_horizontal m y x1 x2 =
  for x = min x1 x2 to max x1 x2 do
    draw_point m x y
  done

let draw_vertical m x y1 y2 =
  for y = min y1 y2 to max y1 y2 do
    draw_point m x y
  done

let draw_diagonal m x1 y1 x2 y2 =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let count = Int.abs dx in
  if Int.abs dx <> Int.abs dy then failwith "Line is not diagonal";
  let xs = if dx < 0 then -1 else 1 in
  let ys = if dy < 0 then -1 else 1 in
  for i = 0 to count do
    draw_point m (x1 + (xs * i)) (y1 + (ys * i))
  done

let draw_line m (x1, y1, x2, y2) ~allow_diagonal =
  if y1 = y2 then draw_horizontal m y1 x1 x2
  else if x1 = x2 then draw_vertical m x1 y1 y2
  else if allow_diagonal then draw_diagonal m x1 y1 x2 y2

let print_map m =
  Array.iteri m ~f:(fun i n ->
      if n > 0 then printf "%d" n else printf ".";
      if (i + 1) % map_size = 0 then printf "\n")

let run_cmds cmds ~allow_diagonal =
  let m = Array.create ~len:(map_size * map_size) 0 in
  List.iter cmds ~f:(draw_line ~allow_diagonal m);
  Array.count m ~f:(fun n -> n >= 2)

let part1 cmds = run_cmds cmds ~allow_diagonal:false
let part2 cmds = run_cmds cmds ~allow_diagonal:true
