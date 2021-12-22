open Base
open Stdio
open Caml.Scanf

(* Range from inclusive min to exclusive max *)
type range = {
  min : int;
  max : int;
}

type cube = {
  x : range;
  y : range;
  z : range;
}

type cmd = {
  status : char;
  cube : cube;
}

let status_on = Char.of_int_exn 1
let status_off = Char.of_int_exn 0

(* Parse input. Convert max values to exclusive as we go *)
let parse_line line =
  let toggle, coords = String.lsplit2_exn line ~on:' ' in
  let status = if String.equal toggle "on" then status_on else status_off in
  sscanf coords "x=%d..%d,y=%d..%d,z=%d..%d" (fun x1 x2 y1 y2 z1 z2 ->
      let x = { min = x1; max = x2 + 1 } in
      let y = { min = y1; max = y2 + 1 } in
      let z = { min = z1; max = z2 + 1 } in
      { status; cube = { x; y; z } })

let parse_input lines = List.map lines ~f:parse_line

let in_init_region cmd =
  let { x; y; z } = cmd.cube in
  x.min >= -50 && x.max <= 50 && y.min >= -50 && y.max <= 50 && z.min >= -50
  && z.max <= 50

let make_axis cmds ~f =
  List.map cmds ~f
  |> List.concat_map ~f:(fun r -> [ r.min; r.max ])
  |> List.dedup_and_sort ~compare:Int.compare
  |> Array.of_list

let map_int axis n =
  Array.binary_search axis ~compare:Int.compare `First_equal_to n
  |> Option.value_exn

let map_range axis r = { min = map_int axis r.min; max = map_int axis r.max }

let run_cmds cmds =
  let x_axis = make_axis cmds ~f:(fun c -> c.cube.x) in
  let y_axis = make_axis cmds ~f:(fun c -> c.cube.y) in
  let z_axis = make_axis cmds ~f:(fun c -> c.cube.z) in

  let xs = Array.length x_axis in
  let ys = Array.length y_axis in
  let zs = Array.length z_axis in
  let reactor = Bytes.make (xs * ys * zs) status_off in
  let count = ref 0 in

  List.iter cmds ~f:(fun cmd ->
      let xr = map_range x_axis cmd.cube.x in
      let yr = map_range y_axis cmd.cube.y in
      let zr = map_range z_axis cmd.cube.z in
      for x = xr.min to xr.max - 1 do
        for y = yr.min to yr.max - 1 do
          for z = zr.min to zr.max - 1 do
            let i = x + (y * xs) + (z * xs * ys) in
            Bytes.set reactor i cmd.status
          done
        done
      done);

  for x = 0 to xs - 1 do
    for y = 0 to ys - 1 do
      for z = 0 to zs - 1 do
        let i = x + (y * xs) + (z * xs * ys) in
        if Char.equal (Bytes.get reactor i) status_on then
          let x = x_axis.(x + 1) - x_axis.(x) in
          let y = y_axis.(y + 1) - y_axis.(y) in
          let z = z_axis.(z + 1) - z_axis.(z) in
          count := !count + (x * y * z)
      done
    done
  done;
  !count

let run () =
  let cmds = In_channel.read_lines "input/day22.txt" |> parse_input in

  let init_cmds = List.filter ~f:in_init_region cmds in
  let count1 = run_cmds init_cmds in
  printf "Part 1: %d\n" count1;

  let count2 = run_cmds cmds in
  printf "Part 2: %d\n" count2
