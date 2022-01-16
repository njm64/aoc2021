open Base

type result =
  | Hit of int
  | Miss

type point = {
  x : int;
  y : int;
}

type input = {
  min : point;
  max : point;
}

let hit_target t p =
  p.x >= t.min.x && p.x <= t.max.x && p.y >= t.min.y && p.y <= t.max.y

let missed_target t p v =
  (* We've passed the maximum x, assuming target is always to the right *)
  p.x > t.max.x
  (* We've stopped moving horizontally but we're outside the target range *)
  || (v.x = 0 && (p.x < t.min.x || p.x > t.max.x))
  (* We've moving down, and are outside the target range *)
  || (v.y < 0 && p.y < t.min.y)

let rec run_simulation target p v max_height =
  let apply_drag x = if x > 0 then x - 1 else if x < 0 then x + 1 else 0 in
  let p = { x = p.x + v.x; y = p.y + v.y } in
  let v = { x = apply_drag v.x; y = v.y - 1 } in
  let max_height = Int.max p.y max_height in
  if hit_target target p then Hit max_height
  else if missed_target target p v then Miss
  else run_simulation target p v max_height

let find_hits target =
  let max_height = ref 0 in
  let hit_count = ref 0 in
  let start = { x = 0; y = 0 } in
  (* Assuming target is always to the right, the x component of the
     velocity must be from 0 to target.max.x. Any greater, and it
     will overshoot. *)
  for x = 0 to target.max.x do
    for y = -1000 to 1000 do
      match run_simulation target start { x; y } 0 with
      | Hit h ->
          max_height := Int.max !max_height h;
          Int.incr hit_count
      | Miss -> ()
    done
  done;
  (!max_height, !hit_count)

let parse_input lines =
  let open Caml.Scanf in
  let line = List.hd_exn lines in
  sscanf line "target area: x=%d..%d, y=%d..%d" (fun x1 x2 y1 y2 ->
      { min = { x = x1; y = y1 }; max = { x = x2; y = y2 } })

let part1 target =
  let max_height, _ = find_hits target in
  max_height

let part2 target =
  let _, hit_count = find_hits target in
  hit_count
