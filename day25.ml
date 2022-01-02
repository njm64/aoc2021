open Base

type input = char array array

let parse_input lines = List.map lines ~f:String.to_array |> List.to_array

let move c (dx, dy) map =
  let w = Array.length map.(0) in
  let h = Array.length map in
  let m = Array.make_matrix ~dimy:w ~dimx:h '.' in
  for x = 0 to w - 1 do
    let x2 = (x + dx) % w in
    for y = 0 to h - 1 do
      let y2 = (y + dy) % h in
      let p = map.(y).(x) in
      if Char.equal p c && Char.equal map.(y2).(x2) '.' then m.(y2).(x2) <- c
      else if not (Char.equal p '.') then m.(y).(x) <- p
    done
  done;
  m

let map_to_string m =
  Array.map m ~f:(fun r -> Array.to_list r |> String.of_char_list)
  |> String.concat_array ~sep:"\n"

let rec iterate map count =
  let s = map_to_string map in
  (*printf "After %d steps:\n%s\n" count s;*)
  let m = map |> move '>' (1, 0) |> move 'v' (0, 1) in
  if String.equal (map_to_string m) s then count + 1 else iterate m (count + 1)

let part1 map = iterate map 0
let part2 _ = 0
