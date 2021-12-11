open Base
open Stdio

(* Parse input into a 2d array of integers *)
let parse_line s = String.to_array s |> Array.map ~f:Char.get_digit_exn
let parse_input lines = List.map lines ~f:parse_line |> Array.of_list

(* Width and height of the map *)
let height m = Array.length m
let width m = Array.length m.(0)

(* Call a function for every point on the map *)
let for_all m f =
  for x = 0 to (width m) - 1 do
    for y = 0 to (height m) - 1 do
      f m x y
    done
  done

(* Call a function for all immediate neighbours of (x, y) *)
let for_neighbours m x y f =
  let w, h = (width m), (height m) in
  for xi = x - 1 to x + 1 do
    for yi = y - 1 to y + 1 do
      if xi >= 0 && yi >= 0 && xi < w && yi < h then f m xi yi
    done
  done

(* If the energy at (x,y) is less than 10, then it hasn't flashed yet, so
   increment it. If it hits 10 after incrementing, this is a flash, so
   increment the neighbours too. Note this mutates the map. *)
let rec inc_energy m x y =
  let e = m.(y).(x) in
  if e < 10 then
    m.(y).(x) <- e + 1;
    if e + 1 = 10 then
      for_neighbours m x y inc_energy

(* If the octopus at (x,y) has flashed, reset it *)
let check_reset m x y =
  if m.(y).(x) = 10 then
    m.(y).(x) <- 0

(* Count the number of flashes in the given map *)
let count_flashes m =
  let count_row = Array.count ~f:(Int.equal 10) in
  Array.map m ~f:count_row |> Array.reduce_exn ~f:( + )

(* Perform one step on the given map. Returns an updated map, and the number
   of flashes. The original map is copied, and not mutated. *)
let step m =
  let m = Array.map m ~f:Array.copy in
  for_all m inc_energy;
  let count = count_flashes m in
  for_all m check_reset;
  (m, count)

(* Count the number of flashes after a given number of iterations *)
let count_flashes m iterations =
  let rec next m i total =
    if i = iterations then total
    else 
      let (m, count) = step m in
      next m (i + 1) (total + count)
  in next m 0 0

(* Count the iterations until all cells have flashed *)
let count_until_all_flashed m =
  let rec next m i =
    let (m, count) = step m in
    if count = (width m) * (height m) then i
    else next m (i + 1)
  in next m 1

let run () =
  let m = In_channel.read_lines "input/day11.txt" |> parse_input in

  let part1 = count_flashes m 100 in
  printf "%d flashes\n" part1;

  let part2 = count_until_all_flashed m in
  printf "%d iterations\n" part2
