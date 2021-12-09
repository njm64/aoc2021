open Base
open Stdio

(* Parse each line into an array of digits *)
let parse_line s = String.to_array s |> Array.map ~f:Char.get_digit_exn

(* Parse input into a height map *)
let parse_input lines = List.map lines ~f:parse_line |> Array.of_list

(* Width and height of the height map *)
let height m = Array.length m
let width m = Array.length m.(0)

(* Return the height for an (x,y) position on the height map. 
   If the position is out of bounds, return the maximum height 9 *)
let get_height m x y =
  if y < 0 || y >= (height m) || x < 0 || x >= (width m) then 9
  else m.(y).(x)

(* Calculate the total risk for the height map *)
let get_risk m =
  let risk = ref 0 in
  for x = 0 to (width m) - 1 do
    for y = 0 to (height m) - 1 do
      let h = get_height m x y in
      if h < get_height m (x - 1) y &&
         h < get_height m (x + 1) y &&
         h < get_height m x (y - 1) &&
         h < get_height m x (y + 1) then
           risk := h + 1 + !risk
    done
  done;
  !risk

(* Get the basin size at an (x,y) coordinate. Note this mutates the map. *)
let rec basin_size m x y =
  if get_height m x y = 9 then 0 else (
    (* Set the height of this point to 9 so we won't consider it again *)
    m.(y).(x) <- 9;
    (* Recursively calculate the size of the basin *)
    basin_size m (x - 1) y +
    basin_size m (x + 1) y +
    basin_size m x (y - 1) +
    basin_size m x (y + 1) + 1
  )

(* Get a list of all basin sizes *)
let basin_sizes m =
  let sizes = ref [] in
  for x = 0 to (width m) - 1 do
    for y = 0 to (height m) - 1 do
      let size = basin_size m x y in
      if size > 0 then
        sizes := (size :: !sizes);
    done
  done;
  !sizes

let run () =
  let m = In_channel.read_lines "input/day9.txt" |> parse_input in

  (* Part 1 *)
  let risk = get_risk m in
  printf "Risk: %d\n" risk;

  (* Part 2 *)
  let p = basin_sizes m 
    |> List.sort ~compare:(fun a b -> Int.compare b a)
    |> List.sub ~pos:0 ~len:3
    |> List.reduce_exn ~f:( * )
  in
  printf "Product of basin sizes: %d\n" p
