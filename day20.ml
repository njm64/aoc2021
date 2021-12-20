open Base
open Stdio

type image = {
  width : int;
  height : int;
  pixels : char array array;
  background : char;
}

let parse_input lines =
  let l1, l2 = List.split_while lines ~f:(Fn.non String.is_empty) in
  let iea = String.concat l1 in
  let pixels =
    List.drop_while l2 ~f:String.is_empty
    |> List.map ~f:String.to_array
    |> List.to_array
  in
  let image =
    {
      width = Array.length pixels.(0);
      height = Array.length pixels;
      pixels;
      background = '.';
    }
  in
  (iea, image)

let get_pixel img x y =
  let c =
    if x >= 0 && x < img.width && y >= 0 && y < img.height then
      img.pixels.(y).(x)
    else img.background
  in
  if Char.equal c '#' then 1 else 0

let iea_index src x y =
  (get_pixel src (x - 1) (y - 1) lsl 8)
  lor (get_pixel src (x + 0) (y - 1) lsl 7)
  lor (get_pixel src (x + 1) (y - 1) lsl 6)
  lor (get_pixel src (x - 1) (y + 0) lsl 5)
  lor (get_pixel src (x + 0) (y + 0) lsl 4)
  lor (get_pixel src (x + 1) (y + 0) lsl 3)
  lor (get_pixel src (x - 1) (y + 1) lsl 2)
  lor (get_pixel src (x + 0) (y + 1) lsl 1)
  lor (get_pixel src (x + 1) (y + 1) lsl 0)

let enhance iea src =
  let height = src.height + 2 in
  let width = src.width + 2 in
  let pixels = Array.make_matrix ~dimy:width ~dimx:height '.' in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let i = iea_index src (x - 1) (y - 1) in
      pixels.(y).(x) <- String.get iea i
    done
  done;
  let background = String.get iea (iea_index src (-2) (-2)) in
  { pixels; width; height; background }

let count_pixels img =
  let count_row r = Array.count r ~f:(fun c -> Char.equal c '#') in
  Array.fold img.pixels ~init:0 ~f:(fun acc r -> acc + count_row r)

let run_iterations iea img n =
  Fn.apply_n_times ~n (enhance iea) img |> count_pixels

let run () =
  let iea, img = In_channel.read_lines "input/day20.txt" |> parse_input in
  printf "Part 1: %d\n" (run_iterations iea img 2);
  printf "Part 2: %d\n" (run_iterations iea img 50)
