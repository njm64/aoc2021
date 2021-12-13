open Base
open Stdio

type fold =
  | Vertical of int
  | Horizontal of int

type point = {
  x : int;
  y : int;
}
[@@deriving compare]

(* Parse a fold command *)
let parse_fold line =
  match String.lsplit2_exn line ~on:'=' with
  | "fold along x", n -> Vertical (Int.of_string n)
  | "fold along y", n -> Horizontal (Int.of_string n)
  | f, _ -> failwith ("Unsupported fold: '" ^ f ^ "'")

(* Parse a point *)
let parse_point line =
  let x, y = String.lsplit2_exn line ~on:',' in
  { x = Int.of_string x; y = Int.of_string y }

(* Parse input into a list of points, and a list of folds *)
let parse_input lines =
  let points, folds =
    List.fold lines ~init:([], []) ~f:(fun (points, folds) line ->
        if String.is_empty line then (points, folds)
        else if String.is_prefix line ~prefix:"fold" then
          (points, parse_fold line :: folds)
        else (parse_point line :: points, folds))
  in
  (points, List.rev folds)

(* Apply a fold to a list of points *)
let fold_points points fold =
  let f =
    match fold with
    | Vertical n ->
        fun { x; y } -> if x > n then { x = n - (x - n); y } else { x; y }
    | Horizontal n ->
        fun { x; y } -> if y > n then { x; y = n - (y - n) } else { x; y }
  in
  List.map points ~f |> List.dedup_and_sort ~compare:compare_point

(* Plot points to the screen. Note arguments to make_matrix are
   inverted, so we can easily print it a row at a time. *)
let plot_points points =
  let max lst = List.max_elt lst ~compare:Int.compare |> Option.value_exn in
  let width = (List.map points ~f:(fun p -> p.x) |> max) + 1 in
  let height = (List.map points ~f:(fun p -> p.y) |> max) + 1 in
  let arr = Array.make_matrix ~dimx:height ~dimy:width ' ' in
  List.iter points ~f:(fun { x; y } -> arr.(y).(x) <- '*');
  for y = 0 to height - 1 do
    printf "%s\n" (arr.(y) |> Array.to_list |> String.of_char_list)
  done

let run () =
  let points, folds = In_channel.read_lines "input/day13.txt" |> parse_input in

  let part1 = List.hd_exn folds |> fold_points points |> List.length in
  printf "Part 1: %d\n" part1;

  let part2 = List.fold folds ~init:points ~f:fold_points in
  printf "Part 2:\n";
  plot_points part2
