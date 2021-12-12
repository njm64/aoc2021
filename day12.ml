open Base
open Stdio

(* Build a graph from the input data, represented as a map from source
   node to a list of connected destination nodes. Add all links in both
   directions. *)
let build_graph lines =
  let m = Map.empty (module String) in
  List.fold_left lines ~init:m ~f:(fun m line ->
      let a, b = String.lsplit2_exn line ~on:'-' in
      Map.add_multi m ~key:a ~data:b |> Map.add_multi ~key:b ~data:a)

(* Return true if the given cave is a small cave. We don't consider the
   start and end caves small caves, since they are handled differently. *)
let is_small_cave = function
  | "start" | "end" -> false
  | _ as s -> String.for_all s ~f:Char.is_lowercase

(* We maintain a map of the number of times each small cave was visited.
   This function increments the visit count for a given small cave. *)
let update_visit_counts m cave =
  if is_small_cave cave then
    Map.update m cave ~f:(fun v ->
        match v with
        | Some v -> v + 1
        | None -> 1)
  else m

(* Given a frequency map from cave to visit count, return the number of
   times a cave was visited more than once. *)
let get_dup_visit_count m =
  let total = Map.data m |> List.fold_left ~init:0 ~f:( + ) in
  let unique = Map.length m in
  total - unique

(* Find paths from src to the end node  *)
let rec find_paths graph src visit_count_map max_dups =
  if get_dup_visit_count visit_count_map > max_dups then []
  else if String.equal src "end" then [ [ "end" ] ]
  else
    let m = update_visit_counts visit_count_map src in
    Map.find_multi graph src
    |> List.filter ~f:(fun c -> not (String.equal c "start"))
    |> List.map ~f:(fun c -> find_paths graph c m max_dups)
    |> List.concat
    |> List.map ~f:(List.cons src)

let run () =
  let graph = In_channel.read_lines "input/day12.txt" |> build_graph in
  let m = Map.empty (module String) in

  (* Part 1: No duplicate visits to small caves allowed *)
  let paths1 = find_paths graph "start" m 0 in
  printf "Part1: %d\n" (List.length paths1);

  (* Part 2: 1 duplicate visit to a small cave allowed *)
  let paths2 = find_paths graph "start" m 1 in
  printf "Part2: %d\n" (List.length paths2)
