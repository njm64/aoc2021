open Base
open Stdio

(* Parse a rule into a tuple of the form ((A, B), C) *)
let parse_rule line =
  match String.split ~on:' ' line with
  | [ key; "->"; data ] ->
      ((String.get key 0, String.get key 1), String.get data 0)
  | _ -> failwith "Unexpected token"

(* Parse a list of lines into a map of insertion rules *)
let parse_rules lines = List.map lines ~f:parse_rule |> Map.Poly.of_alist_exn

(* Parse input into a polymer string, and a map of insertion rules *)
let parse_input lines =
  match lines with
  | polymer :: "" :: rules -> (polymer, parse_rules rules)
  | _ -> failwith "Invalid input"

(* Split a polymer string into a list of pairs with frequencies.
   Initially the frequency of every pair will be 1. *)
let polymer_to_pair_frequencies s =
  let lst = ref [] in
  for i = 0 to String.length s - 2 do
    lst := ((String.get s i, String.get s (i + 1)), 1) :: !lst
  done;
  List.rev !lst

(* Given a list of pairs of the form (key, count), combine
   pairs with the same key by adding their counts together. *)
let simplify_frequency_list lst =
  List.fold lst ~init:Map.Poly.empty ~f:(fun m (key, count) ->
      Map.update m key ~f:(fun e ->
          match e with
          | Some existing -> count + existing
          | None -> count))
  |> Map.to_alist

(* Apply insertion rules to a list of pair frequencies. *)
let rec apply_rules rules pair_frequencies =
  match pair_frequencies with
  | [] -> []
  | ((a, b), count) :: tl -> (
      match Map.find rules (a, b) with
      | Some s -> ((a, s), count) :: ((s, b), count) :: apply_rules rules tl
      | None -> failwith "Insertion rule not found")

(* Given a list of pair frequencies, return the characters with
   the minimum and maximum counts. We include the count of the second
   character in every pair, and the count of the first character in
   the first pair only. *)
let get_element_counts pair_frequencies =
  let count_fst (pair, count) = (fst pair, count) in
  let count_snd (pair, count) = (snd pair, count) in
  List.map pair_frequencies ~f:count_snd
  |> List.cons (List.hd_exn pair_frequencies |> count_fst)
  |> simplify_frequency_list |> List.map ~f:snd
  |> List.sort ~compare:Int.compare
  |> fun lst -> (List.hd_exn lst, List.last_exn lst)

(* Simplify a list of pair frequencies. Need to make sure the first pair is
   not simplified because the ordering is important (i.e. we count both
   elements of the first pair). So remove it before simplifying, and re-add
   it afterwards. *)
let simplify pair_frequencies =
  match pair_frequencies with
  | hd :: tl -> hd :: simplify_frequency_list tl
  | [] -> []

(* Perform one iteration of the pair insertion process, first applying the
   insertion rules, and then simplifying the frequency list. *)
let iterate rules lst = apply_rules rules lst |> simplify

(* Run n iterations, and return the difference between the max & min element *)
let run_iterations pf rules n =
  let min_count, max_count =
    Fn.apply_n_times ~n (iterate rules) pf |> get_element_counts
  in
  max_count - min_count

let run () =
  let polymer, rules = In_channel.read_lines "input/day14.txt" |> parse_input in
  let pair_frequencies = polymer_to_pair_frequencies polymer in
  printf "Part 1: %d\n" (run_iterations pair_frequencies rules 10);
  printf "Part 2: %d\n" (run_iterations pair_frequencies rules 40)
