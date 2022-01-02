open Base

(* Current state for one player *)
type player = {
  id : int;
  pos : int;
  score : int;
}

(* Universe state is represented as (active_player, other_player),
   where active_player is the next player to roll. *)
type input = player * player

(* Parse input and return a universe *)
let parse_input input =
  let pos1 =
    List.nth_exn input 0
    |> String.chop_prefix_exn ~prefix:"Player 1 starting position: "
    |> Int.of_string
  in
  let pos2 =
    List.nth_exn input 1
    |> String.chop_prefix_exn ~prefix:"Player 2 starting position: "
    |> Int.of_string
  in
  let p1 = { id = 1; pos = pos1; score = 0 } in
  let p2 = { id = 2; pos = pos2; score = 0 } in
  (p1, p2)

(* Update the state of a universe with a given total die roll. Swap the
   active and inactive players after each turn*)
let update_universe universe roll =
  let active, inactive = universe in
  let pos = ((active.pos + roll - 1) % 10) + 1 in
  let score = active.score + pos in
  (inactive, { id = active.id; pos; score })

(* Calculate all possible dice totals *)
let dice_totals =
  let t = ref [] in
  for d1 = 1 to 3 do
    for d2 = 1 to 3 do
      for d3 = 1 to 3 do
        t := (d1 + d2 + d3) :: !t
      done
    done
  done;
  !t

(* Given a list of pairs of the form (key, count), combine
   pairs with the same key by adding their counts together. *)
let simplify_freq_list lst =
  List.fold lst ~init:Map.Poly.empty ~f:(fun m (key, count) ->
      Map.update m key ~f:(fun e ->
          match e with
          | Some existing -> count + existing
          | None -> count))
  |> Map.to_alist

(* Return true if a universe has reached the winning state for part 2.*)
let has_won (_active_player, inactive_player) = inactive_player.score >= 21

(* Update a universe with all possible dice totals, and return a new
   list of universes. *)
let universe_permutations (u, count) =
  if has_won u then [ (u, count) ]
  else List.map dice_totals ~f:(fun n -> (update_universe u n, count))

(* Run part 2. Each turn, get a list of all possible permutations for
   all universes, we then simplify that using a frequency map and repeat,
   until all universes meet the winning condition. *)
let rec run_part2 universes =
  if List.map universes ~f:fst |> List.for_all ~f:has_won then universes
  else
    List.map universes ~f:universe_permutations
    |> List.concat |> simplify_freq_list |> run_part2

(* Count wins for the given player id, in a list of (universe,frequency)
   pairs. The winning player is always the player who has just rolled
   (i.e. the inactive player) *)
let count_wins universes id =
  List.filter universes ~f:(fun ((_, p), _) -> p.id = id)
  |> List.map ~f:snd
  |> List.fold_left ~f:( + ) ~init:0

let part1 universe =
  let rec step universe roll_count =
    let d1 = (roll_count % 100) + 1 in
    let d2 = (roll_count % 100) + 2 in
    let d3 = (roll_count % 100) + 3 in
    let u = update_universe universe (d1 + d2 + d3) in
    let active, inactive = u in
    if inactive.score >= 1000 then (roll_count + 3) * active.score
    else step u (roll_count + 3)
  in
  step universe 0

let part2 universe =
  let universes = run_part2 [ (universe, 1) ] in
  let count1 = count_wins universes 1 in
  let count2 = count_wins universes 2 in
  max count1 count2
