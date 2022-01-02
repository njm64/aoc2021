open Base

type input = {
  risks : int array;
  width : int;
  height : int;
}

let parse_line s = String.to_array s |> Array.map ~f:Char.get_digit_exn

let parse_input lines =
  let rows = List.map lines ~f:parse_line in
  let risks = Array.concat rows in
  let width = Array.length (List.hd_exn rows) in
  let height = List.length rows in
  { risks; width; height }

type node_state =
  | Open of int
  | Closed

(* Search state consists of our current position & risk, a map of nodes
   to their current state, and a queue of open nodes sorted by risk. *)
type state = {
  pos : int;
  risk : int;
  map : (int, node_state, Int.comparator_witness) Map.t;
  queue : (int, int list, Int.comparator_witness) Map.t;
}

(* Return a list of all neighbours for position n *)
let neighbours cave n =
  let lst = ref [] in
  let y = n / cave.width in
  let x = n % cave.width in
  if x > 0 then lst := (n - 1) :: !lst;
  if x < cave.width - 1 then lst := (n + 1) :: !lst;
  if y > 0 then lst := (n - cave.width) :: !lst;
  if y < cave.height - 1 then lst := (n + cave.width) :: !lst;
  !lst

(* Pop the next element off the queue. Make sure it's still open, in
   case it was added again with a lower cost. Return a new state
   with updated pos, risk, and queue. *)
let rec find_next state =
  let risk, pos =
    match Map.min_elt state.queue with
    | Some (risk, pos :: _) -> (risk, pos)
    | _ -> failwith "Queue is empty"
  in
  let queue = Map.remove_multi state.queue risk in
  match Map.find state.map pos with
  | Some (Open _) -> { state with pos; risk; queue }
  | _ -> find_next { state with queue }

(* Visit a node n. If the risk it took to get here is less than the
   current risk for the node, then update our state with the new risk.
   Return a new state with updated map and queue. *)
let visit cave state n =
  let risk = state.risk + cave.risks.(n) in
  let update_risk () =
    let map = Map.set state.map ~key:n ~data:(Open risk) in
    let queue = Map.add_multi state.queue ~key:risk ~data:n in
    { state with map; queue }
  in
  match Map.find state.map n with
  | Some (Open r) -> if risk < r then update_risk () else state
  | Some Closed -> state
  | None -> update_risk ()

(* Find a path to the goal, and return total risk *)
let rec find_path cave goal state =
  if state.pos = goal then state.risk
  else
    neighbours cave state.pos
    |> List.fold ~init:state ~f:(visit cave)
    |> find_next |> find_path cave goal

(* Expand cave to 5x width and height *)
let expand_cave cave =
  let width = cave.width * 5 in
  let height = cave.height * 5 in
  let risks = Array.create ~len:(width * height) 0 in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      let orig_x = x % cave.width in
      let orig_y = y % cave.height in
      let risk = cave.risks.((cave.width * orig_x) + orig_y) in
      let inc = (x / cave.width) + (y / cave.width) in
      risks.((width * x) + y) <- ((risk + inc - 1) % 9) + 1
    done
  done;
  { risks; width; height }

let run_with_cave cave =
  let goal = (cave.width * cave.height) - 1 in
  let queue = Map.empty (module Int) in
  let map = Map.empty (module Int) in
  find_path cave goal { pos = 0; risk = 0; map; queue }

let part1 cave = run_with_cave cave
let part2 cave = run_with_cave (expand_cave cave)
