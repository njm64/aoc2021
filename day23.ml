open Base
open Stdio

type pos = {
  row : int;
  col : int;
}

type metrics = {
  row_hallway : int;
  row_room_top : int;
  row_room_bottom : int;
  hallway_length : int;
  map_width : int;
  map_height : int;
  room_rows : int list;
  hallway_dsts : pos list;
}

type amphipod = {
  kind : char;
  pos : pos;
}

type state = {
  metrics : metrics;
  energy : int;
  amphipods : amphipod list;
  map : string;
}

let col_amber, col_bronze, col_copper, col_desert = (2, 4, 6, 8)

(* Metrics are calculated dynamically because they vary by phase *)
let init_metrics phase =
  let row_room_top = 1 in
  let row_room_bottom = if phase = 1 then 2 else 4 in
  let hallway_length = 11 in
  {
    row_hallway = 0;
    row_room_top;
    row_room_bottom;
    hallway_length;
    map_width = hallway_length + 1;
    map_height = row_room_bottom + 1;
    room_rows =
      List.range ~start:`inclusive ~stop:`inclusive ~stride:(-1) row_room_bottom
        row_room_top;
    hallway_dsts =
      List.map [ 0; 1; 3; 5; 7; 9; 10 ] ~f:(fun col -> { row = 0; col });
  }

let col_for_kind kind =
  match kind with
  | 'A' -> col_amber
  | 'B' -> col_bronze
  | 'C' -> col_copper
  | 'D' -> col_desert
  | _ -> failwith "Invalid kind"

(* Get the room for a position. Return None if in hallway *)
let room_for_pos s pos =
  if pos.row = s.metrics.row_hallway then None
  else if pos.col = col_amber then Some 'A'
  else if pos.col = col_bronze then Some 'B'
  else if pos.col = col_copper then Some 'C'
  else if pos.col = col_desert then Some 'D'
  else failwith "Invalid position"

let cost_for_kind kind =
  match kind with
  | 'A' -> 1
  | 'B' -> 10
  | 'C' -> 100
  | 'D' -> 1000
  | _ -> failwith "Invalid kind"

(* Render the map as a string *)
let build_map m amphipods =
  let b = Bytes.make (m.map_width * m.map_height) '#' in
  for i = 0 to m.map_height - 1 do
    Bytes.set b ((i * m.map_width) + m.map_width - 1) '\n'
  done;
  for i = 0 to m.hallway_length - 1 do
    Bytes.set b i '.'
  done;
  for i = m.row_room_top to m.row_room_bottom do
    Bytes.set b ((i * m.map_width) + col_amber) '.';
    Bytes.set b ((i * m.map_width) + col_bronze) '.';
    Bytes.set b ((i * m.map_width) + col_copper) '.';
    Bytes.set b ((i * m.map_width) + col_desert) '.'
  done;
  List.iter amphipods ~f:(fun a ->
      Bytes.set b ((a.pos.row * m.map_width) + a.pos.col) a.kind);
  Bytes.to_string b

(* Parse input. Note we offset the row and col by 1 because our coordinate
   system doesn't include the ascii borders from the input. *)
let parse_input lines =
  List.map [ 1; 2 ] ~f:(fun row ->
      let s = List.nth_exn lines (row + 1) in
      List.map [ col_amber; col_bronze; col_copper; col_desert ] ~f:(fun col ->
          { kind = String.get s (col + 1); pos = { row; col } }))
  |> List.concat

(* Get the character at a given map position *)
let map_get s pos = String.get s.map ((pos.row * s.metrics.map_width) + pos.col)

(* Return true if the given position is unoccupied *)
let is_pos_clear s pos = Char.equal (map_get s pos) '.'

(* Check to see if all positions in the given path are clear *)
let is_path_clear s path = List.for_all path ~f:(is_pos_clear s)

(* Return a list of all positions in a room, ordered from bottom to top *)
let room_positions s kind =
  let col = col_for_kind kind in
  List.map s.metrics.room_rows ~f:(fun row -> { row; col })

(* Find the first clear position in a room, from the bottom up *)
let empty_room_position s kind =
  room_positions s kind |> List.find_exn ~f:(is_pos_clear s)

(* Return true if there are any strange amphipods in the given room *)
let strangers_in_room s kind =
  room_positions s kind
  |> List.exists ~f:(fun pos ->
         let c = map_get s pos in
         not (Char.equal c '.' || Char.equal c kind))

(* Generate a path from source to destination *)
let get_path s src dst =
  let hpath row src_col dst_col =
    let stride = if src_col < dst_col then 1 else -1 in
    List.range ~start:`exclusive ~stop:`inclusive ~stride src_col dst_col
    |> List.map ~f:(fun col -> { row; col })
  in
  let vpath col src_row dst_row =
    let stride = if src_row < dst_row then 1 else -1 in
    List.range ~start:`exclusive ~stop:`inclusive ~stride src_row dst_row
    |> List.map ~f:(fun row -> { row; col })
  in
  if src.col = dst.col && src.row <> dst.row then
    (* Moving up or down within the same room *)
    vpath src.col src.row dst.row
  else if src.row <> s.metrics.row_hallway && dst.row = s.metrics.row_hallway
  then
    (* Room to hallway *)
    let p1 = vpath src.col src.row s.metrics.row_hallway in
    let p2 = hpath s.metrics.row_hallway src.col dst.col in
    p1 @ p2
  else if src.row = s.metrics.row_hallway && dst.row <> s.metrics.row_hallway
  then
    (* Hallway to room *)
    let p1 = hpath s.metrics.row_hallway src.col dst.col in
    let p2 = vpath dst.col src.row dst.row in
    p1 @ p2
  else if
    src.col <> dst.col
    && src.row <> s.metrics.row_hallway
    && dst.row <> s.metrics.row_hallway
  then
    let p1 = vpath src.col src.row s.metrics.row_hallway in
    let p2 = hpath s.metrics.row_hallway src.col dst.col in
    let p3 = vpath dst.col s.metrics.row_hallway dst.row in
    p1 @ p2 @ p3
  else failwith "Invalid path"

let get_valid_destinations s a =
  match (strangers_in_room s a.kind, room_for_pos s a.pos) with
  (* Strangers in our room, and we're in the hallway. Can't go anywhere *)
  | true, None -> []
  (* Strangers in our room, and we're in a room. We can only go to the hallway.
     If we're in our room, we're allowed to leave if there are strangers there,
     as we may need to get out of the way to allow them to leave. *)
  | true, Some _ -> s.metrics.hallway_dsts
  (* No strangers in our room, and we're in the hallway. We can go to our room. *)
  | false, None -> [ empty_room_position s a.kind ]
  (* No strangers in our room, and we're in it. No need to go anywhere. *)
  | false, Some r when Char.equal r a.kind -> []
  (* No strangers in our room, and we're in a different room. We can go either
     to our room, or to the hallway. *)
  | false, Some _ -> empty_room_position s a.kind :: s.metrics.hallway_dsts

(* Helper to replace an element in a list *)
let replace lst a b = List.map lst ~f:(fun e -> if phys_equal e a then b else e)

(* Get possible new states from moving one amphipod *)
let get_new_states_for_amphipod s a =
  get_valid_destinations s a
  |> List.filter_map ~f:(fun dst ->
         let path = get_path s a.pos dst in
         if is_path_clear s path then
           let e = List.length path * cost_for_kind a.kind in
           let amphipods = replace s.amphipods a { a with pos = dst } in
           let map = build_map s.metrics amphipods in
           Some { metrics = s.metrics; energy = s.energy + e; amphipods; map }
         else None)

let is_complete s =
  List.for_all s.amphipods ~f:(fun a ->
      match room_for_pos s a.pos with
      | None -> false
      | Some room -> Char.equal room a.kind)

(* Djikstra search state *)
type search_state =
  | Open of int
  | Closed

(* Return true if the given state is closed
   (i.e. we've already processed its connections *)
let is_closed ht s =
  match Hashtbl.find ht s.map with
  | Some Closed -> true
  | _ -> false

(* Return true if we should process this state
   (i.e. if we haven't already reached it with less energy) *)
let should_process_state ht s =
  match Hashtbl.find ht s.map with
  | Some Closed -> false
  | Some (Open e) -> s.energy < e
  | None -> true

(* The search algorithm uses a priority queue of states, sorted by energy *)
module SearchQueue = Binary_heap.Make (struct
  type t = state

  let compare a b = Int.compare a.energy b.energy
end)

(* Main search loop, this is basically just Djikstra using the map as our state key *)
let find_solution s =
  let queue = SearchQueue.create ~dummy:s 100 in
  SearchQueue.add queue s;
  let ht = Hashtbl.create (module String) in
  let rec next () =
    let s = SearchQueue.pop_minimum queue in
    if is_complete s then s.energy
    else if is_closed ht s then next ()
    else (
      List.map s.amphipods ~f:(get_new_states_for_amphipod s)
      |> List.concat
      |> List.filter ~f:(should_process_state ht)
      |> List.iter ~f:(fun new_state ->
             Hashtbl.set ht ~key:new_state.map ~data:(Open new_state.energy);
             SearchQueue.add queue new_state);
      next ())
  in
  next ()

(* Add the extra amphipods for phase 2 *)
let adjust_input_for_phase2 amphipods =
  let extra =
    [
      { kind = 'D'; pos = { row = 2; col = col_amber } };
      { kind = 'D'; pos = { row = 3; col = col_amber } };
      { kind = 'C'; pos = { row = 2; col = col_bronze } };
      { kind = 'B'; pos = { row = 3; col = col_bronze } };
      { kind = 'B'; pos = { row = 2; col = col_copper } };
      { kind = 'A'; pos = { row = 3; col = col_copper } };
      { kind = 'A'; pos = { row = 2; col = col_desert } };
      { kind = 'C'; pos = { row = 3; col = col_desert } };
    ]
  in
  List.map amphipods ~f:(fun a ->
      if a.pos.row = 2 then { a with pos = { a.pos with row = 4 } } else a)
  @ extra

let run_phase1 amphipods =
  let metrics = init_metrics 1 in
  let map = build_map metrics amphipods in
  find_solution { amphipods; metrics; map; energy = 0 }

let run_phase2 amphipods =
  let amphipods = adjust_input_for_phase2 amphipods in
  let metrics = init_metrics 2 in
  let map = build_map metrics amphipods in
  find_solution { amphipods; metrics; map; energy = 0 }

let run () =
  let amphipods = In_channel.read_lines "input/day23.txt" |> parse_input in
  printf "Phase 1: %d\n" (run_phase1 amphipods);
  printf "Phase 2: %d\n" (run_phase2 amphipods)
