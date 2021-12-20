open Base
open Stdio

type point = {
  x : int;
  y : int;
  z : int;
}
[@@deriving compare]

type scanner = {
  id : int;
  points : point list;
}

type axis =
  | PositiveX
  | NegativeX
  | PositiveY
  | NegativeY
  | PositiveZ
  | NegativeZ

let all_axes =
  [ PositiveX; NegativeX; PositiveY; NegativeY; PositiveZ; NegativeZ ]

type axis_mapping = {
  axis : axis;
  offset : int;
}

type mapping = {
  xmap : axis_mapping;
  ymap : axis_mapping;
  zmap : axis_mapping;
}

let parse_point s =
  match String.split s ~on:',' |> List.map ~f:Int.of_string with
  | [ x; y; z ] -> { x; y; z }
  | _ -> failwith "Invalid point"

let parse_scanner lines =
  match List.filter lines ~f:(Fn.non String.is_empty) with
  | [] -> failwith "Unexpected input"
  | hd :: tl ->
      let id =
        String.chop_prefix_exn hd ~prefix:"--- scanner "
        |> String.chop_suffix_exn ~suffix:" ---"
        |> Int.of_string
      in
      let points =
        List.filter tl ~f:(fun s -> not (String.is_empty s))
        |> List.map ~f:parse_point
      in
      { id; points }

let parse_input lines =
  List.group lines ~break:(fun _ b -> String.is_empty b)
  |> List.map ~f:parse_scanner

let identity_mapping =
  {
    xmap = { axis = PositiveX; offset = 0 };
    ymap = { axis = PositiveY; offset = 0 };
    zmap = { axis = PositiveZ; offset = 0 };
  }

let point_to_scalar axis p =
  match axis with
  | PositiveX -> p.x
  | NegativeX -> -p.x
  | PositiveY -> p.y
  | NegativeY -> -p.y
  | PositiveZ -> p.z
  | NegativeZ -> -p.z

let apply_mapping p m =
  {
    x = point_to_scalar m.xmap.axis p + m.xmap.offset;
    y = point_to_scalar m.ymap.axis p + m.ymap.offset;
    z = point_to_scalar m.zmap.axis p + m.zmap.offset;
  }

let apply_mappings p m = List.fold_left m ~init:p ~f:apply_mapping

(* Attempt to match two scanners with given axes and an offset *)
let match_scanners a b axis_a axis_b offset =
  let list_a = List.map a.points ~f:(point_to_scalar axis_a) in
  let list_b =
    List.map b.points ~f:(fun p -> point_to_scalar axis_b p + offset)
  in
  let set_a = Set.of_list (module Int) list_a in
  let set_b = Set.of_list (module Int) list_b in
  let count = Set.inter set_a set_b |> Set.length in
  count >= 10

(* Attempt to match scanner a on axis_a against scanner b.
   If successful, returns the axis and offset for scanner b *)
let attempt_match a b axis_a =
  With_return.with_return (fun r ->
      List.iter all_axes ~f:(fun axis_b ->
          for offset = -2000 to 2000 do
            if match_scanners a b axis_a axis_b offset then
              r.return (Some { axis = axis_b; offset })
          done);
      None)

(* Attempt to match two scanners on all axes. If successful, returns
   mappings for the X, Y, and Z axes *)
let match_scanners a b =
  let open Option.Let_syntax in
  let%bind xmap = attempt_match a b PositiveX in
  let%bind zmap = attempt_match a b PositiveZ in
  let%bind ymap = attempt_match a b PositiveY in
  Some { xmap; ymap; zmap }

(* Attempt to resolve a single scanner against a list of resolved scanners.
   If successful, returns a mapping list. *)
let resolve_scanner resolved s =
  List.find_map resolved ~f:(fun (r, mappings) ->
      match match_scanners r s with
      | Some m -> Some (m :: mappings)
      | None -> None)

(* Attempt to resolve a list of unresolved scanners against a list
   of resolved scanners. Returns a tuple of newly resolved scanners,
   and still unresolved scanners. *)
let resolve_scanners resolved unresolved =
  List.partition_map unresolved ~f:(fun s ->
      match resolve_scanner resolved s with
      | Some mappings -> First (s, mappings)
      | None -> Second s)

(* Main resolve loop. We keep track of three lists of scanners. Ones
   that are fully resolved, ones that were resolved in the last iteration,
   and unresolved ones. Each iteration, we try to resolve the unresolved
   scanners against the newly resolved ones. *)
let rec resolve_loop resolved newly_resolved unresolved =
  printf "Resolved %d Newly resolved: %d Unresolved: %d\n%!"
    (List.length resolved)
    (List.length newly_resolved)
    (List.length unresolved);
  let succeeded, failed = resolve_scanners newly_resolved unresolved in
  if List.length succeeded = 0 then failwith "Failed to resolve"
  else if List.length failed = 0 then
    List.concat [ resolved; newly_resolved; succeeded ]
  else resolve_loop (List.append resolved newly_resolved) succeeded failed

(* Take a (scanner, mappings) pair, and return all points for this
   scanner with mappings applied *)
let map_scanner_points (scanner, mappings) =
  List.map scanner.points ~f:(fun p -> apply_mappings p mappings)

(* Take raw scanner data, and return a list of beacon positions and
   scanner positions, all relative to the first scanner. *)
let find_all scanners =
  let resolved =
    let newly_resolved = [ (List.hd_exn scanners, [ identity_mapping ]) ] in
    let unresolved = List.tl_exn scanners in
    resolve_loop [] newly_resolved unresolved
  in
  let beacon_positions =
    List.map resolved ~f:map_scanner_points
    |> List.concat
    |> List.sort ~compare:compare_point
    |> List.remove_consecutive_duplicates ~equal:(fun a b ->
           compare_point a b = 0)
  in
  let scanner_positions =
    List.map resolved ~f:(fun (_, mappings) ->
        apply_mappings { x = 0; y = 0; z = 0 } mappings)
  in
  (beacon_positions, scanner_positions)

let manhattan_distance a b =
  Int.abs (a.x - b.x) + Int.abs (a.y - b.y) + Int.abs (a.z - b.z)

let max_manhattan_distance points =
  let max = ref 0 in
  List.iter points ~f:(fun a ->
      List.iter points ~f:(fun b ->
          let d = manhattan_distance a b in
          max := Int.max d !max));
  !max

let run () =
  let scanners = In_channel.read_lines "input/day19.txt" |> parse_input in
  let beacon_positions, scanner_positions = find_all scanners in
  printf "Unique beacons: %d\n" (List.length beacon_positions);
  printf "Maximum distance: %d\n" (max_manhattan_distance scanner_positions)
