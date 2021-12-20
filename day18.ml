open Base
open Stdio

(* One node in the expression tree *)
type node =
  | Regular of int
  | Pair of node * node
[@@deriving compare]

(* Convert a node to a string, for debugging *)
let rec node_to_string node =
  match node with
  | Regular digit -> Int.to_string digit
  | Pair (left, right) ->
      String.concat [ "["; node_to_string left; ","; node_to_string right; "]" ]

(* Path direction for finding nodes in the tree *)
type path_dir =
  | Left
  | Right
[@@deriving compare]

(* A path to an element in the tree *)
type path = path_dir list [@@deriving compare]

(* Convert a path to a string, for debugging *)
let path_to_string path =
  List.map path ~f:(function
    | Left -> 'L'
    | Right -> 'R')
  |> String.of_char_list

type stream = {
  buf : string;
  mutable pos : int;
}

let next_char s =
  let c = String.get s.buf s.pos in
  s.pos <- s.pos + 1;
  c

(* Parse a single node from the input stream *)
let rec parse_node s =
  let c = next_char s in
  match Char.get_digit c with
  | Some digit -> Regular digit
  | None ->
      if not (Char.equal c '[') then failwith "Expected [";
      let left = parse_node s in
      if not (Char.equal (next_char s) ',') then failwith "Expected comma";
      let right = parse_node s in
      if not (Char.equal (next_char s) ']') then failwith "Expected ]";
      Pair (left, right)

let parse_line line = parse_node { buf = line; pos = 0 }
let parse_input lines = List.map ~f:parse_line lines

(* Replace node at the given path with new_node, and return
   an updated top level node *)
let rec replace node path new_node =
  match (node, path) with
  | _, [] -> new_node
  | Pair (l, r), Left :: tl -> Pair (replace l tl new_node, r)
  | Pair (l, r), Right :: tl -> Pair (l, replace r tl new_node)
  | _, _ -> failwith "Invalid path"

(* Return the child node at a given path *)
let rec node_at_path node path =
  match (node, path) with
  | node, [] -> node
  | Pair (l, _), Left :: tl -> node_at_path l tl
  | Pair (_, r), Right :: tl -> node_at_path r tl
  | _, _ -> failwith "Invalid path"

(* Update the node at a path by applying a function to it *)
let update node path f =
  let existing = node_at_path node path in
  let updated = f existing in
  replace node path updated

(* Coerce a node to an integer *)
let node_to_int = function
  | Regular n -> n
  | _ -> failwith "Expected int "

(* Coerce a node to a pair *)
let node_to_pair = function
  | Pair (left, right) -> (left, right)
  | _ -> failwith "Expected pair"

(* Flatten a node into a list of (node, path) tuples *)
let flatten node =
  let rec step node path =
    match node with
    | Regular _ -> [ (node, path) ]
    | Pair (l, r) ->
        let l_path = List.append path [ Left ] in
        let r_path = List.append path [ Right ] in
        List.concat [ step l l_path; [ (node, path) ]; step r r_path ]
  in
  step node []

(* Helper function to find the path of the last integer before
   a given path. *)
let rec find_int path lst last_int_path =
  match lst with
  | [] -> None
  | (Regular _, p) :: tl ->
      if compare_path p path = 0 then last_int_path
      else find_int path tl (Some p)
  | (Pair _, p) :: tl ->
      if compare_path p path = 0 then last_int_path
      else find_int path tl last_int_path

(* Find the path of the previous/next integer node, from the given path *)
let prev_int node path = find_int path (flatten node) None
let next_int node path = find_int path (flatten node |> List.rev) None

(* Find the path of a node to explode. Return None if nothing to explode. *)
let find_explode_path node =
  let rec step node depth path =
    match node with
    | Regular _ -> None
    | Pair (left, right) -> (
        if depth = 4 then Some path
        else
          match step left (depth + 1) (Left :: path) with
          | Some path -> Some path
          | None -> step right (depth + 1) (Right :: path))
  in
  step node 0 [] |> Option.map ~f:List.rev

(* Add two integer nodes *)
let add_nodes a b =
  match (a, b) with
  | Regular a, Regular b -> Regular (a + b)
  | _ -> failwith "Expected regular nodes"

(* Attempt to explode a node. Return None if there's nothing to explode *)
let explode node =
  let open Option.Let_syntax in
  let%bind path = find_explode_path node in
  let left, right = node_at_path node path |> node_to_pair in
  let replace_zero node = replace node path (Regular 0) in
  let update_left node =
    match prev_int node path with
    | Some path -> update node path (add_nodes left)
    | None -> node
  in
  let update_right node =
    match next_int node path with
    | Some path -> update node path (add_nodes right)
    | None -> node
  in
  Some (node |> replace_zero |> update_left |> update_right)

(* Find path of a node to split. Return None if nothing to split. *)
let find_split_path node =
  let rec step node path =
    match node with
    | Regular n -> if n > 9 then Some path else None
    | Pair (left, right) -> (
        match step left (Left :: path) with
        | Some path -> Some path
        | None -> step right (Right :: path))
  in
  step node [] |> Option.map ~f:List.rev

(* Attempt to split a node. Return None if there's nothing to split *)
let split node =
  let open Option.Let_syntax in
  let%bind path = find_split_path node in
  let n = node_at_path node path |> node_to_int in
  let left = n / 2 in
  let right = left + (n % 2) in
  let updated = replace node path (Pair (Regular left, Regular right)) in
  Some updated

(* Reduce a node by exploding it and splitting it, until there is nothing
   left to reduce *)
let rec reduce node =
  match explode node with
  | Some exploded -> reduce exploded
  | None -> (
      match split node with
      | Some split -> reduce split
      | None -> node)

(* Add a list of numbers together, reducing as we go *)
let add_numbers numbers =
  List.reduce_exn numbers ~f:(fun a b -> Pair (a, b) |> reduce)

(* Calculate the magnitude of a node *)
let rec magnitude node =
  match node with
  | Regular n -> n
  | Pair (l, r) -> (3 * magnitude l) + (2 * magnitude r)

(* Calculate the maximum magnitude of adding any two numbers *)
let max_magnitude numbers =
  let max = ref 0 in
  List.iter numbers ~f:(fun a ->
      List.iter numbers ~f:(fun b ->
          if compare_node a b <> 0 then
            let mag = Pair (a, b) |> reduce |> magnitude in
            max := Int.max !max mag));
  !max

let run () =
  let numbers = In_channel.read_lines "input/day18.txt" |> parse_input in
  printf "Magnitude %d\n" (add_numbers numbers |> magnitude);
  printf "Max magnitude %d\n" (max_magnitude numbers)
