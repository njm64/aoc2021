open Base

type result =
  | Ok
  | UnexpectedToken of char
  | UnterminatedExpr of char list

type input = result list

let rec parse_expr lst tags =
  match (lst, tags) with
  (* No more tokens, and an empty tag stack *)
  | [], [] -> Ok
  (* No more tokens, but there are still unclosed tags in the stack *)
  | [], tags -> UnterminatedExpr tags
  (* Open an expression. Just add the closing tag to the tag stack and
     keep parsing. This is tail recursive. *)
  | '{' :: tl, tags -> parse_expr tl ('}' :: tags)
  | '[' :: tl, tags -> parse_expr tl (']' :: tags)
  | '<' :: tl, tags -> parse_expr tl ('>' :: tags)
  | '(' :: tl, tags -> parse_expr tl (')' :: tags)
  (* Some other token, but the tag stack is empty *)
  | hd :: _, [] -> UnexpectedToken hd
  (* Got a token, check it matches the top one on the tag stack *)
  | hd :: tl, hd_tag :: tl_tags ->
      if Char.equal hd hd_tag then parse_expr tl tl_tags else UnexpectedToken hd

let parse_line s = parse_expr (String.to_list s) []
let parse_input lines = List.map lines ~f:parse_line

let unexpected_token_score = function
  | UnexpectedToken ')' -> 3
  | UnexpectedToken ']' -> 57
  | UnexpectedToken '}' -> 1197
  | UnexpectedToken '>' -> 25137
  | _ -> 0

let autocomplete_token_score = function
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | _ -> 0

let autocomplete_score = function
  | UnterminatedExpr tags ->
      Some
        (List.fold_left tags ~init:0 ~f:(fun acc c ->
             (acc * 5) + autocomplete_token_score c))
  | _ -> None

let middle lst =
  let n = List.length lst in
  List.nth lst (n / 2) |> Option.value_exn

let part1 results =
  List.map results ~f:unexpected_token_score |> List.fold_left ~init:0 ~f:( + )

let part2 results =
  List.filter_map results ~f:autocomplete_score
  |> List.sort ~compare:Int.compare
  |> middle
