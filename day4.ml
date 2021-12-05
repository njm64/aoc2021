open Base
open Stdio

let board_size = 5
let board_range = List.range 0 board_size

(* Parse a board from a list of lines. A board is represented as array 
   of rows, where each row is an array of (number, marked) tuples. *)
let parse_board lines =
  let parse_line s = 
    String.split ~on:' ' s 
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:(fun s -> (Int.of_string s, false))
    |> Array.of_list 
  in
  List.tl_exn lines
    |> List.map ~f:parse_line 
    |> Array.of_list

(* Parse input, and return a list of numbers and a list of boards. *)
let parse_input lines =
  let numbers = List.hd_exn lines 
    |> String.split ~on:',' 
    |> List.map ~f:Int.of_string 
  in
  let boards = List.tl_exn lines
    |> List.chunks_of ~length:(board_size + 1)
    |> List.map ~f:parse_board
  in
  (numbers, boards)

(* Mark board with number n, and return an updated board *)
let mark_board n board =
  Array.map board ~f:(fun row ->
    Array.map row ~f:(fun cell ->
      if n = (fst cell) then (n, true) else cell))

(* Check board for victory conditions *)
let check_board board = 
  let check_row i = Array.for_all board.(i) ~f:snd in
  let check_col i = Array.for_all board ~f:(fun r -> snd r.(i)) in
  List.exists board_range ~f:check_row || List.exists board_range ~f:check_col

(* Given a list of boards and a list of numbers, apply the numbers to
   boards, until a bingo is found. Return the winning board, and the
   winning number. *)
let rec find_first_bingo boards numbers =
  match numbers with
  | [] -> failwith "Bingo not found"
  | n::tl ->
      let boards = List.map boards ~f:(mark_board n) in
      match List.find boards ~f:check_board with
      | Some board -> (board, n)
      | None -> find_first_bingo boards tl

(* Find all boards that get a bingo, and return a list of them and 
   their winning numbers, in order. Not tail recursive. *)
let rec find_all_bingos boards numbers =
  match numbers, boards with
  (* If there are no more numbers or boards, we're done *)
  | [], _ -> [] 
  | _, [] -> [] 
  | n::tl, boards ->
      (* Mark all boards with this number, and then partition into
         a list of winning and remaining boards.*)
      let boards = List.map boards ~f:(mark_board n) in
      let winning, remaining = List.partition_tf boards ~f:check_board in
      (* Annotate each winning board with the winning digit, then recurse. *)
      let r = List.map winning ~f:(fun b -> (b, n)) in
      List.append r (find_all_bingos remaining tl)

(* To find the last bingo, find all bingos, then grab the last one *)
let find_last_bingo boards numbers = 
  find_all_bingos boards numbers |> List.last_exn

(* Calculate the score for a board i.e. the sum of all unmarked cells *)
let calc_score board =
  let score_for_cell (n, marked) = if marked then 0 else n in
  Array.to_list board
    |> Array.concat
    |> Array.map ~f:score_for_cell
    |> Array.fold ~init:0 ~f:(+)
  
(* Print a board for debugging *)
let print_board b = 
  Array.iter b ~f:(fun r ->
    Array.iter r ~f:(fun (n, marked) -> 
      printf "%2d%c " n (if marked then '*' else ' '));
    printf "\n")

let print_result (board, num) =
  let score = calc_score board in
  printf "score %d number %d -> %d\n" score num (score * num);
  print_board board;
  printf "\n"

let run () =
  let lines = In_channel.read_lines "input/day4.txt" in
  let (numbers, boards) = parse_input lines in
  printf "First bingo: ";
  print_result (find_first_bingo boards numbers);
  printf "Last bingo: ";
  print_result (find_last_bingo boards numbers)
