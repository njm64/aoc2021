open Base
open Stdio

type input = int list

let parse_input lines = List.map ~f:Int.of_string lines

let count_incs lst =
  let rec step count = function
    | a :: (b :: _ as tl) ->
        if b > a then step (count + 1) tl else step count tl
    | _ -> count
  in
  step 0 lst

let count_incs2 lst =
  let rec step count = function
    | a :: (b :: c :: d :: _ as tl) ->
        if b + c + d > a + b + c then step (count + 1) tl else step count tl
    | _ -> count
  in
  step 0 lst

let run () =
  let nums = In_channel.read_lines "input/day1.txt" |> parse_input in
  printf "Part 1: %d\n" (count_incs nums);
  printf "Part 2: %d\n" (count_incs2 nums)
