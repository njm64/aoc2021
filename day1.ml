open Base
open Stdio

(* Part 1 *)

let count_incs lst =
  let rec step count = function
    | a :: (b :: _ as tl) ->
        if b > a then step (count + 1) tl 
        else step count tl
    | _ -> count
  in
  step 0 lst

let part1 () =
  In_channel.read_lines "input/day1.txt"
  |> List.map ~f:Int.of_string 
  |> count_incs 
  |> printf "%d\n"

(* Part 2 *)

let count_incs2 lst =
  let rec step count = function
    | a :: (b :: c :: d :: _ as tl) ->
        if b + c + d > a + b + c then step (count + 1) tl 
        else step count tl
    | _ -> count
  in
  step 0 lst

let part2 () =
  In_channel.read_lines "input/day1.txt"
  |> List.map ~f:Int.of_string 
  |> count_incs2 
  |> printf "%d\n"
