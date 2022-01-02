open Base

type input = int list

let parse_input lines = List.map ~f:Int.of_string lines

let part1 depths =
  let rec step count = function
    | a :: (b :: _ as tl) ->
        if b > a then step (count + 1) tl else step count tl
    | _ -> count
  in
  step 0 depths

let part2 depths =
  let rec step count = function
    | a :: (b :: c :: d :: _ as tl) ->
        if b + c + d > a + b + c then step (count + 1) tl else step count tl
    | _ -> count
  in
  step 0 depths
