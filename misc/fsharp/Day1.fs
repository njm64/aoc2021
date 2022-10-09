module Aoc.Day1

let parseInput lines = List.map int lines

let part1 depths =
    let rec step count =
        function
        | a :: (b :: _ as tl) ->
            if b > a then
                step (count + 1) tl
            else
                step count tl
        | _ -> count
    step 0 depths

let part2 depths =
    let rec step count =
        function
        | a :: (b :: c :: d :: _ as tl) ->
            if b + c + d > a + b + c then
                step (count + 1) tl
            else
                step count tl
        | _ -> count
    step 0 depths

let run () = Util.run 1 parseInput part1 part2
