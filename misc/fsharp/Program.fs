module Aoc.Main

let runDay d =
    match d with
    | 1 -> Day1.run()
    | 2 -> Day2.run()
    | 3 -> Day3.run()
    | 4 -> Day4.run()
    | 5 -> Day5.run()
    | 6 -> Day6.run()
    | 7 -> Day7.run()
    | 22 -> Day22.run()
    | _ -> failwith "Invalid day"

runDay 7





