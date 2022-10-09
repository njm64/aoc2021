module Aoc.Util

open System.IO

let run day parseInput part1 part2 =
    let lines = File.ReadLines($"/Users/nickm/src/aoc/input/day{day}.txt")
    let input = List.ofSeq lines |> parseInput
    let runPart part runner =
        let startTime = System.Environment.TickCount
        let result = runner input
        let msec = (System.Environment.TickCount - startTime)
        let seconds = double msec / 1000.0
        printf "Day %02d Part %d: %-20s %fs\n" day part (string result) seconds
    runPart 1 part1
    runPart 2 part2
