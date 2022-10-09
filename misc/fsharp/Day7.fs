module Aoc.Day7

// Parse input into a list of numbers
let parseInput lines =
    List.head lines
    |> fun (s: string) -> s.Split(",")
    |> List.ofArray
    |> List.map int

// Build a table with the cost for travelling distance i at each index
let makeFuelTable max =
    let arr = Array.create (max + 1) 0

    for i = 1 to max do
        arr[i] <- arr[i - 1] + i

    arr

// Calculate fuel to move all crabs to a given position
let calcFuel crabs costFn pos =
    List.map (fun p -> abs (pos - p) |> costFn) crabs
    |> List.sum

// Calculate minimum fuel using a cost function f
let calcMinFuel crabs f =
    [ 0 .. List.max crabs ]
    |> List.map (calcFuel crabs f)
    |> List.min

// Part 1: Cost is just the identity function
let part1 crabs = calcMinFuel crabs id

// Part 2: Build a lookup table with fuel costs
let part2 crabs =
    let fuelTable = makeFuelTable (List.max crabs)
    calcMinFuel crabs (fun n -> fuelTable[n])

let run () = Util.run 7 parseInput part1 part2
