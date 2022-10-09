module Aoc.Day8

module List =
    let count f lst =
        List.fold (fun acc s -> if f s then acc + 1 else acc) 0 lst

// Parse each line into a tuple of two lists
let parseLine (s: string) =
    let t =
        s.Split('|')
        |> Array.map (fun (s: string) -> s.Trim().Split(' ') |> List.ofArray)
    (t[0], t[1])

// Parse input into a list of records
let parseInput lines = List.map parseLine lines

// Part 1 : given a record, count all the codes with length 2, 3, 4, or 7
let countUnique (_, b) =
    let codes = set [ 2; 3; 4; 7 ]
    List.count (fun s -> Set.contains (String.length s) codes) b
