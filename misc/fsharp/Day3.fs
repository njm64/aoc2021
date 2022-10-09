module Aoc.Day3

open System

let numBits = 12
let parseInput input = input
let parseBinary (s: string) = Convert.ToInt32(s, 2)

// Return the counts of 0 and 1 bits as a tuple
let countBits lst i =
    let c1 =
        List.filter (fun (n: string) -> n[i] = '1') lst
        |> List.length
    let c0 = List.length lst - c1
    (c0, c1)

// Return the most common bit at index i, defaulting to 1 if they are equal
let mostCommonBit lst i =
    let c0, c1 = countBits lst i
    if c1 >= c0 then '1' else '0'

// Return the least common bit at index i, defaulting to 0 if they are equal
let leastCommonBit lst i =
    let c0, c1 = countBits lst i
    if c0 <= c1 then '0' else '1'

let calcGamma nums =
    Seq.map (mostCommonBit nums) [ 0 .. numBits - 1 ]
    |> String.Concat
    |> parseBinary

let calcEpsilon nums =
    Seq.map (leastCommonBit nums) [ 0 .. numBits - 1 ]
    |> String.Concat
    |> parseBinary

let calcOxygen nums =
    let rec step nums i =
        let b = mostCommonBit nums i
        match List.filter (fun (x: string) -> x[i] = b) nums with
        | [ x ] -> x
        | xs -> step xs (i + 1)
    step nums 0 |> parseBinary

let calcCO2 nums =
    let rec step nums i =
        let b = leastCommonBit nums i
        match List.filter (fun (x: string) -> x[i] = b) nums with
        | [ x ] -> x
        | xs -> step xs (i + 1)
    step nums 0 |> parseBinary

let part1 nums = calcGamma nums * calcEpsilon nums
let part2 nums = calcOxygen nums * calcCO2 nums
let run () = Util.run 3 parseInput part1 part2
