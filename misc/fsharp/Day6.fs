module Aoc.Day6

// Parse input into a list of fish, represented as (timer, count) tuples
let parseInput lines =
    List.head lines
    |> (fun (s: string) -> s.Split(","))
    |> Array.map (fun s -> (int s, 1L))
    |> List.ofArray

// Count the total number fish in a list
let count fish = List.map snd fish |> List.fold (+) 0L

// Count the number of fish with a given timer
let countTime fish t =
    List.filter (fun (t', _) -> t' = t) fish |> count

// Perform one iteration on a list of fish
let iterate fish =
    List.fold
        (fun acc t ->
            match t with
            | 0, count -> (6, count) :: (8, count) :: acc
            | t, count -> (t - 1, count) :: acc)
        [] fish

// Group fish with the same timer value together
let group fish =
    List.map (fun t -> (t, countTime fish t)) [ 0..8 ]

let rec repeat f n arg =
    if n > 0 then repeat f (n - 1) (f arg)
    else arg

let part1 fish =
    repeat (iterate >> group) 80 fish |> count

let part2 fish =
    repeat (iterate >> group) 256 fish |> count

let run () = Util.run 6 parseInput part1 part2
