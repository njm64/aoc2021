module Aoc.Day2

let parseCmd (s: string) =
    let tokens = s.Split()
    (tokens[0], int tokens[1])

let parseInput lines = List.map parseCmd lines

let applyCmd (pos, depth) =
    function
    | "forward", x -> (pos + x, depth)
    | "down", x -> (pos, depth + x)
    | "up", x -> (pos, depth - x)
    | cmd, _ -> failwith ("Unknown command " + cmd)

let applyCmd2 (pos, depth, aim) =
    function
    | "forward", x -> (pos + x, depth + (aim * x), aim)
    | "down", x -> (pos, depth, aim + x)
    | "up", x -> (pos, depth, aim - x)
    | cmd, _ -> failwith ("Unknown command " + cmd)

let part1 cmds =
    let pos, depth = List.fold applyCmd (0, 0) cmds
    pos * depth

let part2 cmds =
    let pos, depth, _ = List.fold applyCmd2 (0, 0, 0) cmds
    pos * depth

let run () = Util.run 2 parseInput part1 part2
