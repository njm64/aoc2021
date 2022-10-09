module Aoc.Day5

open System

let mapSize = 1000

let parseCmd (s: string) =
    match s.Replace(" -> ", ",").Split(",") |> Array.map int with
    | [| x1; y1; x2; y2 |] -> (x1, y1, x2, y2)
    | _ -> failwith "Invalid input"

let parseInput lines = List.map parseCmd lines

let drawPoint (m: int array) x y =
    let i = (y * mapSize) + x in m[i] <- m[i] + 1

let drawHorizontal m y x1 x2 =
    for x = min x1 x2 to max x1 x2 do
        drawPoint m x y

let drawVertical m x y1 y2 =
    for y = min y1 y2 to max y1 y2 do
        drawPoint m x y

let drawDiagonal m x1 y1 x2 y2 =
    let (dx: int) = x2 - x1
    let (dy: int) = y2 - y1
    let count = Math.Abs(dx)
    if Math.Abs(dx) <> Math.Abs(dy) then
        failwith "Line is not diagonal"
    let xs = if dx < 0 then -1 else 1
    let ys = if dy < 0 then -1 else 1
    for i = 0 to count do
        drawPoint m (x1 + (xs * i)) (y1 + (ys * i))

let drawLine1 m (x1, y1, x2, y2) =
    if y1 = y2 then
        drawHorizontal m y1 x1 x2
    else if x1 = x2 then
        drawVertical m x1 y1 y2

let drawLine2 m (x1, y1, x2, y2) =
    if y1 = y2 then
        drawHorizontal m y1 x1 x2
    else if x1 = x2 then
        drawVertical m x1 y1 y2
    else
        drawDiagonal m x1 y1 x2 y2

let part1 cmds =
    let m = Array.create (mapSize * mapSize) 0
    List.iter (drawLine1 m) cmds
    Array.filter (fun n -> n >= 2) m |> Array.length

let part2 cmds =
    let m = Array.create (mapSize * mapSize) 0
    List.iter (drawLine2 m) cmds
    Array.filter (fun n -> n >= 2) m |> Array.length

let run () = Util.run 5 parseInput part1 part2
