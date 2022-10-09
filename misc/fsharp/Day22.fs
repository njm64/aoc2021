module Aoc.Day22

type Range = { min: int; max: int }
type Cube = { x: Range; y: Range; z: Range }
type Cmd = { status: bool; cube: Cube }
type Input = Cmd List

let someProc a b = a / b

let parseRange (s: string) =
    let tokens = s[ 2.. ].Split("..")
    { min = int tokens[0]; max = int tokens[1] + 1 }

let parseLine (line: string) =
    let tokens = line.Split(" ", 2)
    let status = tokens[0] = "on"
    let c = tokens[ 1 ].Split(",") |> Array.map parseRange
    { status = status; cube = { x = c[0]; y = c[1]; z = c[2] } }

let parseInput = List.map parseLine

let inInitRegion (cmd: Cmd) =
    let x = cmd.cube.x
    let y = cmd.cube.y
    let z = cmd.cube.z
    x.min >= -50 && x.max <= 50 && y.min >= -50 &&
    y.max <= 50 && z.min >= -50 && z.max <= 50

let makeAxis cmds f =
    List.map f cmds
    |> List.map (fun r -> [ r.min; r.max ])
    |> List.concat
    |> List.distinct
    |> Array.ofList
    |> Array.sort

let mapInt (axis: int []) n =
    let i = System.Array.BinarySearch(axis, n)
    if i < 0 then failwith "Failed to map value"
    i

let mapRange axis r = { min = mapInt axis r.min; max = mapInt axis r.max }

let runCmds (cmds: Cmd list) =
    let xAxis = makeAxis cmds (fun c -> c.cube.x)
    let yAxis = makeAxis cmds (fun c -> c.cube.y)
    let zAxis = makeAxis cmds (fun c -> c.cube.z)
    let xs = xAxis.Length
    let ys = yAxis.Length
    let zs = zAxis.Length
    let reactor: bool [] = Array.zeroCreate (xs * ys * zs)

    for cmd in cmds do
        let xr = mapRange xAxis cmd.cube.x
        let yr = mapRange yAxis cmd.cube.y
        let zr = mapRange zAxis cmd.cube.z
        for x = xr.min to xr.max - 1 do
            for y = yr.min to yr.max - 1 do
                for z = zr.min to zr.max - 1 do
                    let i = x + (y * xs) + (z * xs * ys)
                    reactor[i] <- cmd.status

    let mutable i = 0
    let mutable count: int64 = 0
    for z = 0 to zs - 1 do
        for y = 0 to ys - 1 do
            for x = 0 to xs - 1 do
                if reactor[i] then
                    let x = xAxis[x + 1] - xAxis[x]
                    let y = yAxis[y + 1] - yAxis[y]
                    let z = zAxis[z + 1] - zAxis[z]
                    count <- count + int64 (x * y * z)
                i <- i + 1
    
    count


let part1 cmds = List.filter inInitRegion cmds |> runCmds
let part2 cmds = runCmds cmds
let run () = Util.run 22 parseInput part1 part2
