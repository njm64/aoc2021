module Aoc.Day4

let boardSize = 5
let boardRange = [ 0 .. boardSize - 1 ]
type board = (int * bool) array array

// Parse a board from a list of lines. A board is represented as array
// of rows, where each row is an array of (number, marked) tuples.
let parseBoard lines =
    let parseLine (line: string) =
        line.Split()
        |> Array.filter (fun s -> s.Length > 0)
        |> Array.map (fun s -> (int s, false))
    List.tail lines
    |> List.map parseLine
    |> Array.ofList

// Parse input, and return a list of numbers and a list of boards
let parseInput lines =
    let firstLine: string = List.head lines
    let numbers =
        firstLine.Split(",")
        |> List.ofArray
        |> List.map int
    let boards =
        List.tail lines
        |> List.chunkBySize (boardSize + 1)
        |> List.map parseBoard
    (numbers, boards)

// Mark board with number n, and return an updated board
let markBoard n board =
    let markCell = (fun cell -> if n = fst cell then (n, true) else cell)
    Array.map (Array.map markCell) board

// Check board for victory conditions
let checkBoard (board: board) =
    let checkRow i = Array.forall snd board[i]
    let checkCol i =
        Array.forall (fun (r: (int * bool) array) -> snd r[i]) board
    List.exists checkRow boardRange
    || List.exists checkCol boardRange

let rec findFirstBingo boards numbers =
    match numbers with
    | [] -> failwith "Bingo not found"
    | n :: tl ->
        let boards = List.map (markBoard n) boards
        match List.tryFind checkBoard boards with
        | Some board -> (board, n)
        | None -> findFirstBingo boards tl

let rec findAllBingos boards numbers =
    match (numbers, boards) with
    | [], _ -> []
    | _, [] -> []
    | n :: tl, boards ->
        let boards = List.map (markBoard n) boards
        let winning, remaining = List.partition checkBoard boards
        let r = List.map (fun b -> (b, n)) winning
        List.append r (findAllBingos remaining tl)

let findLastBingo boards numbers =
    findAllBingos boards numbers |> List.last

let calcScore board =
    let scoreForCell (n, marked) = if marked then 0 else n
    Array.toList board
    |> Array.concat
    |> Array.map scoreForCell
    |> Array.reduce (+)

let part1 (numbers, boards) =
    let board, num = findFirstBingo boards numbers
    calcScore board * num

let part2 (numbers, boards) =
    let board, num = findLastBingo boards numbers
    calcScore board * num

let run () = Util.run 4 parseInput part1 part2
