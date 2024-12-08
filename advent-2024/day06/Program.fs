open System.IO

let input = File.ReadAllLines("input.txt")

// Find the starting position
let findStartPosition input =
    input
    |> Array.mapi (fun row (line: string) ->
        match line.IndexOf('^') with
        | i when i >= 0 -> Some (row, i)
        | _ -> None)
    |> Array.tryPick id
    |> Option.get

let startPos = findStartPosition input

let rows = input.Length
let cols = input[0].Length

type WalkResult =
    | Escaped
    | InfiniteLoop

// Constants for footprints
let pathMask = uint8 0b01100000

let turnRight (step: int * int * char) =
    match step with
    | (-1, 0, 'a') -> (0, 1, 'b')
    | (0, 1, 'b') -> (1, 0, 'd')
    | (1, 0, 'd') -> (0, -1, 'h')
    | (0, -1, 'h') -> (-1, 0, 'a')
    | _ -> failwith "Invalid step"

// Walk the maze
let walk (lines: char array array) =
    let rec loop (step: int * int * char) (row, col) =
        let (i, j, c) = step

        // Calculate the new value for this space.
        let footPrint = lines[row][col]
        let footPrintByte = uint8 footPrint
        let newFootPrint =
            if pathMask &&& footPrintByte = pathMask then
                char (footPrintByte ||| (uint8 c))  // Merge with footprint
            else
                c  // Replace space with footprint

        lines[row][col] <- newFootPrint
        let nextRow, nextCol = row + i, col + j
        if footPrint = newFootPrint then
            InfiniteLoop
        elif nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols then
            Escaped
        elif lines[nextRow][nextCol] = '#' then
            // Turn right and continue
            loop (turnRight step) (row, col)
        else
            // Move to the next cell
            loop step (nextRow, nextCol)

    loop (-1, 0, 'a') startPos

// Part 1: Count path footprints
let part1 =
    let lines = input |> Array.map (fun line -> line.ToCharArray())
    walk lines |> ignore

    let isPathChar (c: char) = pathMask = ((uint8 c) &&& pathMask)
    let count =
        lines
        |> Array.sumBy (fun row -> row |> Array.filter isPathChar |> Array.length)

    printfn "part1 %d" count

// Part 2: Check all possible blockages
let part2 =
    let isInfiniteLoop row col =
        let lines = input |> Array.map (fun line -> line.ToCharArray())
        lines[row][col] <- '#'
        walk lines = InfiniteLoop

    let count =
        [ for row in 0 .. rows - 1 do
            for col in 0 .. cols - 1 do
                if isInfiniteLoop row col then 1 else 0 ]
        |> List.sum

    printfn "part2 %d" count
