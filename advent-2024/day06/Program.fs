open System.IO

let input = File.ReadAllLines("input.txt")

// Find the starting position
let mutable startPos = (-1, -1)
for row in 0..input.Length-1 do
    if (-1, -1) = startPos then
        let line = input[row]
        let i = line.IndexOf('^')
        if i >= 0 then
            startPos <- (row, i)

let rows = input.Length
let cols = input[0].Length

type WalkResult =
| Escaped
| InfiniteLoop

// Trace the path, leaving footprints of a, b, d, and h in lines.
// The least significant 4 bits of a, b, d, and h are distinct.
// a: 0001
// b: 0010
// d: 0100
// h: 1000
// The most significant 4 bits are all 0110.
let pathMask = uint8 0b01100000

let walk (lines: char array array) =
    let mutable step = (-1, 0, 'a')
    let mutable result = None
    let mutable (row, col) = startPos
    while Option.isNone result do
        let (i, j, c) = step

        // Calculate the new value for this space.
        let footPrint = lines[row][col]
        let footPrintByte = uint8 footPrint
        let newFootPrint =
            if pathMask &&& footPrintByte = pathMask then
                char (footPrintByte ||| (uint8 c))  // Merge with footprint
            else
                c  // Replace space with footprint.
        if footPrint = newFootPrint then
            result <- Some InfiniteLoop
        else
            lines[row][col] <- newFootPrint

            // Examine the next space.
            let (nextRow, nextCol) = (row + i, col + j)
            if nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols then
                result <- Some Escaped
            else if lines[nextRow][nextCol] = '#' then
                // Turn right 90 degrees.
                step <-
                    match step with
                    | (-1, 0, 'a') -> (0, 1, 'b')
                    | (0, 1, 'b') -> (1, 0, 'd')
                    | (1, 0, 'd') -> (0, -1, 'h')
                    | (0, -1, 'h') -> (-1, 0, 'a')
                    | bad -> failwith "Bad step"
            else
                // Leave a footprint in the next space.
                row <- nextRow
                col <- nextCol
    Option.get result

let part1 =
    let lines = input |> Array.map (fun line -> line.ToCharArray())
    walk lines |> ignore

    // Count path.
    let isPathChar = fun (c: char) -> pathMask = ((uint8 c) &&& pathMask)
    let count =
        lines |> Array.sumBy (fun row -> 
            Array.filter isPathChar row |> Array.length)

    printfn "part1 %d" count

let part2 =
    let mutable count = 0
    for row in 0..rows-1 do
        for col in 0..cols-1 do
            let lines = input |> Array.map (fun line -> line.ToCharArray())
            lines[row][col] <- '#'
            if InfiniteLoop = walk lines then
                count <- count + 1
    
    printfn "part2 %d" count


