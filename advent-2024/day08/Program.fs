open System.IO
open System

let input = File.ReadAllLines("input.txt")
let rows = input.Length
let cols = input[0].Length

let inRows n = n >= 0 && n < rows
let inCols n = n >= 0 && n < cols
let inBounds (row, col) = inRows row && inCols col

let map =
    let charMap = List.choose id [
        for (row, line) in Array.indexed input do
            for (col, c) in Seq.indexed line do
                if c = '.' then None else Some (c, row, col)
    ]

    // Transform charMap into a Map<char, (row * col) array>
    charMap
    |> Seq.groupBy (fun (c, row, col) -> c) // Group by the character
    |> Seq.map (fun (c, values) -> 
        values |> Seq.map (fun (_, row, col) -> (row, col)) |> Seq.toArray)

let countAntinodes antinodesFrom =
    let antinodes = map |> Seq.map (fun positions ->
        let pairs = seq {
            for i in 0..positions.Length-1 do
                for j in i+1..positions.Length-1 do
                    yield (positions[i], positions[j])
        }
        pairs
        |> Seq.map antinodesFrom
        |> Seq.collect id // flatten
    )
    let antinodes = Seq.collect id antinodes // flatten
    antinodes |> Seq.sort |> Seq.distinct |> Seq.length

let antinodesFrom1 pair =
    let ((row1, col1), (row2, col2)) = pair
    let rowDelta = row1 - row2
    let colDelta = col1 - col2
    [(row1 + rowDelta, col1 + colDelta); (row2 - rowDelta, col2 - colDelta )]
    |> List.filter inBounds 

printfn "part1: %d" (countAntinodes antinodesFrom1)

let antinodesFrom2 pair =
    let rec generateNodes (current, delta, acc) =
        let next = (fst current + fst delta, snd current + snd delta)
        if inBounds next then
            generateNodes (next, delta, next :: acc)
        else
            acc

    let ((row1, col1), (row2, col2)) = pair
    let rowDelta, colDelta = (row1 - row2, col1 - col2)
    
    let antinodes = generateNodes ((row1, col1), (rowDelta, colDelta), [(row1, col1)])
    generateNodes ((row2, col2), (-rowDelta, -colDelta), (row2, col2) :: antinodes)
    
printfn "part2: %d" (countAntinodes antinodesFrom2)
