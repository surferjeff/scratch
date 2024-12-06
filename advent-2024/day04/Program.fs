open System.IO
open System.Text.RegularExpressions

let xmasRx = Regex("XMAS")
let samxRx = Regex("SAMX")

let countXmas (lines: seq<seq<char>>) =
    lines |> Seq.sumBy (fun line -> 
        let s = System.String.Concat(line)
        (xmasRx.Count s) + (samxRx.Count s))

let traverseDiagonals (matrix: string array) =
    let rows = matrix.Length
    let cols = matrix[0].Length

    seq {
        // Traverse diagonals starting from the first column
        for colStart in 0 .. cols - 1 do
            let mutable i = 0
            let mutable j = colStart
            yield seq {
                while i < rows && j < cols do
                    yield matrix[i][j]
                    i <- i + 1
                    j <- j + 1
            }

        // Traverse diagonals starting from the second row
        for rowStart in 1 .. rows - 1 do
            let mutable i = rowStart
            let mutable j = 0
            yield seq {
                while i < rows && j < cols do
                    yield matrix[i][j]
                    i <- i + 1
                    j <- j + 1
            }
    }

let part1 =
    let lines = File.ReadAllLines("input.txt")

    // Horizontally
    let horizontal = lines |> Seq.map (fun s -> s :> seq<char>) |> countXmas

    // Vertically
    let rows = lines.Length
    let cols = lines[0].Length
    let columns = seq {
        for col in 0..cols-1 do
            yield seq { 
                for row in 0..rows-1 do 
                    yield lines[row][col]
            }
    }
    let vertical = columns |> countXmas

    // Diagonally down right.
    let diag1 = traverseDiagonals lines |> countXmas

    // Diagonally up right.
    let reversed = lines |> Seq.rev |> Seq.toArray
    let diag2 = traverseDiagonals reversed |> countXmas


    vertical + horizontal + diag1 + diag2


printfn "part1: %d" part1


let part2 =
    let lines = File.ReadAllLines("input.txt")
    let rows = lines.Length
    let cols = lines[0].Length
    
    let mutable count = 0
    for row in 1..rows-2 do
        for col in 1..cols-2 do
            let a = System.String.Concat(
                lines[row-1][col-1],
                lines[row][col],
                lines[row+1][col+1])
            let b = System.String.Concat(
                lines[row-1][col+1],
                lines[row][col],
                lines[row+1][col-1])
            if (a = "MAS" || a = "SAM") && (b = "MAS" || b = "SAM") then
                count <- 1 + count
    count

printfn "part2: %d" part2
