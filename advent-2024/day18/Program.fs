﻿open System.IO
open System.Collections.Generic

let findPath (region: char array2d) =
    let dim = Array2D.length1 region - 2
    let q = Queue([1, 1, []])
    let mutable found = []
    while q.Count > 0 do
        let (row, col, tail) = q.Dequeue()
        if row = dim && col = dim then
            found <- (row, col) :: tail
            q.Clear()
        elif region[row, col] = '.' then
            region[row, col] <- ','
            let tail = (row, col) :: tail
            q.Enqueue(row + 1, col, tail)
            q.Enqueue(row - 1, col, tail)
            q.Enqueue(row, col + 1, tail)
            q.Enqueue(row, col - 1, tail)
    found

let part1 inputPath dim take  =
    // Create a region with a boundary of solid #.
    let topBottom = String.replicate (dim + 2) "#"
    let middle = "#" + (String.replicate dim ".") + "#"
    let region =
        {0..dim-1}
        |> Seq.fold (fun lines _ -> middle :: lines) [topBottom]
        |> List.append [topBottom]
        |> array2D

    // Mark the bytes falling into the region.
    let byteDrops =
        File.ReadAllLines inputPath
        |> Array.map (fun line -> line.Split(',') |> Array.map (int >> ((+) 1)))
    byteDrops
        |> Array.take take
        |> Array.iter (fun [|col; row|] -> region[row, col] <- '#')

    let found = findPath region

    // Update the region with the path.
    let displayRegion = Array2D.copy region
    found |> List.iter (fun (row, col) -> displayRegion[row, col] <- 'O')
    displayRegion
        |> Array2D.iteri (fun row col c ->
            match col with
            | col when col - 1 = dim -> printfn "%c" c
            | col -> printf "%c" c
        )
    List.length found - 1 |> printfn "part1: %d"

    byteDrops
    |> Array.skip take
    |> Array.pick (fun [|col; row|] ->
        region |>
            Array2D.iteri (fun irow icol c ->
                if c = ',' then
                    region[irow, icol] <- '.'
            )
        region[row, col] <- '#'
        if findPath region = [] then Some (col - 1, row - 1)
        else None)
    |> printfn "part2: %A"

[<EntryPoint>]
let main argv =
    part1 "sample1.txt" 7 12
    part1 "input.txt" 71 1024

    0
