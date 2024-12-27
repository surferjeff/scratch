﻿open System.IO
open System.Collections.Generic


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
    File.ReadAllLines inputPath
        |> Seq.take take
        |> Seq.map (fun line -> line.Split(',') |> Array.map int)
        |> Seq.iter (fun [|col; row|] -> region[row + 1, col + 1] <- '#')

    // Find the path with bread-first search.
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

    // Update the region with the path.
    found |> List.iter (fun (row, col) -> region[row, col] <- 'O')
    region
        |> Array2D.iteri (fun row col c ->
            match col with
            | col when col - 1 = dim -> printfn "%c" c
            | col -> printf "%c" c
        )
    List.length found - 1 |> printfn "%d"


[<EntryPoint>]
let main argv =
    part1 "sample1.txt" 7 12
    part1 "input.txt" 71 1024

    0
