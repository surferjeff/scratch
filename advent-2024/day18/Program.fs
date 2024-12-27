open System.IO
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

let solve inputPath dim take  =
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

    List.length (findPath region) - 1 |> printfn "part1: %d"

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
    solve "sample1.txt" 7 12
    solve "input.txt" 71 1024
    0
