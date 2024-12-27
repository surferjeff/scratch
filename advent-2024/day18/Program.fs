open System.IO

[<EntryPoint>]
let main argv =
    // Create a region with a boundary of solid #.
    let dim = 7
    let topBottom = String.replicate (dim + 2) "#"
    let middle = "#" + (String.replicate dim ".") + "#"
    let region =
        {0..dim-1}
        |> Seq.fold (fun lines _ -> middle :: lines) [topBottom]
        |> List.append [topBottom]
        |> array2D
    // Mark the bytes falling into the region.
    File.ReadAllLines argv[0]
        |> Seq.take 12
        |> Seq.map (fun line -> line.Split(',') |> Array.map int)
        |> Seq.iter (fun [|col; row|] -> region[row + 1, col + 1] <- '#')
    region
        |> Array2D.iteri (fun row col c ->
            match col with
            | col when col - 1 = dim -> printfn "%c" c
            | col -> printf "%c" c
        )
    0
