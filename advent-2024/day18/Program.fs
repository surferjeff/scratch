
[<EntryPoint>]
let main argv =
    let dim = 6
    let topBottom = String.replicate (dim + 3) "#"
    let middle = "#" + (String.replicate (dim + 1) ".") + "#"
    let region =
        {0..dim}
        |> Seq.fold (fun lines _ -> middle :: lines) [topBottom]
        |> List.append [topBottom]
        |> List.toArray
        |> Array.iter (printfn "%s")
    0