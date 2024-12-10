open System.IO

let input = File.ReadAllLines "input.txt"

let arrayFromInput (input: string array) =
    input
    |> Array.map (fun s -> s |> Seq.map (fun c -> int (c - '0')))
    |>  array2D 


let part1 (input: string array) =
    let r2 = arrayFromInput input
    let trails = r2 |> Array2D.mapi (fun i j n ->
        if n = 0 then
            Set.ofList [(i, j)]  // Start a trail.
        else
            Set.empty)

    let inBounds (i, j) =
        i >= 0 && i < Array2D.length1 r2 && j >= 0 && j < Array2D.length2 r2

    let stepTrails trails step =
        r2 |> Array2D.mapi (fun i j n ->
            if n = step then
                [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]
                |> List.filter inBounds
                |> List.fold (fun aset (i, j) -> Set.union aset (Array2D.get trails i j)) Set.empty
            else
                Set.empty)

    let trails =
        { 1..9 }
        |> Seq.fold stepTrails trails

    let mutable score = 0
    for i in 0..Array2D.length1 trails - 1 do
        for j in 0..Array2D.length2 trails - 1 do
            score <- score + Set.count (Array2D.get trails i j)
    score 

printfn "part1: %d" (part1 input)


let part2 (input: string array) =
    let r2 = arrayFromInput input
    let trails = r2 |> Array2D.mapi (fun i j n ->
        if n = 0 then
            1
        else
            0)

    let inBounds (i, j) =
        i >= 0 && i < Array2D.length1 r2 && j >= 0 && j < Array2D.length2 r2

    let stepTrails trails step =
        r2 |> Array2D.mapi (fun i j n ->
            if n = step then
                [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]
                |> List.filter inBounds
                |> List.fold (fun count (i, j) -> count + (Array2D.get trails i j)) 0
            else
                0)

    let trails =
        { 1..9 }
        |> Seq.fold stepTrails trails

    let mutable score = 0
    for i in 0..Array2D.length1 trails - 1 do
        for j in 0..Array2D.length2 trails - 1 do
            score <- score + Array2D.get trails i j
    score 

printfn "part2: %d" (part2 input)
