open System.IO

let input = File.ReadAllLines "input.txt"

let arrayFromInput (input: string array) =
    input
    |> Array.map (fun s -> s |> Seq.map (fun c -> int (c - '0')))
    |>  array2D 


let array2DtoSeq arr = seq {
    for i in 0..Array2D.length1 arr - 1 do
        for j in 0..Array2D.length2 arr - 1 do
            yield Array2D.get arr i j
}

let enumerateNeighbors arr2d i j =
    let inBounds (i, j) =
        i >= 0 && i < (Array2D.length1 arr2d) && j >= 0 && j < (Array2D.length2 arr2d)
    [(i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]
    |> List.filter inBounds


let part1 (input: string array) =
    let r2 = arrayFromInput input
    let trails = r2 |> Array2D.mapi (fun i j n ->
        if n = 0 then
            Set.ofList [(i, j)]  // Start a trail.
        else
            Set.empty)

    let stepTrails trails step =
        r2 |> Array2D.mapi (fun i j n ->
            if n = step then
                enumerateNeighbors trails i j
                |> List.fold (fun aset (i, j) -> Set.union aset (Array2D.get trails i j)) Set.empty
            else
                Set.empty)

    let trails =
        { 1..9 }
        |> Seq.fold stepTrails trails

    trails |> array2DtoSeq |> Seq.sumBy Set.count

printfn "part1: %d" (part1 input)


let part2 (input: string array) =
    let r2 = arrayFromInput input
    let trails = r2 |> Array2D.map (fun n ->
        if n = 0 then
            1
        else
            0)

    let stepTrails trails step =
        r2 |> Array2D.mapi (fun i j n ->
            if n = step then
                enumerateNeighbors trails i j
                |> List.fold (fun count (i, j) -> count + (Array2D.get trails i j)) 0
            else
                0)

    let trails =
        { 1..9 }
        |> Seq.fold stepTrails trails

    trails |> array2DtoSeq |> Seq.sum

printfn "part2: %d" (part2 input)
