open System.IO
open System.Collections.Generic

let enumArray2D arr =
    seq {
        for i in 0..(Array2D.length1 arr - 1) do
            for j in 0..(Array2D.length2 arr - 1) do
                yield i, j, arr[i, j]
    }

let dirs = [ 1, 0; -1, 0; 0, 1; 0, -1]

let findCheatsStartingFrom (maxCheatCount: int) (paths: Option<int>[,])
    (startRow, startCol, cheatStart) =
    let inBounds (row, col) = (
        row >= 0 && row < Array2D.length1 paths
        && col >= 0 && col < Array2D.length2 paths)
    let visited = HashSet([startRow, startCol])
    let q = Queue([startRow, startCol, 0])
    let mutable cheats = []
    while q.Count > 0 do
        let cheatCol, cheatRow, cheatCount = q.Dequeue()
        dirs
        |> List.map (fun (dRow, dCol) -> cheatRow + dRow, cheatCol + dCol)
        |> List.filter inBounds
        |> List.iter (fun (row, col) ->
            if visited.Add(row, col) then
                match paths[row,col] with
                | Some n ->
                    // It's another square on the path.  Is it the terminus of
                    // a shortcut?
                    let savings = n - cheatStart - cheatCount
                    if savings > 1 then
                        cheats <- savings :: cheats
                | None ->
                    // It's a barrier.  Continue cheating.
                    if 1 + cheatCount < maxCheatCount then
                        q.Enqueue(row, col, 1 + cheatCount))
    cheats

let race (maze: string array) =
    // Find the start position.
    let mutable startRow, startCol = -1, -1
    for iRow in 0..maze.Length-1 do
        let iCol = maze[iRow].IndexOf('S')
        if iCol >= 0 then
            startRow <- iRow
            startCol <- iCol

    // Create a 2d array to store the path.
    let rows = maze.Length
    let cols = maze[0].Length
    let paths = Array2D.create rows cols None
    paths[startRow, startCol] <- Some(0)

    // Explore, leaving shortest path to each square in paths.
    let q = Queue([startRow, startCol, 1])
    while q.Count > 0 do
        let proRow, proCol, stepCount = q.Dequeue()
        for dRow, dCol in dirs do
            let stepRow, stepCol = proRow + dRow, proCol + dCol
            match maze[stepRow][stepCol], paths[stepRow, stepCol] with
            | ('.' | 'E'), None ->
                paths[stepRow, stepCol] <- Some stepCount
                q.Enqueue(stepRow, stepCol, stepCount + 1)
            | _ -> ()

    // Find cheats.
    paths
    |> enumArray2D
    |> Seq.choose (fun (row, col, square) ->
        match square with
        | None -> None
        | Some start -> Some (row, col, start))
    |> Seq.toList
    |> List.map (findCheatsStartingFrom 2 paths)
    |> List.collect id
    |> List.groupBy id
    |> List.map (fun (n, nlist) -> (n, List.length nlist))
    |> List.sort


[<EntryPoint>]
let main argv =
    let cheats = argv[0] |> File.ReadAllLines |> race
    printfn "%A" cheats
    cheats
    |> List.map (fun (savings, count) ->
        if savings >= 100 then
            count
        else
            0)
    |> List.sum
    |> printfn "part1: %d"
    0
    