open System.IO
open System.Collections.Generic

let enumArray2D arr =
    seq {
        for i in 0..(Array2D.length1 arr - 1) do
            for j in 0..(Array2D.length2 arr - 1) do
                yield i, j, arr[i, j]
    }

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
    let dirs = [ 1, 0; -1, 0; 0, 1; 0, -1]
    let q = Queue([startRow, startCol, 0])
    while q.Count > 0 do
        let proRow, proCol, stepCount = q.Dequeue()
        for dRow, dCol in dirs do
            let stepRow, stepCol = proRow + dRow, proCol + dCol
            match maze[stepRow][stepCol], paths[stepRow, stepCol] with
            | '.', None ->
                paths[stepRow, stepCol] <- Some stepCount
                q.Enqueue(stepRow, stepCol, stepCount + 1)
            | _ -> ()

    // Find cheats.
    let inBounds (row, col) = row >= 0 && row < rows && col >= 0 && col < cols
    let cheatDirs = dirs |> List.map (fun (i, j) -> i * 2, j * 2)
    paths
    |> enumArray2D
    |> Seq.choose (fun (row, col, square) ->
        match square with
        | None -> None
        | Some start -> Some (row, col, start))
    |> Seq.map (fun (startRow, startCol, cheatStart) ->
        cheatDirs
        |> Seq.map (fun (dRow, dCol) -> startRow + dRow, startCol + dCol)
        |> Seq.filter inBounds
        |> Seq.choose (fun (row, col) -> paths[row, col])
        |> Seq.map (fun cheatEnd -> cheatEnd - cheatStart)
        |> Seq.filter (fun cheatSavings -> cheatSavings > 1))
    |> Seq.collect id
    |> Seq.groupBy id
    |> Seq.map (fun (n, nlist) -> (n, Seq.length nlist))
    |> Seq.sort
    |> Seq.toList


[<EntryPoint>]
let main argv =
    File.ReadAllLines argv[0]
    |> race
    |> printf "%A"
    0