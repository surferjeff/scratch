open System.IO
open System.Collections.Generic

let enumArray2D arr =
    seq {
        for i in 0..(Array2D.length1 arr - 1) do
            for j in 0..(Array2D.length2 arr - 1) do
                yield i, j, arr[i, j]
    }

let renderPath paths =
    for row in 0..Array2D.length1 paths - 1 do
        for col in 0..Array2D.length2 paths - 1 do
            match paths[row, col] with
            | Some N -> printf "%02d " N
            | None -> printf "-- "
        printfn ""

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

    renderPath paths

    // // Find cheats.
    // let inBounds (row, col) = row >= 0 && row < rows && col >= 0 && col < cols
    // let cheatDirs = dirs |> List.map (fun (i, j) -> i * 2, j * 2)
    // let starts = 
    //     paths
    //     |> enumArray2D
    //     |> Seq.choose (fun (row, col, square) ->
    //         match square with
    //         | None -> None
    //         | Some start -> Some (row, col, start))
    //     |> Seq.toList
    // starts
    // |> List.map (fun (startRow, startCol, cheatStart) ->
    //     cheatDirs
    //     |> List.map (fun (dRow, dCol) -> startRow + dRow, startCol + dCol)
    //     |> List.filter inBounds
    //     |> List.choose (fun (row, col) -> paths[row, col])
    //     |> List.map (fun cheatEnd -> cheatEnd - cheatStart)
    //     |> List.filter (fun cheatSavings -> cheatSavings > 1))
    // |> List.collect id
    // |> List.groupBy id
    // |> List.map (fun (n, nlist) -> (n, List.length nlist))
    // |> List.sort


[<EntryPoint>]
let main argv =
    File.ReadAllLines argv[0]
    |> race
    |> printf "%A"
    0