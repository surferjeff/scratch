open System.IO
open System.Text.RegularExpressions

let enumerateArray2D arr = seq {
    for row in 0..Array2D.length1 arr - 1 do
        for col in 0..Array2D.length2 arr - 1 do
            yield row, col, Array2D.get arr row col
}

let parseInput path =
    let rx = Regex("(?<map>#[.O@#]+#)|(?<move>[<>v^])")
    let gridLines = ResizeArray<string>()
    let moves = ResizeArray<char>()
    for m in File.ReadAllText(path) |> rx.Matches do
        if m.Groups["map"].Success then
            gridLines.Add(m.Groups["map"].Value)
        else
            moves.Add(m.Groups["move"].Value[0])
    array2D gridLines, moves.ToArray()

// Updates grid and returns new robot position.
let applyMove (grid: char array2d) (row: int, col: int) (arrow: char) =
    let rowStep, colStep =
        match arrow with
        | '^' -> -1, 0
        | 'v' -> 1, 0
        | '<' -> 0, -1
        | '>' -> 0, 1
        | _ -> failwithf "Illegal step: %c" arrow
    // Explore how far the robot can push boxes.
    let mutable exploreStack = [row + rowStep, col + colStep]
    let mutable moveStack = [row, col, '@']
    let mutable quit = false
    while not quit do
        match exploreStack with
        | [] -> quit <- true
        | (irow, icol) :: xs ->
            let c = Array2D.get grid irow icol
            let nextRow, nextCol = irow + rowStep, icol + colStep
            match rowStep, c with
            | _, '.' -> exploreStack <- xs
            | _, '#' -> quit <- true
            | _ , 'O'
            | 0, ('[' | ']') ->
                // Simple one row move.
                exploreStack <- (nextRow, nextCol) :: xs
                moveStack <- (irow, icol, c) :: moveStack
            | _, '[' ->
                // Two column move.
                exploreStack <- (nextRow, nextCol) :: (nextRow, nextCol + 1) :: xs
                moveStack <- (irow, icol, '[') :: (irow, icol + 1, ']') :: moveStack
            | _, ']' ->
                // Two column move.
                exploreStack <- (nextRow, nextCol) :: (nextRow, nextCol - 1) :: xs
                moveStack <- (irow, icol, ']') :: (irow, icol - 1, '[') :: moveStack
            | _ -> failwithf "Illegal char in grid: %c" c
    if exploreStack = [] then
        // Move boxes.
        for (irow, icol, c) in List.distinct moveStack do
            Array2D.set grid irow icol '.'
            Array2D.set grid (irow + rowStep) (icol + colStep) c
        row + rowStep, col + colStep
    else
        // No space to push.
        row, col    

let widen grid (robotRow, robotCol) =
    let wideGrid = Array2D.create (
        Array2D.length1 grid) (2 * Array2D.length2 grid) '.'
    for (row, col, c) in enumerateArray2D grid do
        match c with
        | '@' -> Array2D.set wideGrid row (2*col) '@'
        | '#' ->
            Array2D.set wideGrid row (2*col) '#'
            Array2D.set wideGrid row (2*col+1) '#'
        | 'O' ->
            Array2D.set wideGrid row (2*col) '['
            Array2D.set wideGrid row (2*col+1) ']'
        | '.' -> ()
        | c -> failwithf "Illegal char in grid: %c" c
    wideGrid, (robotRow, robotCol * 2)

let sumBoxes grid =
    grid
    |> enumerateArray2D
    |> Seq.sumBy (fun (row, col, c) ->
        match c with
        | 'O' | '[' -> row * 100 + col
        | _ -> 0)

[<EntryPoint>]
let main argv =
    let grid, moves = (parseInput argv[0])
    let start =
        grid
        |> enumerateArray2D
        |> Seq.pick (fun (row, col, c) ->
            if c = '@' then Some (row, col)
            else None
        )
    let wideGrid, wideStart = widen grid start

    let robotPos = moves |> Array.fold (applyMove grid) start
    printfn "part1: %d" (sumBoxes grid)

    let widePos = moves |> Array.fold (applyMove wideGrid) wideStart
    printfn "part2: %d" (sumBoxes wideGrid)

    0
