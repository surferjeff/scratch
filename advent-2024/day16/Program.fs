open System.IO
open System.Collections.Generic

type Dir = North | South | East | West

[<Struct>]
type Position = {
    Row: int
    Col: int
    Facing: Dir
}

let parseInput path =
    let grid = File.ReadAllLines(path) |> array2D
    // Expect to find the Start in the bottom right corner.
    let start = {
        Row = Array2D.length1 grid - 2
        Col = 1
        Facing = East
    }
    assert (grid[start.Row, start.Col] = 'S')
    grid, start

let findMinimalRoute (grid: char[,], start: Position) =
    let routes = PriorityQueue<int * Position,int>()
    routes.Enqueue((0, start), 0)
    let visited = HashSet<Position>()
    let mutable result = None
    while result.IsNone do
        let minScore, minPos = routes.Dequeue()
        if visited.Add minPos then
            match grid[minPos.Row, minPos.Col] with
            | 'E' -> result <- Some minScore
            | '.' | 'S' ->
                [
                    1, match minPos.Facing with
                        | North -> { minPos with Row = minPos.Row - 1 }
                        | South -> { minPos with Row = minPos.Row + 1 }
                        | East -> { minPos with Col = minPos.Col + 1 }
                        | West -> { minPos with Col = minPos.Col - 1 }
                    1000, { minPos with Facing = North}
                    1000, { minPos with Facing = East}
                    1000, { minPos with Facing = West}
                    1000, { minPos with Facing = South}
                ]
                |> List.filter (fun (_, pos) -> not (visited.Contains pos))
                |> List.iter (fun (score, pos) ->
                    let score = minScore + score
                    routes.Enqueue((score, pos), score))
            | '#' -> ()
            | c -> failwithf "Bad grid char %c" c
    result.Value

[<EntryPoint>]
let main argv =
    printfn "part1: %d" (parseInput argv[0] |> findMinimalRoute)
    0