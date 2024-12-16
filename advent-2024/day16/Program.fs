open System.IO

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
    let rec explore (grid: char[,]) (routes: Map<int, Position>, visited: Set<Position>) =
        let minScore, minPos = Map.minKeyValue routes
        let routes = Map.remove minScore routes
        match grid[minPos.Row, minPos.Col] with
        | 'E' -> minScore
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
            |> List.filter (fun (score, pos) -> not (Set.contains pos visited))
            |> List.fold (fun (routes, visited) (score, pos) ->
                Map.add (score + minScore) pos routes, Set.add pos visited
                ) (routes, visited)
            |> explore grid
        | '#' -> explore grid (routes, visited)
        | c -> failwithf "Bad grid char %c" c
    explore grid (Map [0, start], Set [start])


[<EntryPoint>]
let main argv =
    printfn "part1: %d" (parseInput argv[0] |> findMinimalRoute)
    0