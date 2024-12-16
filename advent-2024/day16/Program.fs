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


// Maps route scores to the positions that have those scores.
type Routes = Map<int, Position list>

let addRoute score pos (routes: Routes): Routes =
    Map.change score (function
        | Some positions -> Some (pos :: positions)
        | None -> Some [pos]
    ) routes

let popRoute (routes: Routes) =
    let minScore, posList = Map.minKeyValue routes
    match posList with
        | [pos] -> (Map.remove minScore routes), minScore, pos
        | pos :: rest -> Map.add minScore rest routes, minScore, pos
        | bad -> failwith "Empty list found in routes"

let findMinimalRoute (grid: char[,], start: Position) =
    Seq.unfold (fun (routes: Map<int, Position list>, visited: Set<Position>) ->
        let routes, minScore, minPos = popRoute routes
        if Set.contains minPos visited then
            Some (None, (routes, visited))
        else
            let visited = Set.add minPos visited
            match grid[minPos.Row, minPos.Col] with
            | 'E' -> Some (Some minScore, (routes, visited))
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
                    addRoute (score + minScore) pos routes, visited
                    ) (routes, visited)
                |> (fun routesVisited -> Some (None, routesVisited))
            | '#' -> Some (None, (routes, visited))
            | c -> failwithf "Bad grid char %c" c) (Map [0, [start]], Set []) 
    |> Seq.pick id


[<EntryPoint>]
let main argv =
    printfn "part1: %d" (parseInput argv[0] |> findMinimalRoute)
    0