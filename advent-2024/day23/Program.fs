open System
open System.IO

type Graph = Map<string, Set<string>>

// Find triangles in an undirected graph using hash-based edge iterator
let findTriangles (graph: Map<string, Set<string>>) =
    graph
    |> Map.toSeq
    |> Seq.collect (fun (u, neighborsU) ->
        neighborsU
        |> Seq.filter (fun v -> String.Compare(u, v) < 0)
        |> Seq.collect (fun v ->
            let neighborsV = graph.[v]
            Set.intersect neighborsU neighborsV
            |> Seq.filter (fun w -> String.Compare(v, w) < 0)
            |> Seq.map (fun w -> [u; v; w])
        )
    )
    |> Seq.toList

let addEdge (graph: Graph) (edge: string array) =
    graph
    |> Map.change edge[0] (function
        | Some edges -> Some (Set.add edge[1] edges)
        | None -> Some (Set.ofList [edge[1]]))
    |> Map.change edge[1] (function
        | Some edges -> Some (Set.add edge[0] edges)
        | None -> Some (Set.ofList [edge[0]]))

[<EntryPoint>]
let main argv =
    File.ReadAllLines argv[0]
    |> Array.map (fun (line: string) -> line.Split('-'))
    |> Array.fold addEdge Map.empty
    |> findTriangles
    |> Seq.filter (List.exists (fun node -> node.StartsWith('t')))
    |> Seq.length
    |> printfn "part1: %d"
    0