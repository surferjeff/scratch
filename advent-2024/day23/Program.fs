open System
open System.IO

type Graph = Map<string, Set<string>>

// Function to find triangles in an undirected graph using hash-based edge iterator
let findTriangles (graph: Graph) =
    let triangles = ResizeArray<(string * string * string)>()

    // Iterate through each node in the graph
    for u in graph.Keys do
        // Get neighbors of 'u'
        let neighborsU = graph.[u]

        for v in neighborsU do
            // Ensure u < v to avoid duplicate pairs
            if String.Compare(u, v) < 0 then
                let neighborsV = graph.[v]

                // Find the intersection of neighbors of 'u' and 'v'
                let commonNeighbors = Set.intersect neighborsU neighborsV

                for w in commonNeighbors do
                    // Ensure v < w to avoid duplicate triangles
                    if String.Compare(v, w) < 0 then
                        triangles.Add((u, v, w))

    // Return the list of triangles
    triangles

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
    |> Seq.iter (printfn "%A")
    0