open System
open System.IO

type Graph = Map<string, Set<string>>

// Find triangles in an undirected graph using hash-based edge iterator
let findTriangles (graph: Map<string, Set<string>>) =
    graph
    |> Map.toSeq
    |> Seq.collect (fun (u, neighborsU) ->
        neighborsU
        |> Seq.filter (fun v -> String.CompareOrdinal(u, v) < 0)
        |> Seq.collect (fun v ->
            let neighborsV = graph.[v]
            Set.intersect neighborsU neighborsV
            |> Seq.filter (fun w -> String.CompareOrdinal(v, w) < 0)
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


// Find the largest clique (fully connected set of nodes) in an undirected graph
let largestClique (graph: Graph) =
    let rec expandClique clique candidates =
        match candidates with
        | [] -> clique
        | candidate :: rest ->
            let newClique = candidate :: clique
            let newCandidates =
                rest
                |> List.filter (fun n ->
                    newClique |> List.forall (fun c -> graph.[c].Contains n))
            expandClique newClique newCandidates

    graph.Keys
    |> Seq.toList
    |> List.map (fun node ->
        let neighbors = graph.[node] |> Set.toList
        expandClique [node] neighbors)
    |> List.maxBy List.length

[<EntryPoint>]
let main argv =
    let graph = 
        File.ReadAllLines argv[0]
        |> Array.map (fun (line: string) -> line.Split('-'))
        |> Array.fold addEdge Map.empty
    graph
    |> findTriangles
    |> Seq.filter (List.exists (fun node -> node.StartsWith('t')))
    |> Seq.length
    |> printfn "part1: %d"

    graph
    |> largestClique
    |> List.sort
    |> String.concat ","
    |> printfn "part2: %s"

    0