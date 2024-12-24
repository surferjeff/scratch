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


let addEdge2 (sets: Map<string, int>) compA compB setNo =
    match Map.tryFind compA sets, Map.tryFind compB sets with
    | None, None -> sets |> Map.add compA setNo |> Map.add compB setNo
    | Some(n), None -> sets |> Map.add compB n
    | None, Some(n) -> sets |> Map.add compA n
    | Some(n), Some(m) when n = m -> sets
    | Some(n), Some(m) ->
        // Merge the two sets
        sets
        |> Map.toList
        |> List.choose (fun (key, valu) -> if valu = n then Some(key) else None)
        |> List.fold (fun sets comp -> Map.add comp m sets) sets

[<EntryPoint>]
let main argv =
    let edges = 
        File.ReadAllLines argv[0]
        |> Array.map (fun (line: string) -> line.Split('-'))

    edges
    |> Array.fold addEdge Map.empty
    |> findTriangles
    |> Seq.filter (List.exists (fun node -> node.StartsWith('t')))
    |> Seq.length
    |> printfn "part1: %d"

    let sets =
        edges
        |> Array.fold (fun (sets, i) comps ->
            addEdge2 sets comps[0] comps[1] i, i + 1 ) (Map.empty, 0)
        |> fst
    let largestGroupId =
        sets
        |> Map.values
        |> Seq.groupBy id
        |> Seq.map (fun (setId, setIds) -> setId, Seq.length setIds )
        |> Seq.maxBy (fun (groupId, count) -> count)
        |> fst
    sets
    |> Map.toList
    |> List.filter (fun (comp, group) -> group = largestGroupId)
    |> List.map fst
    |> List.sort
    |> String.concat ","
    |> printfn "%s"

    0