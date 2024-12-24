open System.IO

let addLink (sets: Map<string, int>) compA compB setNo =
    printfn "%A" sets
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
    File.ReadAllLines argv[0]
    |> Array.map (fun (line: string) -> line.Split('-'))
    |> Array.fold (fun (sets, i) comps ->
        addLink sets comps[0] comps[1] i, i + 1 ) (Map.empty, 0)
    |> fst
    |> Map.iter (fun comp set -> printfn "%s %d" comp set)
    0