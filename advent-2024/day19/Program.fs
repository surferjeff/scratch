open System.IO
open System.Collections.Generic
open System

[<Struct>]
type Input = {
    Towels: string array
    Designs: string array
}

let parseInput path =
    let lines = File.ReadAllLines(path)
    {
        Towels = lines[0].Split(",") |> Array.map (fun s -> s.Trim())
        Designs = lines[2..] |> Array.map(fun s -> s.Trim())
    }

// Function to perform binary search and check if the prefix is present
let isPrefixPrefixOfArray (arr: string[]) (prefix: string) =
    let binarySearchResult = Array.BinarySearch(arr, prefix, StringComparer.Ordinal)
    if binarySearchResult >= 0 then
        true
    else
        let index = -binarySearchResult - 1  // Convert negative result to an insertion index
        if index < arr.Length then
            arr[index].StartsWith(prefix, StringComparison.Ordinal)
        else
            false

let countMatches (input: Input) =
    let counts = Dictionary([KeyValuePair("", 1L)])
    let q = PriorityQueue([struct ("", 0)]);
    let isDesignPrefix = input.Designs |> Array.sort |> isPrefixPrefixOfArray
    while q.Count > 0 do
        let pre = q.Dequeue()
        for towel in input.Towels do
            let combo = pre + towel
            if isDesignPrefix combo then
                let preCount = counts[pre]
                let got, count = counts.TryGetValue(combo)
                if got then
                    counts[combo] <- count + preCount
                else
                    counts.Add(combo, preCount)
                    q.Enqueue(combo, combo.Length)
    input.Designs
    |> Array.map (fun design ->
        let mutable count = ref 0L
        counts.TryGetValue(design, count) |> ignore
        count.Value)

[<EntryPoint>]
let main argv =
    let counts = argv[0] |> parseInput |> countMatches
    counts |> Seq.filter (fun n -> n > 0) |> Seq.length |> printfn "part1: %d"
    counts |> Seq.sum |> printfn "part2: %d"
    0