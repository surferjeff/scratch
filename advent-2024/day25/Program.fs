open System.IO

let parseBlock (lines: string array) (startIndex: int) =
    let heights = Array.create 5 0
    for i in startIndex+1..startIndex + 5 do
        lines[i] |> String.iteri (fun j c ->
            if c = '#' then heights[j] <- heights[j] + 1)
    heights

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv[0]
    let rec loop (i: int) (keys: int array list) (locks: int array list) =
        if i >= lines.Length then
            keys, locks
        else
            match lines[i] with
            | "#####" -> loop (i + 7) keys  (parseBlock lines i :: locks)
            | "....." -> loop (i + 7) (parseBlock lines i :: keys) locks
            | "" -> loop (i + 1) keys locks
            | bad -> failwithf "Unexpected line %s" bad
    let keys, locks = loop 0 [] []
    List.allPairs keys locks
    |> List.filter (fun (key,lock) ->
        {0..4} |> Seq.forall (fun i -> key[i] + lock[i] < 6))
    |> List.length
    |> printfn "%A"
    0