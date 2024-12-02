open System.IO

let part1 = 
    let mutable aList = []
    let mutable bList = []

    File.ReadLines("input.txt")
        |> Seq.mapi (fun i m -> i, m)
        |> Seq.iter (fun (i, line) ->
            let words = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            aList <- int words[0] :: aList
            bList <- int words[1] :: bList
            ()
        )

    aList <- List.sort(aList)
    bList <- List.sort(bList)

    let sumDiff = 
        Seq.zip aList bList
        |> Seq.map (fun ab ->
            let i, j = ab
            abs (i - j))
        |> Seq.reduce (+)

    printfn "total: %d" sumDiff

let part2 = 
    let mutable aList = []
    let mutable bMap = Map []

    File.ReadLines("input.txt")
        |> Seq.iter (fun line ->
            let words = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            aList <- int words[0] :: aList
            let b = int words[1]
            bMap <- bMap |> Map.change b (function
                | Some n -> Some (n + 1)
                | None -> Some 1)
            ()
        )

    let score =
        aList
        |> Seq.map (fun n ->
            match Map.tryFind n bMap with 
            | Some count -> count * n
            | None -> 0)
        |> Seq.reduce (+)
        
    printf "score: %d" score
