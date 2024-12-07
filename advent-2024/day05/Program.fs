open System.IO

let main =
    let lines = File.ReadAllLines("input.txt")
    // Maps a number to a set of other numbers that must come before.
    let mutable rules: Map<uint, Set<uint>> = Map.empty
    let mutable sum = 0u
    let mutable updates = []
    for line in lines do
        if line.Contains('|') then
            let [|a; b|] = line.Split('|') |> Array.map (fun text -> uint text)
            rules <- rules.Change (b, function
                | Some set -> Some (set.Add a)
                | None -> Some (Set.ofList [a]))

        elif line.Contains(',') then
            let update = line.Split(',') |> Array.map (fun text -> uint text)
            updates <- update :: updates

    // part 1
    let mutable invalidUpdates = []
    for update in updates do            
        let mutable possibles = Set.ofSeq { 10u..99u }
        let mutable valid = true
        for n in update do
            if not valid then
                () // Nothing more todo
            elif possibles.Contains n then
                // This number can work.
                match rules.TryFind n with
                | None -> ()
                | Some precedents -> possibles <- possibles - precedents
            else
                valid <- false
        if valid then
            sum <- sum + update[update.Length / 2]
        else
            invalidUpdates <- update :: invalidUpdates

    printfn "part1 %d" sum

    // part2
