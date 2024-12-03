open System.IO

let isSafeStep increasing a b =
    match increasing, b - a with
    | true, (1 | 2 | 3) -> true
    | false, (-1 | -2 | -3) -> true
    | _ -> false

let numbersAreSafe numbers =
    let mutable increasing = None
    let mutable a = None
    let mutable isSafe = true
    for b in numbers do
        match isSafe, a with
        | false, _ -> ()
        | _, None -> a <- Some b
        | _, Some av ->
            let incr = increasing |> Option.defaultValue (av < b)
            if isSafeStep incr av b then
                increasing <- Some incr
                a <- Some b
            else
                isSafe <- false
    isSafe

let part1 =
    File.ReadLines "input.txt"
    |> Seq.filter (fun line -> 
        let words = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        numbersAreSafe (Seq.map int words))
    |> Seq.length

printfn "part1: %d" part1

let part2 =
    File.ReadLines "input.txt"
    |> Seq.sumBy (fun line ->
        let words = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        let numbers = Seq.map int words |> ResizeArray
        let length = Seq.length numbers
        let mutable foundSafe = false
        for i in seq { 0 .. length-1} do
            if not foundSafe then
                let nums = ResizeArray(numbers)
                nums.RemoveAt(i)
                if numbersAreSafe nums then
                    foundSafe <- true
        if foundSafe then 1 else 0
    )

printfn "part2: %d" part2


