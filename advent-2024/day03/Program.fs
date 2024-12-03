open System.IO
open System.Text.RegularExpressions

let part1 =
    let text = File.ReadAllText("input.txt")
    let rx = Regex("mul\(([0-9]{1,3}),([0-9]{1,3})\)")
    rx.Matches(text) |> Seq.sumBy (fun m ->
        let a = int m.Groups[1].Value
        let b = int m.Groups[2].Value
        a * b
    )

printfn "part1: %d" part1

let part2 =
    let text = File.ReadAllText("input.txt")
    let rx = Regex("(?<do>do\(\))|(?<dont>don't\(\))|(?<mul>mul\((?<a>[0-9]{1,3}),(?<b>[0-9]{1,3})\))")
    let mutable doing = true
    let mutable total = 0
    for m in rx.Matches(text) do 
        if m.Groups["do"].Success then
            doing <- true
        elif m.Groups["dont"].Success then
            doing <- false
        elif doing && m.Groups["a"].Success then
            let a = int m.Groups["a"].Value
            let b = int m.Groups["b"].Value
            total <- total + a * b
    printfn "part2: %d" total
        
