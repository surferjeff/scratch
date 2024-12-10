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
    let (_, total) =
        rx.Matches(text)
        |> Seq.fold (fun (doing, total) m ->
            if m.Groups["do"].Success then
                true, total
            elif m.Groups["dont"].Success then
                false, total
            elif doing then
                true, (int m.Groups["a"].Value) * (int m.Groups["b"].Value)
            else
                doing, total)
            (true, 0)

    printfn "part2: %d" total
        
