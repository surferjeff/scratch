open System.IO
open System.Text.RegularExpressions

let part1 =
    let text = File.ReadAllText("input.txt")
    let rx = Regex("mul\(([0-9]{1,3}),([0-9]{1,3})\)")
    let mutable total = 0
    for m in rx.Matches(text) do
        let a = int m.Groups[1].Value
        let b = int m.Groups[2].Value
        total <- total +  a * b
    printfn "part1: %d" total