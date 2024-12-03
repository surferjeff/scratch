open System.IO
open System.Text.RegularExpressions

let part1 =
    let text = File.ReadAllText("input.txt")
    let rx = Regex("mul\([0-9]{1,3},[0-9]{1,3}\)")
    for m in rx.EnumerateMatches(text) do
        printfn "%s" (text.Substring(m.Index, m.Length))
