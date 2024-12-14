open System.IO
open System.Text.RegularExpressions

[<Struct>]
type Robot = {
    // Position
    pX: int; pY: int
    // Velocity
    vX: int; vY: int
}

let parseInput path =
    let text = File.ReadAllText(path)
    let rx = Regex("p=(?<pX>-?\\d+),(?<pY>-?\\d+) v=(?<vX>-?\\d+),(?<vY>-?\\d+)")
    rx.Matches(text)
    |> Seq.map (fun m -> {
        pX = int m.Groups["pX"].Value
        pY = int m.Groups["pY"].Value
        vX = int m.Groups["vX"].Value
        vY = int m.Groups["vY"].Value
    })
    |> Seq.toList

printfn "%A" (parseInput "test1.txt")
