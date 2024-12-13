open System.IO
open System.Text.RegularExpressions

[<Struct>]
type Game = {
    A: int * int
    B: int * int
    Prize: int * int
}

let zeroGame = { A = (0, 0); B = (0, 0); Prize = (0, 0) }

let readInput path =
    let rx = Regex("(Button A: X\+(?<AX>\d+), Y\+(?<AY>\d+))"
        + "|(Button B: X\+(?<BX>\d+), Y\+(?<BY>\d+))"
        + "|(Prize: X=(?<ZX>\d+), Y=(?<ZY>\d+))")
    let getXY (m: Match) (x: string) (y: string) =
        (int m.Groups[x].Value, int m.Groups[y].Value)
    let _, games =
        rx.Matches(File.ReadAllText path)
        |> Seq.fold (fun (game, accGames) m ->
            if m.Groups["AX"].Success then
                ({game with A = getXY m "AX" "AY"}, accGames)
            elif m.Groups["BX"].Success then
                ({game with B = getXY m "BX" "BY"}, accGames)
            elif m.Groups["ZX"].Success then
                (zeroGame, {game with Prize = getXY m "ZX" "ZY"} :: accGames)
            else
                (game, accGames)
        ) (zeroGame, [])
    games

let games = readInput "input.txt"
for game in games do
    printfn "%A" game