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

let play (game: Game) =
    let prizeX, prizeY = game.Prize
    let aX, aY = game.A
    let bX, bY = game.B
    let rec loop aCount bCount best =
        if aCount < 0 then 
            best
        else
            let clawX = aCount * aX + bCount * bX
            let clawY = aCount * aY + bCount * bY
            if (clawX, clawY) = game.Prize then
                let tokens = 3 * aCount + bCount
                loop (aCount - 1) bCount (max best (Some -tokens))
            elif clawX > prizeX || clawY > prizeY then
                loop (aCount - 1) bCount best
            else
                loop aCount (bCount + 1) best
    let aCount = min (prizeX / aX) (prizeY / aY)
    loop aCount 0 None |> Option.map (fun n -> -n)

let input = readInput "input.txt"

let part1 =
    input
    |> Seq.map play
    |> Seq.choose id
    |> Seq.sum

printfn "part1: %d" part1

