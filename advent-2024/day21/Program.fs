open System.Collections.Generic

let numberPad = [
    "789"
    "456"
    "123"
    " 0A"
]

let arrowPad = [
    " ^A"
    "<v>"
]

let mapFromPad (pad: string list) =
    let mutable grid = Map.empty
    for row in 0..pad.Length-1 do
        let line = pad[row]
        for col in 0..line.Length - 1 do
            grid <- Map.add line[col] (row, col) grid
    grid

let enumMoves lessChar moreChar iend istart =
    match iend - istart with
    | 0 -> ""
    | n when n < 0 -> String.replicate -n lessChar
    | n -> String.replicate n moreChar

// Moves that pass through the empty space aren't legal.
let moveIsLegal spacePos rowStart colStart moves =
    moves
    |> Seq.scan (fun (row, col) move -> 
        match move with
        | 'v' -> row + 1, col
        | '^' -> row - 1, col
        | '>' -> row, col + 1
        | '<' -> row, col + 1
        | 'A' -> row, col
        | c -> failwithf "Bad move character %c" c
    ) (rowStart, colStart)
    |> Seq.contains spacePos 
    |> not
    
// Maps the starting position and first key press to string of keypresses.
type MovesMap = Dictionary<char*char*char option, string>

let movesFromMap (map: Map<char, int*int>) =
    let moveMap = MovesMap()
    let moveIsLegal = moveIsLegal (Map.find ' ' map)
    for (keyStart, keyEnd) in Seq.allPairs (Map.keys map) (Map.keys map) do
        let rowStart, colStart = Map.find keyStart map
        let rowEnd, colEnd = Map.find keyEnd map
        let vertMoves = enumMoves "^" "v" rowEnd rowStart
        let horizMoves = enumMoves "<" ">" colEnd colStart
        if vertMoves.Length > 0 || horizMoves.Length > 0 then
            let legalMoves = 
                [vertMoves + horizMoves; horizMoves + vertMoves]
                |> List.filter (moveIsLegal rowStart colStart)
            for moves in legalMoves do
                moveMap[(keyStart, keyEnd, Some moves[0])] <- moves
            moveMap[(keyStart, keyEnd, None)] <- legalMoves[0]
    moveMap


let movesFromPad = mapFromPad >> movesFromMap
let numberMoves = movesFromPad numberPad
let arrowMoves = movesFromPad arrowPad

let pipePrint format thing =
    printfn format thing
    thing

let enumMovesInPattern (movesMap: MovesMap) (pattern: string) =
    let mutable moves = []
    for i in pattern.Length-2..-1..0 do
        moves <- (pattern[i], pattern[i+1]) :: moves
    moves <- ('A', pattern[0]) :: moves
    moves |> List.map (fun a2b -> (Map.find a2b movesMap) + "A")

let tripleCode (code: string) =
    code
    |> pipePrint "%A"
    |> enumMovesInPattern numberMoves
    |> pipePrint "%A"
    |> String.concat ""
    |> enumMovesInPattern arrowMoves
    |> pipePrint "%A"
    |> String.concat ""
    |> enumMovesInPattern arrowMoves
    |> pipePrint "%A"
    |> String.concat ""

let part1 (codes: string list) =
    codes
    |> List.map (fun code -> code, (tripleCode code))
    |> List.map (fun (code, tripleCode) -> (tripleCode.Length, int code[0..code.Length-2]))
    |> pipePrint "%A"
    |> List.map (fun (n, m) -> n * m)
    |> List.sum
    |> printfn "part1: %d"

[<EntryPoint>]
let main argv =
    part1 [
        // "029A"
        // "980A"
        // "179A"
        // "456A"
        "379A"
    ]
    0