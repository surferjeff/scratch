
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
            let c = line[col]
            if c <> ' ' then
                grid <- Map.add c (row, col) grid
    grid

let enumMoves lessChar moreChar iend istart =
    match iend - istart with
    | 0 -> ""
    | n when n < 0 -> String.replicate -n lessChar
    | n -> String.replicate n moreChar

let movesFromMap (map: Map<char, int*int>) =
    Seq.allPairs (Map.keys map) (Map.keys map)
    |> Seq.map (fun (keyStart, keyEnd) -> 
        let rowStart, colStart = Map.find keyStart map
        let rowEnd, colEnd = Map.find keyEnd map
        let moves = (enumMoves "^" "v" rowEnd rowStart) + (
            enumMoves "<" ">" colEnd colStart)
        ((keyStart, keyEnd), moves))
    |> Map

let movesFromPad = mapFromPad >> movesFromMap
let numberMoves = movesFromPad numberPad
let arrowMoves = movesFromPad arrowPad

let pipePrint format thing =
    printfn format thing
    thing

let enumMovesInPattern (movesMap: Map<char*char, string>) (pattern: string) =
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