
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

let enumMoves less more iend istart =
    match iend - istart with
    | 0 -> ""
    | n when n < 0 -> String.replicate -n less
    | n -> String.replicate n more

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

printfn "%A" numberMoves


let pipePrint format thing =
    printfn format thing
    thing

// let countMoves map (keyStart, keyEnd) =
//     let rowStart, colStart = Map.find keyStart map
//     let rowEnd, colEnd = Map.find keyEnd map
//     1 + abs (rowStart - rowEnd) + abs (colStart - colEnd)

// let countMovesInPattern (map: Map<char, int*int>) (pattern: char array) =
//     let mutable moves = []
//     for i in pattern.Length-2..-1..0 do
//         moves <- (pattern[i], pattern[i+1]) :: moves
//     moves <- ('A', pattern[0]) :: moves
//     moves
//     |> pipePrint "%A"
//     |> List.map (countMoves map)
//     |> pipePrint "%A"
//     |> List.sum

// countMovesInPattern numberMap ("029A".ToCharArray())
// |> printfn "%d"

