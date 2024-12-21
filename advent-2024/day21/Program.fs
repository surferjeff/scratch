
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

let numberMap = mapFromPad numberPad
let arrowMap = mapFromPad arrowPad

let pipePrint format thing =
    printfn format thing
    thing

let countMoves map (keyStart, keyEnd) =
    let rowStart, colStart = Map.find keyStart map
    let rowEnd, colEnd = Map.find keyEnd map
    1 + abs (rowStart - rowEnd) + abs (colStart - colEnd)

let countMovesInPattern (map: Map<char, int*int>) (pattern: char array) =
    let mutable moves = []
    for i in pattern.Length-2..-1..0 do
        moves <- (pattern[i], pattern[i+1]) :: moves
    moves <- ('A', pattern[0]) :: moves
    moves
    |> pipePrint "%A"
    |> List.map (countMoves map)
    |> pipePrint "%A"
    |> List.sum

countMovesInPattern numberMap ("029A".ToCharArray())
|> printfn "%d"

