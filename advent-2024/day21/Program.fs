
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

let countMoves map keyStart keyEnd =
    let rowStart, colStart = Map.find keyStart map
    let rowEnd, colEnd = Map.find keyEnd map
    abs (rowStart - rowEnd) + abs (colStart - colEnd)

let countMovesInPattern (map: Map<char, int*int>) (pattern: char seq)=
    pattern
    |> Seq.append (Seq.singleton 'A')
    |> Seq.rev
    |> Seq.fold (fun (pairList, prev) keyPress ->
        (keyPress, prev) :: pairList, keyPress) ([], '*')
    |> fst
    |> Seq.filter (fun (keyStart, keyEnd) -> keyEnd <> '*')
    |> printfn "%A"

countMovesInPattern numberMap "029A"

