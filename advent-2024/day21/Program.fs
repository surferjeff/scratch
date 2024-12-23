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

let pipePrint format thing =
    printfn format thing
    thing

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
type MovesMap = Map<char*char, string list>

let movesFromMap (map: Map<char, int*int>) =
    let moveIsLegal = moveIsLegal (Map.find ' ' map)
    let legalKeys = map.Keys |> Seq.filter (fun c -> c <> ' ') |> Seq.toList
    Seq.allPairs legalKeys legalKeys
    |> Seq.fold (fun movesMap (keyStart, keyEnd) ->
        let rowStart, colStart = Map.find keyStart map
        let rowEnd, colEnd = Map.find keyEnd map
        let vertMoves = enumMoves "^" "v" rowEnd rowStart
        let horizMoves = enumMoves "<" ">" colEnd colStart
        let legalMoves = 
            [vertMoves + horizMoves; horizMoves + vertMoves]
            |> List.filter (fun s -> s.Length > 0)
            |> List.distinct
            |> List.filter (moveIsLegal rowStart colStart)
        Map.add (keyStart, keyEnd) legalMoves movesMap) Map.empty

let movesFromPad = mapFromPad >> movesFromMap
let numberMoves = movesFromPad numberPad
let arrowMoves = movesFromPad arrowPad

[<Struct>]
type Segment =
| Defined of string
| Undefined of string * string

// Try to find a pair where the last character of a matches the first character of b.
// This will minimize keystrokes at the next level.
let joinMoves (a: string list) (b: string list) =
    List.allPairs a b
    |> List.tryFind (fun (a, b) -> a[a.Length-1] = b[0])
    |> Option.map (fun (a, b) -> [a], [b])
    |> Option.defaultValue ((List.take 1 a), b)

// There are two options for some moves from one key to another.  Select a path
// that minimizes key changes.
let selectMoves (moves: string list seq) =
    let head = Seq.head moves
    let tail = Seq.tail moves
    tail
    |> Seq.fold (fun past next ->
        let pastHead, newHead = joinMoves (List.head past) next
        newHead :: pastHead :: List.tail past
    ) [head]
    |> List.collect id
    |> List.append [""]
    |> List.rev
    |> String.concat "A"

let explodeMoves (moves: string list list) =
    let rec explode moves = 
        match moves with
        | head :: [] -> [head]
        | head :: tail ->
            let tails = explode tail
            List.allPairs head tails
            |> List.map (fun (h, t) -> h :: t)
        | tail -> failwithf "Unexpected tail %A" tail
    explode moves
    |> List.map (fun moves ->
        let moves = Seq.append moves [""]
        String.concat "A" moves)

let enumMovesInPattern (movesMap: MovesMap) (pattern: char seq) =
    pattern
    |> Seq.append "A"
    |> Seq.pairwise
    |> Seq.map (fun keyPresses -> Map.find keyPresses movesMap)
    |> Seq.filter (not << List.isEmpty)

let a29() =
    "029A"
    |> enumMovesInPattern numberMoves
    |> Seq.toList
    |> pipePrint "%A"
    |> explodeMoves
    |> printfn "%A"

let solve movesMap = enumMovesInPattern movesMap >> selectMoves >> pipePrint "%A"

let tripleCode (code: string) =
    code
    |> pipePrint "%A"
    |> solve numberMoves
    |> solve arrowMoves
    |> solve arrowMoves

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
    a29()
    // part1 [
    //     "029A"
    //     "980A"
    //     "179A"
    //     "456A"
    //     "379A"
    // ]
    0