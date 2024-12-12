open System.IO

// A capital letter represents unvisited.  Lowercase means visited.
// ~ means beyond the edge.
let inputToGrid (lines: string array) =
    let cols = lines[0].Length
    let border = [| String.replicate (cols + 2) "~" |]
    let grid = Seq.concat [
        border
        lines |> Array.map (fun line -> $"~{line}~")
        border
    ]
    array2D grid

let toUpper (c: char) = char ((int c) &&& 0b11011111)
let toLower (c: char) = char ((int c) ||| 0b00100000)

let allGridCoords arr = seq {
    for i in 0..Array2D.length1 arr - 1 do
        for j in 0..Array2D.length2 arr - 1 do
            yield (i, j)
}

[<Struct>]
type Fence = {
    Span: char  // 'i' or 'j'
    I: int
    J: int
}

let neighbors i j = [
    i + 1, j, {Span = 'i'; I = i; J = j}
    i - 1, j, {Span = 'I'; I = i - 1; J = j}
    i, j + 1, {Span = 'j'; I = i; J = j}
    i, j - 1, {Span = 'J'; I = i; J = j - 1 }
]

// Sorting by fenceKey makes it easy to identify sides.
let fenceKey fence =
    if toLower fence.Span = 'i' then
        (fence.I, fence.J)
    else
        (fence.J, fence.I) 

// Returns the total area and a list of Fences.
let rec flood (grid: char[,]) i j accArea accFence =
    let c = Array2D.get grid i j
    if c >= 'a' then
        (accArea, accFence)  // already visited or boundary
    else
        Array2D.set grid i j (toLower c)
        neighbors i j
        |> List.fold (fun (area, fence) (ni, nj, nfence) ->
            let nc = Array2D.get grid ni nj
            if c = nc then // unvisited matching color
                flood grid ni nj area fence
            elif c = (toUpper nc) then // already visited matching color
                (area, fence)
            else // mismatched colors; erect a fence.
                (area, nfence :: fence)
        ) (1 + accArea, accFence)

let collectRegions inputPath =
    let grid =
        File.ReadAllLines(inputPath)
        |> inputToGrid
    allGridCoords grid
    |> Seq.map (fun (i, j) -> flood grid i j 0 [])

let regions = collectRegions "input.txt" |> Seq.filter (fun (area, _) -> area > 0) |> Seq.toList
let part1 = regions |> List.sumBy (fun (area, fences) -> area * (List.length fences))

printfn "part1: %d" part1

let countSides (fences: Fence list) =
    fences
    |> List.groupBy (fun fence -> fence.Span)
    |> List.map (fun (_, fenceList) -> fenceList |> List.map fenceKey |> List.sort)
    |> List.sumBy (fun fenceList ->
        let _, acc =
            fenceList
            |> List.fold (fun (prev, acc) (i, j) ->
                if prev = (i, j - 1) then
                    ((i, j), acc)  // Continue the side.
                else
                    ((i, j), acc + 1) // New side
            ) ((-1, -1), 0)
        acc
    )

let part2 = regions |> List.sumBy (fun (area, fences) -> area * (countSides fences))

printfn "part2: %d" part2
    


