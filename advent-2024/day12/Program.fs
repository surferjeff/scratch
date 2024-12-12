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

let rec flood (grid: char[,]) i j =
    let c = Array2D.get grid i j
    if c >= 'a' then
        (0, 0)  // already visited or boundary
    else
        Array2D.set grid i j (toLower c)
        [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1)]
        |> List.fold (fun (area, fence) (ni, nj) ->
            let nc = Array2D.get grid ni nj
            if c = nc then // unvisited matching color
                let (narea, nfence) = flood grid ni nj
                (area + narea, fence + nfence)
            elif c = (toUpper nc) then // already visited matching color
                (area, fence)
            else // mismatched colors; erect a fence.
                (area, fence + 1)
        ) (1, 0)

let part1 =
    let grid =
        File.ReadAllLines("input.txt")
        |> inputToGrid
    allGridCoords grid
    |> Seq.map (fun (i, j) ->
        let (area, fence) = flood grid i j
        area * fence)
    |> Seq.sum

printf "part1: %d" part1
    


