open System.IO

let input = File.ReadAllLines("input.txt")

// A capital letter represents unvisited.  Lowercase means visited.
// # means beyond the edge.
let inputToGrid (lines: string array) =
    let cols = lines[0].Length
    let border = [| String.replicate (cols + 2) "#" |]
    let grid = Seq.concat [
        border
        lines |> Array.map (fun line -> $"#{line}#")
        border
    ]
    array2D grid

let toUpper (c: char) = char ((int c) &&& 0b11011111)
let toLower (c: char) = char ((int c) ||| 0b00100000)

let rec flood (grid: char[,]) i j =
    let c = Array2D.get grid i j
    Array2D.set grid i j (toLower c)
    [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1)]
    |> List.fold (fun (area, fence) (ni, nj) ->
        let nc = Array2D.get grid ni nj
        if nc = '#' then // boundary
            (area, fence + 1)
        elif c = nc then // unvisited matching color
            flood grid ni nj
        elif c = (toUpper nc) then // already visited matching color
            (area, fence)
        else // mismatched colors
            (area, fence + 1)
    ) (1, 0)
