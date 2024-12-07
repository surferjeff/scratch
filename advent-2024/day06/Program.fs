open System.IO

let main =
    let lines = File.ReadAllLines("input.txt")

    // Find the starting position
    let mutable pos = (-1, -1)
    for row in 0..lines.Length-1 do
        if (-1, -1) = pos then
            let line = lines[row]
            let i = line.IndexOf('^')
            if i >= 0 then
                pos <- (row, i)

    // Trace the path.
    let rows = lines.Length
    let cols = lines[0].Length
    let lines = lines |> Array.map (fun line -> line.ToCharArray())
    let mutable step = (-1, 0)
    let mutable exit = false
    while not exit do
        let (row, col) = pos
        lines[row][col] <-'+'
        let (i, j) = step
        let (row, col) = (row + i, col + j)
        if row < 0 || row >= rows || col < 0 || col >= cols then
            exit <- true
        else if lines[row][col] = '#' then
            // Turn right 90 degrees.
            step <-
                match step with
                | (-1, 0) -> (0, 1)
                | (0, 1) -> (1, 0)
                | (1, 0) -> (0, -1)
                | (0, -1) -> (-1, 0)
                | bad -> failwith "Bad step"
        else
            pos <- (row, col)

    // Count path.
    let count =
        lines |> Array.sumBy (fun row -> 
            Array.filter ((=) '+') row |> Array.length)
    
    printfn "part1 %d" count