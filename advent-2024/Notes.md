

Day 16 072c9f9f797e9608488afba3f2fbae4e19e4d301

First I implemented a loop with a tail recursion and got a stack overflow.
So I learn Seq.unfold.  It worked, but was slow and the code was hard
to understand.  So, I switched to a while loop and mutable code and
everything improved.  Add 'continue' and 'return' statements to F#, and the
code would just about be perfect.


Day 20 c2e9dc97e751ffcef0d1f2f5271516fb5a735f9f

There's a bug in this code:
```
    paths
    |> enumArray2D
    |> Seq.choose (fun (row, col, square) ->
        match square with
        | None -> None
        | Some start -> Some (row, col, start))
    |> Seq.map (fun (startRow, startCol, cheatStart) ->
        cheatDirs
        |> Seq.map (fun (dRow, dCol) -> startRow + dRow, startCol + dCol)
        |> Seq.filter inBounds
        |> Seq.choose (fun (row, col) -> paths[row, col])
        |> Seq.map (fun cheatEnd -> cheatEnd - cheatStart)
        |> Seq.filter (fun cheatSavings -> cheatSavings > 1))
    |> Seq.collect id
    |> Seq.groupBy id
    |> Seq.map (fun (n, nlist) -> (n, Seq.length nlist))
    |> Seq.sort
    |> Seq.toList
```

I initially started writing this code with if statements and while loops,
but the inner code was indented more than halfway across the screen.
`continue`, `break`, and `return` statements would have kept the indentation
reasonable, but they're missing from F#.

So, I switched to F#'s `filter`, `map`, `c`, etc.  Problems:
1. It was very difficult to write, even after 20 days of intensely working 
   through Advent of Code problems with F#.  I had lots of syntax errors.
   For puzzles, I guess it's OK.  When I'm trying to solve users' problems,
   I don't want to waste energy trying to solve F#'s problems.
2. Debugging this code is very hard.  Inserting `printfn` statements isn't
   simple.  Neither is setting breakpoints in useful places.

