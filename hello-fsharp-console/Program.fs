// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let nine = Square.square 3

printfn "%A" nine

let rec sumTo x =
    if x = 0 then
        0
    else
        x + (sumTo (x - 1))

printfn "%A" (sumTo 5)