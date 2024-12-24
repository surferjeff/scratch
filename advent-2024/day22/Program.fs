open System.IO

let mix (a: int64) (b: int64) = a ^^^ b
let prune (a: int64) = a % 16777216L

let nextSecret (number: int64) =
    let n1 = number * 64L |> mix number |> prune
    let n2 = n1 / 32L |> mix n1 |> prune
    let n3 = n2 * 2048L |> mix n2 |> prune
    n3

let nextN n secret =
    {0..n-1} |> Seq.scan (fun n _ -> nextSecret n) secret

let sampl2() =
    for n in nextN 10 123 do
        printfn "%d" n

let sample2() =
    [1; 10; 100; 2024]
    |> List.map (fun n -> [int64 n; nextN 2000 (int64 n) |> Seq.last])
    |> printfn "%A"

[<EntryPoint>]
let main argv =
    File.ReadAllLines argv[0]
    |> Array.map int64
    |> Array.map (nextN 2000 >> Seq.last)
    |> Array.sum
    |> printfn "%A"
    0