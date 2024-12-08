open System.IO
open System

let input = File.ReadAllLines("input.txt")

let lines = input |> Array.map (fun line ->
    let separators = ": ".ToCharArray()
    line.Split(separators,  StringSplitOptions.RemoveEmptyEntries) |> Array.map uint64)

let combine1 a b =
    [a + b; a * b]

let canEquate combine (line: uint64 array) =
    let rec operate (result: uint64) (numbers: uint64 list) (partialResult: uint64) =
        match numbers with
        | [] -> if result = partialResult then Some result else None
        | n :: rest ->
            combine partialResult n
            |> List.tryPick (operate result rest)
    let result ::  first :: numbers = List.ofArray line
    operate result numbers first

let part1 =
    lines |> Array.map (canEquate combine1) |> Array.choose id |> Array.sum

printfn "part1: %d" part1

let combine2 a b =
    [a + b; a * b; uint64 ((string a) + (string b))]

printfn "part2: %d" (lines |> Array.map (canEquate combine2) |> Array.choose id |> Array.sum)
