open System.IO

type Gate = {
    Wires: string list
    Signals: int list
    Op: string
}

[<EntryPoint>]
let main argv =
    File.ReadAllLines argv[0]
    |> Array.fold (fun (wires, gates) line ->
        if line.Contains(':') then
            let words = line.Split(": ") |> Array.map (fun s -> s.Trim())
            let [| wire;  value |] = words
            (wire, value) :: wires, gates
        elif line.Contains("->") then
            let words = line.Split() |> Array.map (fun s -> s.Trim())
            let [| wireA; op; wireB; _; wireC |] = words
            (wires, (wireC, { Wires = [wireA; wireB]; Signals = []; Op = op}) :: gates)
        else
            (wires, gates)
    ) (List.empty, List.empty)
    |> printfn "%A"
    0