open System.IO

[<Struct>]
type Gate = {
    Wires: string list
    Signals: int list
    Op: string
}

// Checks to see if gate's input wires are carrying a signal.  If so, updates
// allWires with its signal and returns None.
let propagate (allWires: Map<string, int>) (gateName: string, gate: Gate) =
    let wires, signals =
        gate.Wires
        |> List.fold (fun (gateWires, signals) wire ->
            match Map.tryFind wire allWires with
            | None -> (wire :: gateWires, signals)
            | Some signal -> (gateWires, signal :: signals)) (List.empty, List.empty)
    if List.isEmpty wires then
        let signal =
            match gate.Op with
            | "AND" -> List.fold (&&&) 1 signals
            | "OR" -> List.fold (|||) 0 signals
            | "XOR" -> List.fold (^^^) 0 signals
            | bad -> failwithf "Bad operation %s" bad
        Map.add gateName signal allWires, None
    else
        allWires, Some {gate with Wires = wires; Signals = signals}


[<EntryPoint>]
let main argv =
    let wires, gates =
        File.ReadAllLines argv[0]
        |> Array.fold (fun (wires, gates) line ->
            if line.Contains(':') then
                let words = line.Split(": ") |> Array.map (fun s -> s.Trim())
                let [| wire;  value |] = words
                Map.add wire (int value) wires,  gates
            elif line.Contains("->") then
                let words = line.Split() |> Array.map (fun s -> s.Trim())
                let [| wireA; op; wireB; _; wireC |] = words
                (wires, (wireC, { Wires = [wireA; wireB]; Signals = []; Op = op}) :: gates)
            else
                (wires, gates)
        ) (Map.empty, List.empty)

    propagate wires (List.head gates) |> printfn "%A"

    0