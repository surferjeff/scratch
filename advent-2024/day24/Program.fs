open System.IO

[<Struct>]
type Gate = {
    Name: string
    Wires: string list
    Signals: int list
    Op: string
}

// Checks to see if gate's input wires are carrying a signal.  If so, updates
// allWires with its signal and returns None.
let propagate (allWires: Map<string, int>) (gate: Gate) =
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
        Map.add gate.Name signal allWires, None
    else
        allWires, Some {gate with Wires = wires; Signals = signals}


[<EntryPoint>]
let main argv =
    let mutable wires, gates =
        File.ReadAllLines argv[0]
        |> Array.fold (fun (wires, gates) line ->
            if line.Contains(':') then
                let words = line.Split(": ") |> Array.map (fun s -> s.Trim())
                let [| wire;  value |] = words
                Map.add wire (int value) wires,  gates
            elif line.Contains("->") then
                let words = line.Split() |> Array.map (fun s -> s.Trim())
                let [| wireA; op; wireB; _; wireC |] = words
                (wires, { Name = wireC; Wires = [wireA; wireB]; Signals = []; Op = op} :: gates)
            else
                (wires, gates)
        ) (Map.empty, List.empty)

    while not (List.isEmpty gates) do
        let nextWires, nextGates = 
            gates
            |> List.fold (fun (wires, gates) gate ->
                match propagate wires gate with
                | wires, None -> wires, gates
                | wires, Some gate -> wires, gate :: gates
            ) (wires, List.empty)
        wires <- nextWires
        gates <- nextGates

    printfn "wires: %A\ngates: %A" wires gates

    let mutable z = 0
    for i in 0..99 do 
        let wire = sprintf "z%02d" i
        match wires |> Map.tryFind wire with
        |  Some n -> z <- z ||| (n <<< i)
        | _ -> ()

    printfn "part1: %d" z

    0