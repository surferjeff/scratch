open System.IO
open System.Text.RegularExpressions

[<Struct>]
type Registers = {
    A: int64
    B: int64
    C: int64
}
let zeroRegisters = { A = 0L; B = 0L; C = 0L}

[<Struct>]
type Operation = {
    Code: int
    Operand: int
}

let parseInput path =
    let rx = Regex("(Register (?<regname>A|B|C): (?<regvalue>\\d+))"
        + "|((?<opcode>\\d+),(?<operand>\\d+))")
    let mutable regs = zeroRegisters
    let ops = ResizeArray<Operation>()
    for m in File.ReadAllText path |> rx.Matches do
        if m.Groups["regname"].Success then
            let regValue = int64 m.Groups["regvalue"].Value
            match m.Groups["regname"].Value with
            | "A" -> regs <- { regs with A = regValue }
            | "B" -> regs <- { regs with B = regValue }
            | "C" -> regs <- { regs with C = regValue }
            | bad -> failwithf "Bad register name %s" bad
        else
            {
                Code = int m.Groups["opcode"].Value
                Operand = int m.Groups["operand"].Value
            } |> ops.Add        

    regs, ops

let execute (regs: Registers) (op: Operation): Registers =
    zeroRegisters

[<EntryPoint>]
let main argv =
    printfn "%A" (parseInput argv[0])
    0