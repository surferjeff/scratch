open System.IO
open System.Text.RegularExpressions

[<Struct>]
type Registers = {
    A: int
    B: int
    C: int
}
let zeros = { A = 0; B = 0; C = 0}

let parseInput path =
    let rx = Regex("(Register (?<regname>A|B|C): (?<regvalue>\\d+))"
        + "|(?<op>\\d+)")
    let mutable regs = zeros
    let program = ResizeArray<int>()
    for m in File.ReadAllText path |> rx.Matches do
        if m.Groups["regname"].Success then
            let regValue = int m.Groups["regvalue"].Value
            match m.Groups["regname"].Value with
            | "A" -> regs <- { regs with A = regValue }
            | "B" -> regs <- { regs with B = regValue }
            | "C" -> regs <- { regs with C = regValue }
            | bad -> failwithf "Bad register name %s" bad
        else
            int m.Groups["op"].Value |> program.Add        

    regs, program.ToArray

type Jump = int option

let execute (regs: Registers) (out: int list) (opCode: int) (operand: int): (Registers * int list * Jump) =
    let combo() =
        match operand with
        | 0 | 1 | 2 | 3 -> operand
        | 4 -> regs.A
        | 5 -> regs.B
        | 6 -> regs.C
        | bad -> failwithf "Invalid combo operand in %A" (opCode, operand)

    match opCode with
    | 0 -> { regs with A = regs.A / (2 <<< combo())}, out, None
    | 1 -> { regs with B = regs.B ^^^ operand }, out, None
    | 2 -> { regs with B = combo() &&& 0b0111 }, out, None
    | 3 -> if regs.A = 0 then
                regs, out, None
            else
                regs, out, Some operand
    | 4 -> { regs with B = regs.B ^^^ regs.C }, out, None
    | 5 -> regs, (combo() &&& 0b011) :: out, None
    | 6 -> { regs with B = regs.A / (2 <<< combo())}, out, None
    | 7 -> { regs with C = regs.A / (2 <<< combo())}, out, None
    | bad -> failwithf "Bad op code in %A" (opCode, operand)

let run (regs: Registers) (program: int array) =
    let mutable regs = regs
    let mutable ip = 0
    let mutable out = []
    while ip < program.Length do
        let xregs, xout, jmp  = execute regs out program[ip] program[ip+1]
        regs <- xregs
        out <- xout
        ip <- match jmp with
                | None -> ip + 2
                | Some n -> n
    regs, List.rev out  

let tests =
    let regs, out = run { zeros with C = 9} [|2; 6|]
    assert(regs.B = 1)

    let regs, out = run { zeros with A = 10} [|5; 0; 5; 1; 5; 4|]
    assert(out = [0; 1; 2])


[<EntryPoint>]
let main argv =
    tests
    0