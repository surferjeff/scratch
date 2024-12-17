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

let execute (program: int array) (regs: Registers, ip: int, out: int list): (Registers * int * int list) =
    let opCode, operand = program[ip], program[ip+1]
    let combo() =
        match operand with
        | 0 | 1 | 2 | 3 -> operand
        | 4 -> regs.A
        | 5 -> regs.B
        | 6 -> regs.C
        | bad -> failwithf "Invalid combo operand in %A" (opCode, operand)

    match opCode with
    | 0 -> { regs with A = regs.A / (1 <<< combo())}, ip + 2, out
    | 1 -> { regs with B = regs.B ^^^ operand }, ip + 2, out
    | 2 -> { regs with B = combo() &&& 0b0111 }, ip + 2, out
    | 3 -> if regs.A = 0 then
                regs, ip + 2, out
            else
                regs, operand, out
    | 4 -> { regs with B = regs.B ^^^ regs.C }, ip + 2, out
    | 5 -> regs, ip + 2, (combo() &&& 0b0111) :: out
    | 6 -> { regs with B = regs.A / (2 <<< combo())}, ip + 2, out
    | 7 -> { regs with C = regs.A / (2 <<< combo())}, ip + 2, out
    | bad -> failwithf "Bad op code in %A" (opCode, operand)

let run (regs: Registers) (program: int array) =
    let mutable state = (regs, 0, [])
    let isDone (_, ip, _) = ip >= program.Length
    while not (isDone state) do
        state <- execute program state
    let regs, _, out = state
    regs, List.rev out  

let tests() =
    let regs, out = run { zeros with C = 9} [|2; 6|]
    assert(regs.B = 1)

    let regs, out = run { zeros with A = 10} [|5; 0; 5; 1; 5; 4|]
    assert(out = [0; 1; 2])

    let regs, out = run { zeros with A = 2024} [|0;1;5;4;3;0|]
    assert(out = [4;2;5;6;7;7;7;7;3;1;0])
    assert(regs.A = 0)

    let regs, out = run { zeros with B = 29} [|1; 7|]
    assert(regs.B = 26)

    let regs, out = run { zeros with B = 2024; C = 43690} [|4; 0|]
    assert(regs.B = 44354)

let failingTests() = ()

[<EntryPoint>]
let main argv =
    failingTests()
    tests()
    0