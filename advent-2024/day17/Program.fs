open System.IO
open System.Text.RegularExpressions

[<Struct>]
type Registers = {
    A: int
    B: int
    C: int
    IP: int  // Instruction pointer
}
let zeros = { A = 0; B = 0; C = 0; IP = 0}

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

    regs, program.ToArray()

let operate (program: int array) (regs: Registers, out: int list)
        : (Registers * int list) =
    let opCode, operand = program[regs.IP], program[regs.IP+1]
    let combo() =
        match operand with
        | 0 | 1 | 2 | 3 -> operand
        | 4 -> regs.A
        | 5 -> regs.B
        | 6 -> regs.C
        | bad -> failwithf "Invalid combo operand in %A" (opCode, operand)

    match opCode with
    | 0 -> { regs with A = regs.A / (1 <<< combo()); IP = regs.IP + 2 }, out
    | 1 -> { regs with B = regs.B ^^^ operand; IP = regs.IP + 2 }, out
    | 2 -> { regs with B = combo() &&& 0b0111; IP = regs.IP + 2 }, out
    | 3 -> if regs.A = 0 then
                { regs with IP = regs.IP + 2 }, out
            else
                { regs with IP = operand }, out
    | 4 -> { regs with B = regs.B ^^^ regs.C; IP = regs.IP + 2 }, out
    | 5 -> { regs with IP = regs.IP + 2}, (combo() &&& 0b0111) :: out
    | 6 -> { regs with B = regs.A / (1 <<< combo()); IP = regs.IP + 2 }, out
    | 7 -> { regs with C = regs.A / (1 <<< combo()); IP = regs.IP + 2}, out
    | bad -> failwithf "Bad op code in %A" (opCode, operand)

let run (regs: Registers, program: int array) =
    let mutable state = regs, []
    let keepRunning (regs, _) = regs.IP < program.Length
    while keepRunning state do
        state <- operate program state
    let regs, out = state
    regs, List.rev out  

let runExpect (regs: Registers) (program: int array) (expected: int list) =
    let mutable expected = expected
    let mutable regs = regs
    let mutable matchingOut = true
    let mutable i = 0
    while regs.IP < program.Length && matchingOut do
        let xregs, xout = operate program (regs, [])
        match xout, expected with
        | [n], m :: rest -> 
            if n = m then
                expected <- rest
            else
                matchingOut <- false
        | [n], [] -> matchingOut <- false
        | _ -> ()
        i <- i + 1
        regs <- xregs
    matchingOut && expected = []

let part2 =
    let regs, program = parseInput "input.txt"
    let expected = program |> Array.rev |> Array.toList
    let mutable found = false
    let mutable regs = { regs with A = 0 }
    while not found do
        regs <- { regs with A = regs.A + 1 }
        found <- runExpect regs program expected
    printfn "%A" regs

let tests() =
    let regs, out = run ({ zeros with C = 9}, [|2; 6|])
    assert(regs.B = 1)

    let regs, out = run ({ zeros with A = 10}, [|5; 0; 5; 1; 5; 4|])
    assert(out = [0; 1; 2])

    let regs, out = run ({ zeros with A = 2024}, [|0;1;5;4;3;0|])
    assert(out = [4;2;5;6;7;7;7;7;3;1;0])
    assert(regs.A = 0)

    let regs, out = run ({ zeros with B = 29}, [|1; 7|])
    assert(regs.B = 26)

    let regs, out = run ({ zeros with B = 2024; C = 43690}, [|4; 0|])
    assert(regs.B = 44354)

let failingTests() = ()

[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        if argv[0] = "part2" then
            part2
        else
            let _, out = parseInput argv[0] |> run
            out |> Seq.map string |> String.concat "," |> printfn "part1: %s"
    else
        failingTests()
        tests()
    0
