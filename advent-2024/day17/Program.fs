open System.IO
open System.Text.RegularExpressions
open System.Reflection
open System.Reflection.Emit

type Reg = int64
let reg n = int64 n
[<Struct>]
type Registers = {
    A: Reg
    B: Reg
    C: Reg
    IP: Reg  // Instruction pointer
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

let compile (program: int array) =
    let assemblyName = new AssemblyName("Compiler")
    let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule("Compiler")
    let args = [| typeof<int64 array>; typeof<int array> |]
    let method = DynamicMethod("run", typeof<int>, args, moduleBuilder)
    let gen = method.GetILGenerator(program.Length * 32)
    let labels = { 0..20 } |> Seq.map (fun _ -> gen.DefineLabel() ) |> Seq.toArray
    let mutable ilabel = 0

    // Index into the out array
    let iout = gen.DeclareLocal(typeof<int>)
    gen.Emit(OpCodes.Ldc_I4_0) 
    gen.Emit(OpCodes.Stloc, iout)

    // let emitLoadRegister index =
    //     gen.Emit(OpCodes.Ldarg_0)
    //     gen.Emit(index)
    //     gen.Emit(OpCodes.Ldelem_I8)
    
    // let emitLoadA() = emitLoadRegister OpCodes.Ldc_I4_0
    // let emitLoadB() = emitLoadRegister OpCodes.Ldc_I4_1
    // let emitLoadC() = emitLoadRegister OpCodes.Ldc_I4_2

    // let emitStoreRegister index =
    //     gen.Emit(OpCodes.Ldarg_0)
    //     gen.Emit(index)
    //     gen.Emit(OpCodes.Stelem_I8)

    // let emitStoreA() = emitStoreRegister OpCodes.Ldc_I4_0
    // let emitStoreB() = emitStoreRegister OpCodes.Ldc_I4_1
    // let emitStoreC() = emitStoreRegister OpCodes.Ldc_I4_2

    // let emitCombo (operand: int) =
    //     match operand with
    //     | 0 | 1 | 2 | 3 -> gen.Emit(OpCodes.Ldc_I8, operand)
    //     | 4 -> emitLoadA()
    //     | 5 -> emitLoadB()
    //     | 6 -> emitLoadC()
    //     | bad -> failwithf "Invalid combo operand in %d" operand

    // let emitADivCombo (gen: ILGenerator) operand =
    //     emitLoadA()
    //     gen.Emit(OpCodes.Ldc_I8, 1)
    //     emitCombo operand
    //     gen.Emit(OpCodes.Shl)
    //     gen.Emit(OpCodes.Div)

    // for i in 0..0 do
    //     let operand = program[i+1]
    //     if ilabel < labels.Length then
    //         gen.MarkLabel(labels[ilabel])
    //         ilabel <- ilabel + 1
    //     match program[i] with
    //     | 0 ->
    //         emitADivCombo gen operand
    //         emitStoreA()
    //     | 1 ->
    //         emitLoadB()
    //         gen.Emit(OpCodes.Ldc_I8, operand)
    //         gen.Emit(OpCodes.Xor)
    //         emitStoreB()
    //     | 2 ->
    //         emitCombo operand
    //         gen.Emit(OpCodes.Ldc_I8, 0x0111)
    //         gen.Emit(OpCodes.And)
    //         emitStoreB()
    //     | 3 ->
    //         emitLoadA()
    //         let skip = gen.DefineLabel()
    //         gen.Emit(OpCodes.Ldc_I8, 0)
    //         gen.Emit(OpCodes.Beq, skip)
    //         gen.Emit(OpCodes.Br, labels[operand / 2])
    //         gen.MarkLabel(skip)            
    //     | 4 ->
    //         emitLoadB()
    //         emitLoadC()
    //         gen.Emit(OpCodes.Xor)
    //         emitStoreB()
    //     | 5 ->
    //         gen.Emit(OpCodes.Ldarg_1)
    //         gen.Emit(OpCodes.Ldloc, iout)

    //         emitCombo operand
    //         gen.Emit(OpCodes.Ldc_I8, 0x0111)
    //         gen.Emit(OpCodes.And)

    //         gen.Emit(OpCodes.Stelem_I4)

    //         gen.Emit(OpCodes.Ldloc, iout)
    //         gen.Emit(OpCodes.Ldc_I8, 1)
    //         gen.Emit(OpCodes.Add)
    //         gen.Emit(OpCodes.Stloc, iout)
    //     | 6 -> 
    //         emitADivCombo gen operand
    //         emitStoreB()
    //     | 7 -> 
    //         emitADivCombo gen operand
    //         emitStoreC()
    //     | bad -> failwithf "Bad op code %d" program[i]

    gen.Emit(OpCodes.Ldloc, iout)
    gen.Emit(OpCodes.Ret)
    method

let runCompiled (regs: Registers, program: int array) =
    let compiled = compile program
    let outBuffer = Array.create 100 0
    let regsArray = [| regs.A; regs.B; regs.C |]
    let args  = [| box regsArray ; outBuffer |]
    let outLen = compiled.Invoke(null, args)
    let regs = {
        A = regsArray[0];
        B = regsArray[1];
        C = regsArray[2];
        IP = program.Length
    }
    let outList =
        outBuffer
        |> Array.take (unbox outLen)
        |> Array.toList
    regs, outList


let operate (program: int array) (regs: Registers, out: int list)
        : (Registers * int list) =
    let opCode, operand = program[int regs.IP], program[int regs.IP+1]
    let combo() =
        match operand with
        | 0 | 1 | 2 | 3 -> operand
        | 4 -> int32 regs.A
        | 5 -> int32 regs.B
        | 6 -> int32 regs.C
        | bad -> failwithf "Invalid combo operand in %A" (opCode, operand)

    match opCode with
    | 0 -> { regs with A = regs.A / (1L <<< combo()); IP = regs.IP + 2L }, out
    | 1 -> { regs with B = regs.B ^^^ operand; IP = regs.IP + 2L }, out
    | 2 -> { regs with B = combo() &&& 0b0111; IP = regs.IP + 2L }, out
    | 3 -> if regs.A = 0 then
                { regs with IP = regs.IP + 2L }, out
            else
                { regs with IP = operand }, out
    | 4 -> { regs with B = regs.B ^^^ regs.C; IP = regs.IP + 2L }, out
    | 5 -> { regs with IP = regs.IP + 2L}, (combo() &&& 0b0111) :: out
    | 6 -> { regs with B = regs.A / (1L <<< combo()); IP = regs.IP + 2L }, out
    | 7 -> { regs with C = regs.A / (1L <<< combo()); IP = regs.IP + 2L}, out
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

let part2() =
    let regs, program = parseInput "input.txt"
    let expected = program |> Array.rev |> Array.toList
    let mutable found = false
    let mutable regs = { regs with A = 0 }
    let mutable nextPrint = 1_000_000
    while not found do
        regs <- { regs with A = regs.A + 1L }
        found <- runExpect regs program expected
        if regs.A = nextPrint then
            printfn "%d" nextPrint
            nextPrint <- nextPrint + 1_000_000
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

let compiledTests() = 
    let regs, out = runCompiled ({ zeros with C = 9}, [|2; 6|])
    assert(regs.B = 1)


let failingTests() = ()

[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        if argv[0] = "part2" then
            part2()
        else
            let _, out = parseInput argv[0] |> run
            out |> Seq.map string |> String.concat "," |> printfn "part1: %s"
    else
        failingTests()
        compiledTests()
        tests()
    0
