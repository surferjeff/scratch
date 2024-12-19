open System.IO
open System.Text.RegularExpressions
open System.Reflection
open System.Reflection.Emit
open System

type Reg = int64
let reg n = int64 n

type Registers = {
    mutable A: Reg
    mutable B: Reg
    mutable C: Reg
}

type Machine(regs: Registers) =
    let mutable A = regs.A
    let mutable B = regs.B
    let mutable C = regs.C

    let outBuf = Array.create 20 0
    let mutable outI = 0

    member this.combo(operand: int) =
        match operand with
        | 0 | 1 | 2 | 3 -> operand
        | 4 -> int32 A
        | 5 -> int32 B
        | 6 -> int32 C
        | bad -> failwithf "Invalid combo operand in %d" operand
    
    member this.op0adv operand = A <- A / (1L <<< this.combo(operand)); -2
    member this.op1bxl operand = B <- B ^^^ operand; -2
    member this.op2bst operand = B <- this.combo(operand) &&& 0b0111; -2
    member this.op3jnz operand = if A = 0 then -2 else operand
    member this.op4bxc operand = B <- B ^^^ C; -2
    member this.op5out operand =
        outBuf[outI] <- this.combo(operand) &&& 0b0111
        outI <- outI + 1
        -2
    member this.op6bdv operand = B <- A / (1L <<< this.combo(operand)); -2
    member this.op7cdv operand = C <- A / (1L <<< this.combo(operand)); -2

    member this.ops = [| this.op0adv; this.op1bxl; this.op2bst; this.op3jnz;
        this.op4bxc; this.op5out; this.op6bdv; this.op7cdv |]

    member this.result() =
        { A = A; B = B; C = C }, outBuf[0..outI-1]
        

let runMachine (regs: Registers, program: int array) =
    let machine = Machine(regs)
    let mutable ip = 0
    while ip < program.Length do
        let jmp = machine.ops[program[ip]] program[ip+1]
        if jmp < 0 then
            ip <- ip - jmp
        else
            ip <- jmp
    machine.result()

let zeros = { A = 0; B = 0; C = 0 }

let parseInput path =
    let rx = Regex("(Register (?<regname>A|B|C): (?<regvalue>\\d+))"
        + "|(?<op>\\d+)")
    let mutable regs = zeros
    let program = ResizeArray<int>()
    for m in File.ReadAllText path |> rx.Matches do
        if m.Groups["regname"].Success then
            let regValue = reg m.Groups["regvalue"].Value
            match m.Groups["regname"].Value with
            | "A" -> regs <- { regs with A = regValue }
            | "B" -> regs <- { regs with B = regValue }
            | "C" -> regs <- { regs with C = regValue }
            | bad -> failwithf "Bad register name %s" bad
        else
            int m.Groups["op"].Value |> program.Add        

    regs, program.ToArray()

let compile (program: int array) (loopCount: int64) =
    let assemblyName = new AssemblyName("Compiler")
    let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule("Compiler")
    let args = [| typeof<int array>; typeof<Registers>; |]
    let method = DynamicMethod("run", typeof<int>, args, moduleBuilder)
    let gen = method.GetILGenerator(program.Length * 32 + 100)

    let fieldFlags = BindingFlags.Public ||| BindingFlags.Instance;
    let fieldA = typeof<Registers>.GetField("A@", fieldFlags)
    let fieldB = typeof<Registers>.GetField("B@", fieldFlags)
    let fieldC = typeof<Registers>.GetField("C@", fieldFlags)

    // Local variables.
    let locA = gen.DeclareLocal(typeof<int64>);
    let locB = gen.DeclareLocal(typeof<int64>);
    let locC = gen.DeclareLocal(typeof<int64>);
    // 3: I, index into outBuf.
    let locI = gen.DeclareLocal(typeof<int32>);

    let emitLoadA() = gen.Emit(OpCodes.Ldloc_0)
    let emitLoadB() = gen.Emit(OpCodes.Ldloc_1)
    let emitLoadC() = gen.Emit(OpCodes.Ldloc_2)
    let emitLoadI() = gen.Emit(OpCodes.Ldloc_3)

    let emitStoreA() = gen.Emit(OpCodes.Stloc_0)
    let emitStoreB() = gen.Emit(OpCodes.Stloc_1)
    let emitStoreC() = gen.Emit(OpCodes.Stloc_2)
    let emitStoreI() = gen.Emit(OpCodes.Stloc_3)

    // Initialize local variables.
    gen.Emit(OpCodes.Ldarg_1)
    gen.Emit(OpCodes.Ldfld, fieldA)
    emitStoreA()

    gen.Emit(OpCodes.Ldarg_1)
    gen.Emit(OpCodes.Ldfld, fieldB)
    emitStoreB()

    gen.Emit(OpCodes.Ldarg_1)
    gen.Emit(OpCodes.Ldfld, fieldC)
    emitStoreC()

    gen.Emit(OpCodes.Ldc_I4_0)
    emitStoreI()

    // Store copies of A, B and C so we can reload them when we loop.
    let copyA = gen.DeclareLocal(typeof<int64>)
    emitLoadA()
    gen.Emit(OpCodes.Stloc, copyA)
    let loopLimit = gen.DeclareLocal(typeof<int64>)
    emitLoadA()
    gen.Emit(OpCodes.Ldc_I8, loopCount)
    gen.Emit(OpCodes.Add)
    gen.Emit(OpCodes.Stloc, loopLimit)

    let copyB = gen.DeclareLocal(typeof<int64>)
    emitLoadB()
    gen.Emit(OpCodes.Stloc, copyB)

    let copyC = gen.DeclareLocal(typeof<int64>)
    emitLoadC()
    gen.Emit(OpCodes.Stloc, copyC)

    // Helper functions.
    let emitCombo (operand: int) =
        match operand with
        | 0 | 1 | 2 | 3 -> gen.Emit(OpCodes.Ldc_I8, int64 operand)
        | 4 -> emitLoadA()
        | 5 -> emitLoadB()
        | 6 -> emitLoadC()
        | bad -> failwithf "Invalid combo operand in %d" operand

    let emitADivCombo operand =
        emitLoadA()
        gen.Emit(OpCodes.Ldc_I8, 1L)
        emitCombo operand
        gen.Emit(OpCodes.Conv_I4)
        gen.Emit(OpCodes.Shl)
        gen.Emit(OpCodes.Div)

    let labelReset = gen.DefineLabel()
    let labels = { 0..20 } |> Seq.map (fun _ -> gen.DefineLabel() ) |> Seq.toArray
    let mutable ilabel = 0

    for i in 0..2..program.Length-1 do
        let operand = program[i+1]
        if ilabel < labels.Length then
            gen.MarkLabel(labels[ilabel])
            ilabel <- ilabel + 1
        match program[i] with
        | 0 ->
            emitADivCombo operand
            emitStoreA()
        | 1 ->
            emitLoadB()
            gen.Emit(OpCodes.Ldc_I8, int64 operand)
            gen.Emit(OpCodes.Xor)
            emitStoreB()
        | 2 ->
            emitCombo operand
            gen.Emit(OpCodes.Ldc_I8, 0b0111L)
            gen.Emit(OpCodes.And)
            emitStoreB()
        | 3 ->
            emitLoadA()
            let skip = gen.DefineLabel()
            gen.Emit(OpCodes.Ldc_I8, 0L)
            gen.Emit(OpCodes.Beq, skip)
            gen.Emit(OpCodes.Br, labels[operand / 2])
            gen.MarkLabel(skip)
        | 4 ->
            emitLoadB()
            emitLoadC()
            gen.Emit(OpCodes.Xor)
            emitStoreB()
        | 5 ->  // Output
            if loopCount > 0 then
                // Has I exceeded the length of the array?
                emitLoadI()
                gen.Emit(OpCodes.Ldarg_0)
                gen.Emit(OpCodes.Ldlen)
                gen.Emit(OpCodes.Bge, labelReset)

                // Compare output to the array.
                emitCombo operand
                gen.Emit(OpCodes.Ldc_I8, 0b0111L)
                gen.Emit(OpCodes.And)
                gen.Emit(OpCodes.Conv_I4)

                gen.Emit(OpCodes.Ldarg_0)
                emitLoadI()
                gen.Emit(OpCodes.Ldelem_I4)

                // Next iteration of the loop if not equal.
                gen.Emit(OpCodes.Sub)
                gen.Emit(OpCodes.Brtrue, labelReset)

            else
                // Write output
                gen.Emit(OpCodes.Ldarg_0)
                emitLoadI()
                gen.Emit(OpCodes.Conv_I8)
                emitCombo operand
                gen.Emit(OpCodes.Ldc_I8, 0b0111L)
                gen.Emit(OpCodes.And)
                gen.Emit(OpCodes.Conv_I4)
                gen.Emit(OpCodes.Stelem_I4)

            // Increment I
            emitLoadI()
            gen.Emit(OpCodes.Ldc_I4_1)
            gen.Emit(OpCodes.Add)
            emitStoreI()

        | 6 -> 
            emitADivCombo operand
            emitStoreB()
        | 7 -> 
            emitADivCombo operand
            emitStoreC()

        | bad -> failwithf "Bad op code %d" program[i]

    if loopCount > 0 then
        // Compare I to the length of the array.
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Ldlen)
        emitLoadI()
        gen.Emit(OpCodes.Sub)
        gen.Emit(OpCodes.Brtrue, labelReset)

        // Lengths match, program matches!
        // Store the A that generated the program.
        gen.Emit(OpCodes.Ldloc, copyA)
        emitStoreA()

    let labelReturn = gen.DefineLabel()
    gen.MarkLabel(labelReturn)

    // Move all the local variables back into fields.
    gen.Emit(OpCodes.Ldarg_1)
    emitLoadA()
    gen.Emit(OpCodes.Stfld, fieldA)

    gen.Emit(OpCodes.Ldarg_1)
    emitLoadB()
    gen.Emit(OpCodes.Stfld, fieldB)

    gen.Emit(OpCodes.Ldarg_1)
    emitLoadC()
    gen.Emit(OpCodes.Stfld, fieldC)

    // Return I
    emitLoadI()
    gen.Emit(OpCodes.Ret)

    /////////////////////////////////////////////////////////
    // Reset loop
    gen.MarkLabel(labelReset)

    // Restore A, B, C from copies.  Increment A.
    gen.Emit(OpCodes.Ldloc, copyA)
    gen.Emit(OpCodes.Ldc_I8, 1L)
    gen.Emit(OpCodes.Add)
    emitStoreA()
    emitLoadA()
    gen.Emit(OpCodes.Stloc, copyA)

    gen.Emit(OpCodes.Ldloc, copyB)
    emitStoreB()

    gen.Emit(OpCodes.Ldloc, copyC)
    emitStoreC()

    // Reset I
    gen.Emit(OpCodes.Ldc_I4_0)
    emitStoreI()

    emitLoadA()
    gen.Emit(OpCodes.Ldloc, loopLimit)
    gen.Emit(OpCodes.Beq, labelReturn)

    gen.Emit(OpCodes.Br, labels[0])

    method    

let part2 path =
    let mutable regs, program = parseInput path
    let loopCount = 200_000_000L
    let compiled = compile program loopCount
    let args = [| box program; regs|]
    let mutable found = false
    while not found do
        let regsArray =
            {0L..9L}
            |> Seq.map (fun i -> { regs with A = regs.A + loopCount * i})
            |> Seq.toArray
        let outLengths = 
            regsArray
            |> Array.Parallel.map (fun regs ->
                let args = [| box program; regs|]
                int (unbox (compiled.Invoke(null, args))))
        for i in 0..outLengths.Length-1 do
            if outLengths[i] = program.Length then
                printfn "part2: %d" regsArray[i].A
                found <- true
        if not found then
            regs <- regsArray[regsArray.Length-1]
            printfn "%d" regs.A

let runCompiled (regs: Registers, program: int array) =
    let compiled = compile program 0
    let outBuffer = Array.create 100 0
    let args  = [| box outBuffer; regs |]
    let outLen = compiled.Invoke(null, args)
    let outLen = int (unbox outLen)
    regs, outBuffer[0..(outLen - 1)]

let tests() =
    let regs, out = runCompiled ({ zeros with C = 9}, [|2; 6|])
    assert(regs.B = 1)

    let regs, out = runCompiled ({ zeros with B = 29}, [|1; 7|])
    assert(regs.B = 26)

    let regs, out = runCompiled ({ zeros with B = 2024; C = 43690}, [|4; 0|])
    assert(regs.B = 44354)

    let regs, out = runCompiled ({ zeros with A = 2024}, [|0;1;5;4;3;0|])
    assert(out = [|4;2;5;6;7;7;7;7;3;1;0|])
    assert(regs.A = 0)

    let regs, out = runCompiled ({ zeros with A = 10}, [|5; 0; 5; 1; 5; 4 |])
    assert(out = [|0; 1; 2|])

let failingTests() = ()


[<EntryPoint>]
let main args =
    let argv = args |> Array.filter ((<>) "-part2")
    let p2 = argv.Length < args.Length
    if argv.Length > 0 then
        if p2 then
            // runCompiled is more than 100 times faster than than runMachine,
            // but it's still too slow to find the answer.
            // Also, the loop doesn't properly terminate on the right answer.
            part2 argv[0]
        else
            let _, out = parseInput argv[0] |> runCompiled
            out |> Seq.map string |> String.concat "," |> printfn "part1: %s"
    else
        failingTests()
        tests()
    0
