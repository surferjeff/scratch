﻿open System.IO
open System.Text.RegularExpressions
open System.Reflection
open System.Reflection.Emit
open System

type Reg = int64
let reg n = int64 n
[<Struct>]
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
    let typeBuilder = moduleBuilder.DefineType("DynamicClass", TypeAttributes.Public, typeof<Machine>)
    let method = typeBuilder.DefineMethod("run", MethodAttributes.Public, typeof<System.Void>, [| |])
    let gen = method.GetILGenerator(program.Length * 32)
    let labels = { 0..20 } |> Seq.map (fun _ -> gen.DefineLabel() ) |> Seq.toArray
    let mutable ilabel = 0

    // Move all the class members into locals for better performance.
    let fieldFlags = BindingFlags.NonPublic ||| BindingFlags.Instance;
    let fieldA = typeof<Machine>.GetField("A", fieldFlags)
    let fieldB = typeof<Machine>.GetField("B", fieldFlags)
    let fieldC = typeof<Machine>.GetField("C", fieldFlags)
    let fieldOutBuf = typeof<Machine>.GetField("outBuf", fieldFlags)
    let fieldOutI = typeof<Machine>.GetField("outI", fieldFlags)

    let A = gen.DeclareLocal(typeof<int64>);
    let B = gen.DeclareLocal(typeof<int64>);
    let C = gen.DeclareLocal(typeof<int64>);
    let outI = gen.DeclareLocal(typeof<int>);
    let outBuf = gen.DeclareLocal(typeof<int array>);

    gen.Emit(OpCodes.Ldarg_0)
    gen.Emit(OpCodes.Ldfld, fieldA)
    gen.Emit(OpCodes.Stloc, A);

    gen.Emit(OpCodes.Ldarg_0)
    gen.Emit(OpCodes.Ldfld, fieldB)
    gen.Emit(OpCodes.Stloc, B);

    gen.Emit(OpCodes.Ldarg_0)
    gen.Emit(OpCodes.Ldfld, fieldC)
    gen.Emit(OpCodes.Stloc, C);

    gen.Emit(OpCodes.Ldarg_0)
    gen.Emit(OpCodes.Ldfld, fieldOutBuf)
    gen.Emit(OpCodes.Stloc, outBuf);

    gen.Emit(OpCodes.Ldarg_0)
    gen.Emit(OpCodes.Ldfld, fieldOutI)
    gen.Emit(OpCodes.Stloc, outI);


    // let emitCombo (operand: int) =
    //     match operand with
    //     | 0 | 1 | 2 | 3 -> gen.Emit(OpCodes.Ldc_I8, operand)
    //     | 4 ->
    //         gen.Emit(OpCodes.Ldarg_0)
    //         gen.Emit(OpCodes.Ldfld, fieldA)
    //     | 5 ->
    //         gen.Emit(OpCodes.Ldarg_0)
    //         gen.Emit(OpCodes.Ldfld, fieldB)
    //     | 6 ->
    //         gen.Emit(OpCodes.Ldarg_0)
    //         gen.Emit(OpCodes.Ldfld, fieldC)
    //     | bad -> failwithf "Invalid combo operand in %d" operand

    // let emitADivCombo operand =
    //     gen.Emit(OpCodes.Ldarg_0)
    //     gen.Emit(OpCodes.Ldfld, fieldA)
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
    //         gen.Emit(OpCodes.Ldarg_0)
    //         emitADivCombo operand
    //         gen.Emit(OpCodes.Stfld, fieldA)
    //     | 1 ->
    //         gen.Emit(OpCodes.Ldarg_0)
    //         gen.Emit(OpCodes.Ldarg_0)
    //         gen.Emit(OpCodes.Ldfld, fieldB)
    //         gen.Emit(OpCodes.Ldc_I8, operand)
    //         gen.Emit(OpCodes.Xor)
    //         gen.Emit(OpCodes.Stfld, fieldB)
    //     | 2 ->
    //         gen.Emit(OpCodes.Ldarg_0)
    //         emitCombo operand
    //         gen.Emit(OpCodes.Ldc_I8, 0x0111)
    //         gen.Emit(OpCodes.And)
    //         gen.Emit(OpCodes.Stfld, fieldB)            
    //     | 3 ->
    //         emitLoadA()
    //         let skip = gen.DefineLabel()
    //         gen.Emit(OpCodes.Ldc_I8, 0)
    //         gen.Emit(OpCodes.Beq, skip)
    //         gen.Emit(OpCodes.Br, labels[operand / 2])
    //         gen.MarkLabel(skip)            
    //     | 4 ->
    //         emitBeginStoreB()
    //         emitLoadB()
    //         emitLoadC()
    //         gen.Emit(OpCodes.Xor)
    //         emitCommitStoreRegister()
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
    //         emitBeginStoreB()
    //         emitADivCombo gen operand
    //         emitCommitStoreRegister()
    //     | 7 -> 
    //         emitBeginStoreC()
    //         emitADivCombo gen operand
    //         emitCommitStoreRegister()
    //     | bad -> failwithf "Bad op code %d" program[i]
    // gen.Emit(OpCodes.Ldloc, iout)

    // Move all the local variables back into fields.
    gen.Emit(OpCodes.Ldarg_0)
    gen.Emit(OpCodes.Ldloc, A);
    gen.Emit(OpCodes.Stfld, fieldA)

    gen.Emit(OpCodes.Ldarg_0)
    gen.Emit(OpCodes.Ldloc, B);
    gen.Emit(OpCodes.Stfld, fieldB)

    gen.Emit(OpCodes.Ldarg_0)
    gen.Emit(OpCodes.Ldloc, C);
    gen.Emit(OpCodes.Stfld, fieldC)

    gen.Emit(OpCodes.Ldarg_0)
    gen.Emit(OpCodes.Ldloc, outI);
    gen.Emit(OpCodes.Stfld, fieldOutI)

    gen.Emit(OpCodes.Ret)
    
    let newType = typeBuilder.CreateType()
    fun (regs: Registers) ->
        let instance = Activator.CreateInstance()
        method.Invoke(instance, null)

let part2() =
    let regs, program = parseInput "input.txt"
    let mutable found = false
    let mutable regs = { regs with A = 0 }
    let mutable nextPrint = 1_000_000
    while not found do
        regs <- { regs with A = regs.A + 1L }
        let _, out = runMachine (regs, program)
        found <- out = program
        if regs.A = nextPrint then
            printfn "%d" nextPrint
            nextPrint <- nextPrint + 1_000_000
    printfn "%A" regs

let tests() =
    let regs, out = runMachine ({ zeros with C = 9}, [|2; 6|])
    assert(regs.B = 1)

    let regs, out = runMachine ({ zeros with A = 10}, [|5; 0; 5; 1; 5; 4|])
    assert(out = [|0; 1; 2|])

    let regs, out = runMachine ({ zeros with A = 2024}, [|0;1;5;4;3;0|])
    assert(out = [|4;2;5;6;7;7;7;7;3;1;0|])
    assert(regs.A = 0)

    let regs, out = runMachine ({ zeros with B = 29}, [|1; 7|])
    assert(regs.B = 26)

    let regs, out = runMachine ({ zeros with B = 2024; C = 43690}, [|4; 0|])
    assert(regs.B = 44354)



[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        if argv[0] = "part2" then
            part2()
        else
            let _, out = parseInput argv[0] |> runMachine
            out |> Seq.map string |> String.concat "," |> printfn "part1: %s"
    else
        // compiledTests()
        tests()
    0
