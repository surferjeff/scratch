open System.Collections.Generic
let input = [| 0UL; 37551UL; 469UL; 63UL; 1UL; 791606UL; 2065UL; 9983586UL |]

// Maps the number engraved on the stone to the number of times it appears in
// the line.
// Switching from an F# Map to a .NET Dictionary reduced the runtime from
// 300 ms to 170 ms.
type StoneMap = Dictionary<uint64,uint64>

let addStone stone count (stones: StoneMap) =
    match stones.TryGetValue(stone) with
    | true, vcount -> stones[stone] <- vcount + count
    | false, _ -> stones[stone] <- count
    stones

let blink (stones: StoneMap) _ =
    stones
    |> Seq.map (fun kv -> (kv.Key, kv.Value))
    |> Seq.fold (fun newStones (stone, count) ->
        if stone = 0UL then
            newStones |> addStone 1UL count
        else
            let text = (string stone)
            if text.Length % 2 = 0 then
                let halfLength = text.Length / 2
                newStones
                |> addStone (uint64 (text.Substring(0, halfLength))) count
                |> addStone (uint64 (text.Substring(halfLength))) count
            else
                newStones |> addStone (stone * 2024UL) count
    ) (StoneMap(stones.Count * 2))

let part1 =
    let stones = input |> Seq.map (fun n -> KeyValuePair(n, 1UL)) |> StoneMap
    Seq.fold blink stones { 1..25 }

printfn "part1: %d" (part1.Values |> Seq.sum)

let part2 = Seq.fold blink part1 { 1..50 }

printfn "part2: %d" (part2.Values |> Seq.sum)