
let input = [| 0UL; 37551UL; 469UL; 63UL; 1UL; 791606UL; 2065UL; 9983586UL |]

// Maps the number engraved on the stone to the number of times it appears in
// the line.
type StoneMap = Map<uint64,uint64>

let addStone (stone: uint64) (count: uint64) (stones: StoneMap) =
    stones |> Map.change stone (function
    | None -> Some count
    | Some n -> Some (n + count))

let blink (stones: StoneMap) _ =
    stones
    |> Map.toSeq
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
    ) Map.empty

let part1 =
    let stones = input |> Seq.map (fun n -> (n, 1UL)) |> Map
    Seq.fold blink stones { 1..25 }

printfn "part1: %d" (part1 |> Map.values |> Seq.sum)

let part2 = Seq.fold blink part1 { 1..50 }

printfn "part2: %d" (part2 |> Map.values |> Seq.sum)