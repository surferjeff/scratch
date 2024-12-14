open System.IO
open System.Text.RegularExpressions

[<Struct>]
type Robot = {
    // Position
    pX: int; pY: int
    // Velocity
    vX: int; vY: int
}

let parseInput path =
    let text = File.ReadAllText(path)
    let rx = Regex("p=(?<pX>-?\\d+),(?<pY>-?\\d+) v=(?<vX>-?\\d+),(?<vY>-?\\d+)")
    rx.Matches(text)
    |> Seq.map (fun m -> {
        pX = int m.Groups["pX"].Value
        pY = int m.Groups["pY"].Value
        vX = int m.Groups["vX"].Value
        vY = int m.Groups["vY"].Value
    })
    |> Seq.toList

let simulate xLength yLength (robots: Robot list) _ =
    robots
    |> List.map (fun robot -> 
        let pX = (robot.pX + robot.vX) % xLength
        let pY = (robot.pY + robot.vY) % yLength
        {
            // Wrap negative numbers back into the arena.        
            pX = if pX < 0 then pX + xLength else pX
            pY = if pY < 0 then pY + yLength else pY
            vX = robot.vX
            vY = robot.vY
        })

let calcSafetyFactor (robots: Robot list) xLength yLength =
    let halfWidth = xLength / 2
    let halfHeight = yLength / 2
    let q1, q2, q3, q4 =
        robots
        |> List.fold (fun (q1, q2, q3, q4) robot ->
            match (compare robot.pX halfWidth), (compare robot.pY halfHeight) with
                | -1, -1 -> (q1 + 1), q2, q3, q4
                | -1, 1 -> q1, (q2 + 1), q3, q4
                | 1, -1 -> q1, q2, (q3 + 1), q4
                | 1, 1 -> q1, q2, q3, (q4 + 1)
                | _ -> q1, q2, q3, q4
        ) (0, 0, 0, 0)
    q1 * q2 * q3 * q4


let robots = parseInput "input.txt"
printfn "%d robots" (List.length robots)

let movedRobots = { 1..100 } |> Seq.fold (simulate 101 103) robots
printfn "%A" (calcSafetyFactor movedRobots 11 7)

