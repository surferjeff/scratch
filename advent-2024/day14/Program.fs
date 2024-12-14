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

let simulate (robots: Robot list) xLength yLength seconds =
    robots
    |> List.map (fun robot -> {
        pX = (robot.pX + seconds * robot.vX) % xLength
        pY = (robot.pY + seconds * robot.vY) % yLength
        vX = robot.vX
        vY = robot.vY
    })
    // Wrap negative numbers back into the arena.
    |> List.map (fun robot -> {
        pX = if robot.pX < 0 then robot.pX + xLength else robot.pX
        pY = if robot.pY < 0 then robot.pY + yLength else robot.pY
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


let robots = parseInput "test1.txt"

// let robo10 = simulate robots 11 7 10
// let robo1010 = simulate robo10 11 7 10
// let robo20 = simulate robots 11 7 20

// assert (robo1010 = robo20)


let movedRobots = simulate robots 11 7 100
printfn "%A" movedRobots
printfn "%A" (calcSafetyFactor movedRobots 11 7)

