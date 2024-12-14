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

let simulate width height seconds (robots: Robot list) =
    robots
    |> List.map (fun robot -> 
        let pX = (robot.pX + robot.vX * seconds) % width
        let pY = (robot.pY + robot.vY * seconds) % height
        {
            // Wrap negative numbers back into the arena.        
            pX = if pX < 0 then pX + width else pX
            pY = if pY < 0 then pY + height else pY
            vX = robot.vX
            vY = robot.vY
        })

let calcSafetyFactor (robots: Robot list) width height =
    let halfWidth = width / 2
    let halfHeight = height / 2
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
let movedRobots = robots |> simulate 101 103 100
printfn "%A" (calcSafetyFactor movedRobots 101 103)

let renderRobots width height (robots: Robot list) =
    let arena = Array2D.create width height '.'
    robots |> List.iter (fun robot ->
        Array2D.set arena robot.pX robot.pY '*'
    )
    for y in 0..height - 1 do
        let line =
            { 0..width - 1}
            |> Seq.map (fun x -> Array2D.get arena x y)
        printfn "%s" (System.String(Seq.toArray line))

let robotsOverlap (robots: Robot list) =
    let rec markArena arena robots =
        match robots with
        | [] -> false
        | robot :: rest ->
            if Set.contains (robot.pX, robot.pY) arena then
                true
            else
                markArena (Set.add (robot.pX, robot.pY) arena) rest
    markArena Set.empty robots

{ 1 .. 10000}
|> Seq.fold (fun robots i -> 
    let robots = simulate 101 103 1 robots
    if not (robotsOverlap robots) then  
        printfn "Seconds: %d" i
        renderRobots 101 103 robots
        printfn ""
    robots) robots
|> ignore