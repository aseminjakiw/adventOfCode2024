module AdventOfCode.Day10

open System
open System.IO
open AdventOfCode.Day8
open AdventOfCode.Shared

type MapPosition =
    { Position: Vec2
      Height: byte }

    override this.ToString() = $"{this.Height}@{this.Position}"

let filePath = @"Day 10 Input.txt"

let parseChar (c: char) = c.ToString() |> Byte.Parse

let loadData () =
    let content = File.ReadAllLines filePath
    let width = content[0].Length
    let height = content.Length

    seq {
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let localHeight = content[y][x] |> parseChar

                yield
                    { Position = Vec2.NewI (x + 1) (y + 1)
                      Height = localHeight }
    }
    |> Seq.toList


module Part1 =
    let getNeighbors data position =
        let xCenter = position.X
        let yCenter = position.Y

        data
        |> List.filter (fun p ->
            match p.Position with
            | { Y = y; X = x } when x = xCenter - 1f && y = yCenter -> true
            | { Y = y; X = x } when x = xCenter && y = yCenter - 1f -> true
            | { Y = y; X = x } when x = xCenter + 1f && y = yCenter -> true
            | { Y = y; X = x } when x = xCenter && y = yCenter + 1f -> true
            | _ -> false)

    let rec followTrail data pointsToCheck visited =
        match pointsToCheck with
        | [] -> visited
        | head :: tail ->
            let visited = head :: visited

            let validNeighbors =
                head.Position
                |> getNeighbors data
                |> List.filter (fun neighbor -> neighbor.Height - 1uy = head.Height)

            let newPoints = validNeighbors @ tail
            followTrail data newPoints visited


    let getTrailHeadScore data head =
        let trails = followTrail data [ head ] [] |> List.distinct

        let score = trails |> List.filter (fun point -> point.Height = 9uy) |> List.length
        (head, score)

    let run () =
        let data = loadData ()

        data
        |> List.filter (fun p -> p.Height = 0uy)
        // |> dumpName "possible trail heads"
        |> List.map (getTrailHeadScore data)
        // |> dumpName "Trailhead scores"
        |> List.map snd
        |> List.sum

module Part2 =
    let getTrailHeadScore data head =
        let trails = Part1.followTrail data [ head ] []

        let score = trails |> List.filter (fun point -> point.Height = 9uy) |> List.length
        (head, score)
    
    let run () = 
        let data = loadData ()

        data
        |> List.filter (fun p -> p.Height = 0uy)
        // |> dumpName "possible trail heads"
        |> List.map (getTrailHeadScore data)
        // |> dumpName "Trailhead scores"
        |> List.map snd
        |> List.sum
