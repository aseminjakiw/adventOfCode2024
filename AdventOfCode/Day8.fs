module AdventOfCode.Day8

open System
open System.IO
open AdventOfCode.Shared

let filePath = @"Day 8 Input.txt"


[<StructuredFormatDisplay("{Display}")>]
type Vec2 =
    { Y: single
      X: single }

    member this.Display = this.ToString()

    override this.ToString() = $"({this.Y}|{this.X})"


module Vec2 =
    let New x y = { X = x; Y = y }
    let NewI (x: int) (y: int) = { X = single x; Y = single y }

    let length (vec: Vec2) =
        vec.X * vec.X + vec.Y * vec.Y |> MathF.Sqrt

    let addV a b = New (a.X + b.X) (a.Y + b.Y)
    let addS vec scalar = New (vec.X + scalar) (vec.Y + scalar)

    let subtract a b = New (b.X - a.X) (b.Y - a.Y)
    let inverse vec = New -vec.X -vec.Y


[<StructuredFormatDisplay("{Frequency}@{Position}")>]
type Antenna = { Position: Vec2; Frequency: char }

type FieldBoundary = { Width: int; Height: int }

type Data =
    { Boundary: FieldBoundary
      Antennas: Antenna list }

let parseField c =
    match c with
    | '.' -> None
    | _ -> c |> Some

let loadData () =
    let content = File.ReadAllLines filePath
    let width = content[0].Length
    let height = content.Length

    let mutable antennas = []

    for y in 0 .. (height - 1) do
        for x in 0 .. (width - 1) do
            let field = content[y][x] |> parseField

            match field with
            | None -> ()
            | Some value ->
                let x = x + 1
                let y = y + 1

                antennas <-
                    { Position = Vec2.NewI x y
                      Frequency = value }
                    :: antennas

    { Boundary = { Width = width; Height = height }
      Antennas = antennas }

module Part1 =
    let getAntinodes (a, b) =
        let a = a.Position
        let b = b.Position
        let connectionVec = Vec2.subtract a b
        let antinodeA = connectionVec |> Vec2.inverse |> Vec2.addV a
        let antinodeB = connectionVec |> Vec2.addV b
        [ antinodeA; antinodeB ]

    let isInsideField boundary vec =
        let width = single boundary.Width
        let height = single boundary.Height

        vec.X >= 1f && vec.X <= width && vec.Y >= 1f && vec.Y <= height

    let isIntegerPos vec =
        (vec.X |> int32 |> single = vec.X) && (vec.Y |> int32 |> single = vec.Y)


    let run () =
        let data = loadData ()

        data.Antennas
        |> List.groupBy _.Frequency
        |> List.collect (fun (_, antennas) ->
            let antennaCombos =
                seq {
                    for a in antennas do
                        for b in antennas do
                            yield (a, b)
                }
                |> Seq.toList
                |> List.filter (fun (a, b) -> a <> b)
                |> List.map (fun (a, b) ->
                    match (a.Position.X - b.Position.X, a.Position.Y - b.Position.Y) with
                    | _, dy when dy > 0f -> (b, a)
                    | dx, _ when dx > 0f -> (b, a)
                    | _, _ -> (a, b))
                |> List.sort
                |> List.distinct

            antennaCombos)
        |> List.collect getAntinodes
        |> List.filter isIntegerPos
        |> List.filter (isInsideField data.Boundary)
        |> List.distinct
        |> List.sort
        |> List.length

module Part2 =
    let rec extendAntinodes boundary connectionVec lastAntinode =
        let antinode = connectionVec |> Vec2.addV lastAntinode

        if Part1.isInsideField boundary antinode then
            antinode :: extendAntinodes boundary connectionVec antinode
        else
            []

    let getAntinodes data (a, b) =
        let a = a.Position
        let b = b.Position
        let connectionVec = Vec2.subtract a b

        let connectionVecInverse = connectionVec |> Vec2.inverse

        let antinodesA = extendAntinodes data connectionVecInverse a
        let antinodesB = extendAntinodes data connectionVec b

        antinodesA @ antinodesB


    let run () =
        let data = loadData ()

        data.Antennas
        |> dumpName "Antennas"
        |> List.groupBy _.Frequency
        |> List.collect (fun (_, antennas) ->
            let antennaCombos =
                seq {
                    for a in antennas do
                        for b in antennas do
                            yield (a, b)
                }
                |> Seq.toList
                |> List.filter (fun (a, b) -> a <> b)
                |> List.map (fun (a, b) ->
                    match (a.Position.X - b.Position.X, a.Position.Y - b.Position.Y) with
                    | _, dy when dy > 0f -> (b, a)
                    | dx, _ when dx > 0f -> (b, a)
                    | _, _ -> (a, b))
                |> List.sort
                |> List.distinct

            antennaCombos)
        |> dumpName "Antenna combos"
        |> List.collect (getAntinodes data.Boundary)
        |> dumpName "Antinodes raw"
        |> List.filter Part1.isIntegerPos
        |> List.filter (Part1.isInsideField data.Boundary)
        |> List.append (data.Antennas |> List.map _.Position)
        |> List.distinct
        |> List.sort
        |> dumpName "Antinodes"
        |> List.length
