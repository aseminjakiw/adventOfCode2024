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

[<StructuredFormatDisplay("{Frequency}@{Position}")>]
type Antenna = { Position: Vec2; Frequency: char }

type Data =
    { Width: int
      Height: int
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

    { Width = width
      Height = height
      Antennas = antennas }

module Part1 =
    let 
    
    let run () =
        let data = loadData ()

        data.Antennas
        |> List.groupBy _.Frequency
        |> List.map (fun (freq, antennas) ->
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
        |> dump
        |> List.map (fun _ -> -1)
        |> List.sum

module Part2 =
    let run () = -1
