module AdventOfCode.Day12

open System.IO
open AdventOfCode.Day8
open AdventOfCode.Shared

type Plant = char

type MapPosition =
    { Position: Vec2
      Plant: Plant }

    override this.ToString() = $"{this.Plant}@{this.Position}"

let filePath = @"Day 12 Input.txt"

let loadData () =
    let content = File.ReadAllLines filePath
    let width = content[0].Length
    let height = content.Length

    seq {
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let localHeight = content[y][x]

                yield
                    { Position = Vec2.NewI (x + 1) (y + 1)
                      Plant = localHeight }
    }
    |> Seq.toList


module Part1 =
    type BorderType =
        | Outer
        | SamePlant
        | DifferentPlant

    type BorderSide =
        | North
        | East
        | South
        | West

    type Border = { Type: BorderType; Side: BorderSide }

    type Plot =
        { Position: MapPosition
          PlotId: int
          Borders: Set<Border> }
        
    let rec toPlots mapInputs  plots=
        match mapInputs with
        | [] -> plots
        | head :: tail ->
            
            

    let run () =
        let input = loadData ()

        input |> dumpIgnore
        -1

module Part2 =
    let run () = -1
