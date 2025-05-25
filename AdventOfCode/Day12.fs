module AdventOfCode.Day12

open System.IO
open AdventOfCode.Day8

type PlantType = PlantType of char
type Plot = { Pos: Vec2; Plant: PlantType }

let loadFile filePath = File.ReadAllLines filePath

let toPlots content =
    seq {
        for y in 0 .. (Array.length content - 1) do
            let line = content[y]

            for x in 0 .. (String.length line - 1) do
                let char = line[x]

                { Pos = Vec2.NewI (x + 1) (y + 1)
                  Plant = PlantType char }
    }
    |> List.ofSeq

module Part1 =
    let splitPlotByAdjacenty plot plots =
        let searchWest = { plot.Pos with X = plot.Pos.X + 1f }
        let searchEast = { plot.Pos with X = plot.Pos.X - 1f }
        let searchSouth = { plot.Pos with Y = plot.Pos.Y + 1f }
        let searchNorth = { plot.Pos with Y = plot.Pos.Y - 1f }

        plots
        |> List.partition (fun p ->
            p.Pos = searchNorth
            || p.Pos = searchEast
            || p.Pos = searchSouth
            || p.Pos = searchWest)

    let notInField field plot = field |> List.contains plot |> not

    [<TailCall>]
    let rec walkField plantType plots field candidates notMatching =
        match candidates with
        | [] -> field, plots @ notMatching
        | candidate :: remainingCandidates ->
            if candidate.Plant = plantType && notInField field candidate then
                let adjacentPlots, remainingPlots = splitPlotByAdjacenty candidate plots
                let newField = candidate :: field
                let newCombinedCandidates = adjacentPlots @ remainingCandidates
                walkField plantType remainingPlots newField newCombinedCandidates notMatching
            else
                let newNotMatching = candidate :: notMatching
                walkField plantType plots field remainingCandidates newNotMatching

    [<TailCall>]
    let rec createFields plots fields =
        match plots with
        | [] -> fields
        | plot :: tail ->
            let field, remainingPlots = walkField plot.Plant tail [] [ plot ] []
            let newFields = field :: fields
            createFields remainingPlots newFields

    let getAdjacentPlots plot plots =
        let searchWest = { plot.Pos with X = plot.Pos.X + 1f }
        let searchEast = { plot.Pos with X = plot.Pos.X - 1f }
        let searchSouth = { plot.Pos with Y = plot.Pos.Y + 1f }
        let searchNorth = { plot.Pos with Y = plot.Pos.Y - 1f }

        let tryFind search =
            plots |> List.tryFind (fun p -> p.Pos = search)

        [ tryFind searchNorth
          tryFind searchEast
          tryFind searchSouth
          tryFind searchWest ]

    let calcPrice field =
        let perimeter =
            field
            |> List.map (fun p -> getAdjacentPlots p field |> List.filter Option.isNone |> List.length)
            |> List.sum

        let area = List.length field
        perimeter * area

    let run () =
        let plots =
            @"D:\Projekte\AdventOfCode\AdventOfCode\Day 12 Input.txt" |> loadFile |> toPlots

        let fields = createFields plots []
        let prices = fields |> List.map calcPrice
        let total = List.sum prices

        total

module Part2 =
    let run () = -1
