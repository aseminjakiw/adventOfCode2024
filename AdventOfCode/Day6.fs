module AdventOfCode.Day6

open System.Diagnostics
open System.IO
open AdventOfCode.Shared

let filePath = @"Day 6 Input.txt"

type Direction =
    | Up
    | Right
    | Down
    | Left

type InputField =
    | Empty
    | Barrier
    | GuardStart of Direction

type Field =
    | Empty
    | Visited
    | Barrier
    | BarrierVisited of Set<Direction>

type Guard =
    { X: int; Y: int; Direction: Direction }

type BarrierOption = { X: int; Y: int }

let parseField c =
    match c with
    | '.' -> InputField.Empty
    | '#' -> InputField.Barrier
    | '^' -> GuardStart Up
    | _ -> failwith $"unexpected char {c}"

let loadData () =
    let content = File.ReadAllLines filePath
    let width = content[0].Length
    let height = content.Length
    let data = Array2D.create width height Empty
    let mutable guard = { X = 0; Y = 0; Direction = Up }

    for y in 0 .. (height - 1) do
        for x in 0 .. (width - 1) do
            let field = content[y][x] |> parseField

            let value =
                match field with
                | GuardStart direction ->
                    guard <- { X = x; Y = y; Direction = direction }
                    Empty
                | InputField.Empty -> Empty
                | InputField.Barrier -> Barrier

            data[y, x] <- value

    data, guard



let rotateRight direction =
    match direction with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let insideMap data (y, x) =
    let length1 = data |> Array2D.length1
    let length2 = data |> Array2D.length2

    x >= 0 && x < length1 && y >= 0 && y < length2

let hasVisitedBarrierToTheRight (data: Field array2d) guard =
    let searchDirection = guard.Direction |> rotateRight

    let fieldsToCheck =
        match searchDirection with
        | Up -> data[0 .. guard.Y, guard.X] |> Array.rev
        | Right -> data[guard.Y, guard.X ..]
        | Down -> data[guard.Y .., guard.X]
        | Left -> data[guard.Y, 0 .. guard.X] |> Array.rev

    fieldsToCheck
    |> Array.tryPick (fun f ->
        match f with
        | Empty -> None
        | Visited -> None
        | Barrier -> Some false
        | BarrierVisited directions ->
            if directions.Contains searchDirection then
                Some true
            else
                Some false)
    |> Option.contains true

type SimulationResult =
    { Iteration: int
      BarrierOptions: BarrierOption list
      Successful: bool }

let rec simulate iteration data guard barrierOptions =
    let iteration = iteration + 1

    let nextFieldCoordinates =
        match guard.Direction with
        | Up -> (guard.Y - 1, guard.X)
        | Right -> (guard.Y, guard.X + 1)
        | Down -> (guard.Y + 1, guard.X)
        | Left -> (guard.Y, guard.X - 1)

    if not (insideMap data nextFieldCoordinates) then
        { Iteration = iteration
          BarrierOptions = barrierOptions
          Successful = true }
    elif iteration > 10_000 then
        { Iteration = iteration
          BarrierOptions = barrierOptions
          Successful = false }
    else
        let y, x = nextFieldCoordinates
        let nextField = data[y, x]

        match nextField with
        | Empty
        | Visited ->
            let barrierOptions =
                if hasVisitedBarrierToTheRight data guard then
                    { X = x; Y = y } :: barrierOptions
                else
                    barrierOptions

            data[guard.Y, guard.X] <- Visited
            data[y, x] <- Visited
            simulate iteration data { guard with X = x; Y = y } barrierOptions
        | Barrier ->
            data[y, x] <- [ guard.Direction ] |> Set.ofList |> BarrierVisited

            simulate
                iteration
                data
                { guard with
                    Direction = rotateRight guard.Direction }
                barrierOptions
        | BarrierVisited directions ->
            data[y, x] <- guard.Direction |> directions.Add |> BarrierVisited

            simulate
                iteration
                data
                { guard with
                    Direction = rotateRight guard.Direction }
                barrierOptions


let flatten data =
    let length = data |> Array2D.length1

    seq {
        for y in 0 .. (length - 1) do
            yield data[y, *]
    }
    |> Seq.toArray
    |> Array.collect id

type PuzzleResult =
    { SumVisitedFields: int
      FoundPossibleBarriers: int }

let run () =
    let watch = Stopwatch()
    watch.Start()
    let data, guard = loadData ()
    let simulationResult = simulate 0 data guard []
    watch.Stop()
    watch.Elapsed |> dumpIgnore

    // simulationResult |> dumpIgnore
    simulationResult.Successful |> dumpNameIgnore "Simulation success"
    simulationResult.Iteration |> dumpNameIgnore "Simulation iterations"

    { SumVisitedFields = data |> flatten |> Array.filter (fun f -> f = Visited) |> Array.length
      FoundPossibleBarriers = simulationResult.BarrierOptions.Length }
