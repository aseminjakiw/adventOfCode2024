module AdventOfCode.Day6

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

type Guard =
    { X: int; Y: int; Direction: Direction }


type Field =
    | Empty
    | Visited of Set<Direction>
    | Barrier

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
                    direction |> Set.singleton |> Visited
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

type SimulationStopReason =
    | MaxIterationsReached
    | MapLeft
    | Looping

type SimulationResult =
    { Iteration: int
      StopReason: SimulationStopReason }

let nextFieldCoordinates guard =
    match guard.Direction with
    | Up -> (guard.Y - 1, guard.X)
    | Right -> (guard.Y, guard.X + 1)
    | Down -> (guard.Y + 1, guard.X)
    | Left -> (guard.Y, guard.X - 1)


let rec simulate iteration data guard onIterationContinue =
    let iteration = iteration + 1

    let nextFieldCoordinates = nextFieldCoordinates guard

    if not (insideMap data nextFieldCoordinates) then
        { Iteration = iteration
          StopReason = MapLeft }
    elif iteration > 10_000 then
        { Iteration = iteration
          StopReason = MaxIterationsReached }
    else
        onIterationContinue data guard

        let y, x = nextFieldCoordinates
        let nextField = data[y, x]

        match nextField with
        | Empty ->
            let guard = { guard with X = x; Y = y }
            data[y, x] <- guard.Direction |> Set.singleton |> Visited
            simulate iteration data guard onIterationContinue
        | Visited directions ->
            if directions |> Set.contains guard.Direction then
                { Iteration = iteration
                  StopReason = Looping }
            else
                let guard = { guard with X = x; Y = y }
                data[y, x] <- Set.add guard.Direction directions |> Visited
                simulate iteration data guard onIterationContinue
        | Barrier ->
            simulate
                iteration
                data
                { guard with
                    Direction = rotateRight guard.Direction }
                onIterationContinue


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
    let mutable possibleBarrierCounter = 0

    let searchForLoops data guard =
        let noop a b = ()
        let data = Array2D.copy data

        let y, x = nextFieldCoordinates guard
        data[y, x] <- Barrier

        let result = simulate 0 data guard noop

        match result.StopReason with
        | MaxIterationsReached -> ()
        | MapLeft -> ()
        | Looping -> possibleBarrierCounter <- possibleBarrierCounter + 1


    let data, guard = loadData ()
    let simulationResult = simulate 0 data guard searchForLoops

    // simulationResult |> dumpIgnore
    simulationResult.StopReason |> dumpNameIgnore "Simulation success"
    simulationResult.Iteration |> dumpNameIgnore "Simulation iterations"

    { SumVisitedFields =
        data
        |> flatten
        |> Array.filter (fun f ->
            match f with
            | Empty -> false
            | Visited _ -> true
            | Barrier -> false)
        |> Array.length
      FoundPossibleBarriers = possibleBarrierCounter }
