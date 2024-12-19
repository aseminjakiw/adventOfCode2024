module AdventOfCode.Day2

open System
open System.IO

// Teil 1
let filePath = @"Day 2 Input.txt"

let parseLine (l: string) = l.Split(' ') |> Array.map Int32.Parse

let distance (x: int) y = y - x |> Math.Abs

type Monotony =
    | Increasing
    | Decreasing
    | None


module Part1 =
    let isSafe line =
        let isDistanceOk =
            line
            |> Array.pairwise
            |> Array.map (fun (x, y) -> distance x y)
            |> Array.toList
            |> List.forall (fun x -> x <= 3 && x >= 1)

        let isMonotonyOk =
            line
            |> Array.pairwise
            |> Array.map (fun (x, y) ->
                if y > x then Increasing
                elif x > y then Decreasing
                else None)
            |> Array.groupBy id
            |> Array.length = 1

        isDistanceOk && isMonotonyOk

    let run () =
        filePath
        |> File.ReadAllLines
        |> Array.map parseLine
        |> Array.where isSafe
        |> Array.length



// Teil 2
module Part2 =
    type FolderState =
        | Init1
        | Init2 of int
        | Init2OneError of int
        | Ok of int * Monotony
        | OneError of int * Monotony
        | Error

    let isDistanceOk x y =
        distance x y
        |> function
            | 1
            | 2
            | 3 -> true
            | _ -> false

    let getMonotony (x, y) =
        if x > y then Decreasing
        elif y > x then Increasing
        else None

    let isSafe (line: int array) =
        let rec checkPart indexToRemove =
            if indexToRemove >= line.Length then
                false
            else
                let reducedLine = Array.removeAt indexToRemove line

                if Part1.isSafe reducedLine then
                    true
                else
                    checkPart (indexToRemove + 1)

        if Part1.isSafe line then true else checkPart 0


    let run () =
        filePath
        |> File.ReadAllLines
        |> Array.map parseLine
        |> Array.where isSafe
        |> Array.length
