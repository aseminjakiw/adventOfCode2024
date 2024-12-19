module AdventOfCode.Day3

open System
open System.IO
open System.Text.RegularExpressions

let filePath = @"Day 3 input.txt"

let content = File.ReadAllText filePath

let toLong s = Int64.Parse s

module Part1 =
    let run () =
        let matches = Regex.Matches(content, @"mul\(([0-9]{1,3}),([0-9]{1,3})\)")

        matches
        |> Seq.map (fun m ->
            let x = m.Groups[1].Value |> toLong
            let y = m.Groups[2].Value |> toLong
            (x, y))
        |> Seq.map (fun (x, y) -> x * y)
        |> Seq.sum

module Part2 =
    type Instruction =
        | Multiply of int64 * int64
        | Do
        | Dont

    type State = { DoMultiply: bool; CurrentSum: int64 }


    let parseMatch (m: Match) =
        if m.Groups[3].Success then
            Do
        elif m.Groups[4].Success then
            Dont
        else
            let x = m.Groups[1].Value |> toLong
            let y = m.Groups[2].Value |> toLong
            Multiply(x, y)

    let execute state instruction =
        match instruction, state.DoMultiply with
        | Do, _ -> { state with DoMultiply = true }
        | Dont, _ -> { state with DoMultiply = false }
        | Multiply(x, y), true ->
            let prod = x * y

            { state with
                CurrentSum = state.CurrentSum + prod }
        | Multiply _, false -> state


    let run () =
        let matches =
            Regex.Matches(content, @"mul\(([0-9]{1,3}),([0-9]{1,3})\)|(do)\(\)|(don't)\(\)")

        let result =
            matches
            |> Seq.map parseMatch
            |> Seq.fold execute { DoMultiply = true; CurrentSum = 0 }

        result.CurrentSum
