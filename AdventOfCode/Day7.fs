module AdventOfCode.Day7

open System
open System.IO

let filePath = @"Day 7 Input.txt"

type Line = { Result: int64; Numbers: int64 list }


let parseLine (str: string) =
    let split = str.Split ':'
    let result = split[0].Trim() |> Int64.Parse
    let numbers = split[1].Trim().Split ' ' |> Array.map Int64.Parse |> Array.toList

    { Result = result; Numbers = numbers }


let getData () =
    File.ReadAllLines filePath |> Array.toSeq |> Seq.map parseLine |> Seq.toList




module Part1 =
    type Operator =
        | Add
        | Mul
        | Con

    [<StructuredFormatDisplay("{Operator} {Number}")>]
    type Operation = { Number: int64; Operator: Operator }

    let doOperation firstNumber operations =
        let fold acc op =
            match op.Operator with
            | Add -> acc + op.Number
            | Mul -> acc * op.Number
            | Con ->
                let secondNumberLength = op.Number.ToString("N0").Length
                let necessaryShift = pown 10L secondNumberLength
                acc * necessaryShift + op.Number

        let x = operations |> List.fold fold firstNumber
        x

    let rec getCombinations (numbers: int64 list) : Operation list list =
        match numbers with
        | [] -> [ [] ]
        | number :: tail ->
            let x = getCombinations tail
            let a = x |> List.map (fun o -> { Number = number; Operator = Add } :: o)
            let b = x |> List.map (fun o -> { Number = number; Operator = Mul } :: o)
            let c = x |> List.map (fun o -> { Number = number; Operator = Con } :: o)
            a @ b @ c

    let isOk line =
        let firstNumber = line.Numbers |> List.head
        let remainingNumbers = line.Numbers |> List.tail

        getCombinations remainingNumbers
        |> Seq.map (doOperation firstNumber)
        |> Seq.contains line.Result

    let run () =
        getData () |> List.filter isOk |> List.sumBy _.Result

module Part2 =
    let run () = -1
