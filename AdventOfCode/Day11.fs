module AdventOfCode.Day11

open System
open System.Diagnostics
open System.IO
open AdventOfCode.Day8
open AdventOfCode.Shared

type MapPosition =
    { Position: Vec2
      Height: byte }

    override this.ToString() = $"{this.Height}@{this.Position}"

let filePath = @"Day 11 Input.txt"

let parseChar (c: char) = c.ToString() |> Byte.Parse

let loadData () =
    let content = File.ReadAllText filePath

    content.Split(" ") |> Array.map Int64.Parse |> Array.toList


module Part1 =
    let (|HasEvenNumberDigits|_|) (number: int64) =
        let numberString = number.ToString("D")

        if numberString.Length |> Int32.IsEvenInteger then
            Some numberString
        else
            None

    let processNumber (number: int64) =
        match number with
        | 0L -> [ 1L ]
        | HasEvenNumberDigits numberString ->
            let firstNumber = numberString.Substring(0, numberString.Length / 2) |> Int64.Parse
            let secondNumber = numberString.Substring(numberString.Length / 2) |> Int64.Parse
            [ firstNumber; secondNumber ]
        | _ -> [ number * 2024L ]

    [<TailCall>]
    let rec repeatProcess (stopwatch: Stopwatch) numbers remainingIterations =
        match remainingIterations with
        | 0 -> numbers
        | _ ->
            stopwatch.Restart()
            let newNumbers = numbers |> List.collect processNumber
            stopwatch.Stop()

            $"{remainingIterations}: {stopwatch.Elapsed}" |> dumpIgnore

            repeatProcess stopwatch newNumbers (remainingIterations - 1)

    let run () =
        let data = loadData ()

        let watch = Stopwatch()
        let result = repeatProcess watch data 40 |> List.length
        watch.Elapsed |> dumpNameIgnore "Duration"

        result

module Part2 =
    let run () = -1
