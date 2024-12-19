module AdventOfCode.Day9

open System
open System.IO
open AdventOfCode.Shared

let filePath = @"Day 9 Input.txt"

let parseField c =
    match c with
    | '.' -> None
    | _ -> c |> Some

let parseChar (c: char) =
    c.ToString()
    |> Int32.TryParse
    |> function
        | true, b -> Some b
        | false, _ -> None

let loadData () =
    let content = File.ReadAllText filePath

    content |> Seq.choose parseChar |> Seq.toList

module Part1 =
    let run () =
        loadData () |> dumpIgnore
        -1

module Part2 =
    let run () = -1
