module AdventOfCode.Day4

open System.IO
open System.Text.RegularExpressions
open AdventOfCode.Shared

let filePath = @"Day 4 Input.txt"

let fileContent = File.ReadAllLines filePath
let rows = fileContent.Length
let columns = fileContent[0].Length

let getData () =
    let data = Array2D.create columns rows ' '

    fileContent
    |> Seq.iteri (fun rowIndex row -> row |> Seq.iteri (fun columnIndex c -> data[rowIndex, columnIndex] <- c))

    data



module Part1 =
    let printInputInfo () =
        fileContent
        |> Array.collect (fun x -> x |> Seq.toArray)
        |> Array.groupBy id
        |> Array.map (fun (key, items) -> (key, items.Length))
        |> dump

    let countXmasForward (str: string) = Regex.Count(str, "XMAS")
    let countXmasBackward (str: string) = Regex.Count(str, "SAMX")

    let countXmas str =
        (countXmasForward str) + (countXmasBackward str)

    let charsToString (chars: char seq) = chars |> Seq.toArray |> System.String

    let countAll (input: char seq seq) =
        input |> Seq.map charsToString |> Seq.map countXmas |> Seq.sum

    let getDiagonalIndicies number =
        seq {
            for x in 0..number do
                yield (number - x, x)
        }

    let tryGet x y data =
        let length1 = data |> Array2D.length1
        let length2 = data |> Array2D.length2

        if x >= 0 && x < length1 && y >= 0 && y < length2 then
            data[x, y] |> Some
        else
            None

    let tryGetAll data positions =
        positions |> Seq.map (fun (x, y) -> tryGet x y data)

    let getDiagonalElements data =
        let rec getDiagonals number data =
            let elements =
                getDiagonalIndicies number |> tryGetAll data |> Seq.choose id |> Seq.toList

            if elements.Length = 0 then
                []
            else
                elements :: getDiagonals (number + 1) data

        getDiagonals 0 data

    let transpose data =
        let length1 = data |> Array2D.length1
        let length2 = data |> Array2D.length2
        let transposed = Array2D.create length2 length1 data[0, 0]

        for x in 0 .. (length1 - 1) do
            for y in 0 .. (length2 - 1) do
                transposed[y, x] <- data[x, y]

        transposed

    let rotateLeft data =
        let length1 = data |> Array2D.length1
        let length2 = data |> Array2D.length2
        let transposed = Array2D.create length2 length1 data[0, 0]

        for x in 0 .. (length1 - 1) do
            let row = data[x, *] |> Array.rev

            for y in 0 .. (length2 - 1) do
                transposed[y, x] <- row[y]

        transposed


    let run () =
        let data = getData ()

        let columnsCount =
            [ 0 .. (columns - 1) ]
            |> Seq.map (fun i -> data[i, *])
            |> Seq.map Array.toSeq
            |> countAll
        // |> dumpName "columnsCount"

        let rowsCount =
            [ 0 .. (rows - 1) ]
            |> Seq.map (fun i -> data[*, i])
            |> Seq.map Array.toSeq
            |> countAll
        // |> dumpName "rowsCount"

        let diagonalCount =
            data |> getDiagonalElements |> List.toSeq |> Seq.map List.toSeq |> countAll
        // |> dumpName "diagonal left bottom to right top"

        // let x = array2D [| [|1;2;3|];[|4;5;6|] |]
        // x |> dump
        // // x |> transpose |> dump
        // x |> rotateLeft |> dump

        let transposedDiagonalCount =
            data
            |> rotateLeft
            |> getDiagonalElements
            |> List.toSeq
            |> Seq.map List.toSeq
            |> countAll
        // |> dumpName "diagonal left top to right bottom"

        columnsCount + rowsCount + diagonalCount + transposedDiagonalCount

module Part2 =
    let isMatch (data: char array2d) =
        if data[1, 1] = 'A' then
            match data[0, 0], data[2, 2], data[2, 0], data[0, 2] with
            | 'M', 'S', 'M', 'S' -> true
            | 'M', 'S', 'S', 'M' -> true
            | 'S', 'M', 'M', 'S' -> true
            | 'S', 'M', 'S', 'M' -> true
            | _ -> false
        else
            false

    let run () =
        let data = getData ()

        let length1 = data |> Array2D.length1
        let length2 = data |> Array2D.length2

        seq {
            for x in 1 .. (length1 - 2) do
                for y in 1 .. (length2 - 2) do
                    yield data[x - 1 .. x + 1, y - 1 .. y + 1]
        }
        |> Seq.where isMatch
        |> Seq.length
