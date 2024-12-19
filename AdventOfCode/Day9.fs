module AdventOfCode.Day9

open System
open System.IO

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

type EntryType =
    | File of ID: int64
    | Free

type Entry = { Length: int; Type: EntryType }

type Block =
    | Empty
    | Used of FileID: int64

let expandBlocks entry =
    match entry.Type with
    | File id -> Used id
    | Free -> Empty
    |> List.replicate entry.Length

let stringJoin (a: string) (b: string) = a + b

let blockToString block =
    match block with
    | Empty -> "."
    | Used fileId -> fileId.ToString() |> Seq.last |> Array.singleton |> String

let entryToBlockString entry =
    entry |> expandBlocks |> List.map blockToString |> List.fold stringJoin ""

let blocksToString blocks =
    blocks |> Seq.map blockToString |> Seq.fold stringJoin ""


module Part1 =

    let run () =
        let entries =
            loadData ()
            |> List.mapi (fun index number ->
                if Int32.IsEvenInteger index then
                    { Length = number
                      Type = index / 2 |> int64 |> File }
                else
                    { Length = number; Type = Free })
        // |> dumpName "Entries"

        let blocks = entries |> List.collect expandBlocks |> List.toArray
        // |> dumpName "Blocks"

        let mutable freePointer = 0
        let mutable copyPointer = (blocks |> Array.length) - 1

        // blocks |> blocksToString |> dumpNameIgnore "blocks before"

        while freePointer < copyPointer do
            if blocks[freePointer].IsUsed then
                freePointer <- freePointer + 1
            elif blocks[copyPointer].IsEmpty then
                copyPointer <- copyPointer - 1
            else
                blocks[freePointer] <- blocks[copyPointer]
                blocks[copyPointer] <- Empty

        // blocks |> blocksToString |> dumpNameIgnore "blocks after"

        blocks
        |> Array.toSeq
        |> Seq.mapi (fun index block ->
            match block with
            | Empty -> None
            | Used fileId -> (int64 index) * fileId |> Some)
        |> Seq.choose id
        |> Seq.sum

module Part2 =
    let run () =
        let mutable entries =
            loadData ()
            |> List.mapi (fun index number ->
                if Int32.IsEvenInteger index then
                    { Length = number
                      Type = index / 2 |> int64 |> File }
                else
                    { Length = number; Type = Free })
            |> List.toArray
        // |> dumpName "Entries"

        let findFileWithIdLower upperBoundFileId =
            let fileIndex =
                entries
                |> Array.findIndexBack (fun entry ->
                    match entry.Type with
                    | File id -> id < upperBoundFileId
                    | Free -> false)

            let file = entries[fileIndex]

            let fileId =
                match file.Type with
                | File id -> id
                | Free -> failwith "Impossible"

            (fileIndex, file, fileId)

        let mutable fileIndex, file, fileId = findFileWithIdLower Int64.MaxValue

        while fileId > 0 do
            entries
            |> Array.takeWhile (fun entry ->
                match entry.Type with
                | File id -> id <> fileId
                | Free -> true)
            |> Array.tryFindIndex (fun entry -> entry.Type.IsFree && entry.Length >= file.Length)
            |> function
                | None -> ()
                | Some freeIndex ->
                    let freeSpace = entries[freeIndex]

                    entries[freeIndex] <- file
                    entries[fileIndex] <- { file with Type = Free }

                    if freeSpace.Length = file.Length then
                        ()
                    else
                        let remainingFreeBlocks = freeSpace.Length - file.Length

                        let newEntry =
                            { Length = remainingFreeBlocks
                              Type = Free }

                        entries <- entries |> Array.insertAt (freeIndex + 1) newEntry

            let fileIndex', file', fileId' = findFileWithIdLower fileId
            fileIndex <- fileIndex'
            file <- file'
            fileId <- fileId'



        let blocks = entries |> Array.collect (expandBlocks >> List.toArray)







        // blocks |> blocksToString |> dumpNameIgnore "blocks after"




        blocks
        |> Array.toSeq
        |> Seq.mapi (fun index block ->
            match block with
            | Empty -> None
            | Used fileId -> (int64 index) * fileId |> Some)
        |> Seq.choose id
        |> Seq.sum
