module AdventOfCode.Day5

open System
open System.IO

let filePath = @"Day 5 Input.txt"

type OrderRule =
    { First: int
      Second: int }

    override this.ToString() = $"{this.First}|{this.Second}"

type Update = int list

type Data =
    { OrderRules: OrderRule list
      Updates: Update list }

let parseOrderRule (line: string) =
    line.Split '|'
    |> Array.map Int32.Parse
    |> function
        | [| x; y |] -> { First = x; Second = y }
        | _ -> failwith "invalid order rule"

let parseUpdate (line: string) =
    line.Split ',' |> Array.map Int32.Parse |> Array.toList

let getData () =
    let content = File.ReadAllLines filePath

    let orderRules =
        content
        |> Array.takeWhile (fun l -> l <> "")
        |> Array.map parseOrderRule
        |> Array.toList

    let updates =
        content
        |> Array.skipWhile (fun l -> l.Contains '|' || l = "")
        |> Array.map parseUpdate
        |> Array.toList

    { OrderRules = orderRules
      Updates = updates }

let data = getData ()

module Part1 =
    let scan elements =
        let rec scan before element after =
            match after with
            | [] -> [ (before, element, []) ]
            | head :: tail -> (before, element, after) :: scan (head :: before) head tail

        scan [] (elements |> List.head) (elements |> List.tail)

    let isElementOrderOk (before, element, after) =
        let beforeOk =
            data.OrderRules
            |> List.filter (fun r -> r.First = element)
            |> List.filter (fun r -> before |> List.contains r.Second)
            |> List.isEmpty

        let afterOk =
            data.OrderRules
            |> List.filter (fun r -> r.Second = element)
            |> List.filter (fun r -> after |> List.contains r.First)
            |> List.isEmpty

        beforeOk && afterOk

    let isUpdateOk update =
        update |> scan |> List.forall isElementOrderOk

    let getCenterElement items =
        let lenght = items |> List.length

        if Int32.IsEvenInteger lenght then
            failwith "even number of items"
        else
            items[(lenght - 1) / 2]

    let run () =
        data.Updates |> List.filter isUpdateOk |> List.map getCenterElement |> List.sum


module Part2 =
    let order x y =
        let rule1 = data.OrderRules |> List.tryFind (fun r -> r.First = x && r.Second = y)
        let rule2 = data.OrderRules |> List.tryFind (fun r -> r.First = y && r.Second = x)

        match rule1, rule2 with
        | None, None -> 0
        | None, Some _ -> 1
        | Some _, None -> -1
        | Some _, Some _ -> failwith "found rule for both ordering directions"

    let run () =
        data.Updates
        |> List.filter (Part1.isUpdateOk >> not)
        |> List.map (List.sortWith order)
        |> List.map Part1.getCenterElement
        |> List.sum
