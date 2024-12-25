module AdventOfCode.Day11

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open AdventOfCode.Day8
open BenchmarkDotNet.Attributes

type MapPosition =
    { Position: Vec2
      Height: byte }

    override this.ToString() = $"{this.Height}@{this.Position}"

let filePath = @"Day 11 Input.txt"

let parseChar (c: char) = c.ToString() |> Byte.Parse

let loadData () =
    let content = File.ReadAllText filePath

    content.Split(" ") |> Array.map Int64.Parse

let (|HasEvenNumberDigits|_|) (number: int64) =
    let numberString = number.ToString("D")

    if numberString.Length |> Int32.IsEvenInteger then
        Some numberString
    else
        None


let processNumber (number: int64) =
    match number with
    | 0L -> [| 1L |]
    | HasEvenNumberDigits numberString ->
        let firstNumber = numberString.Substring(0, numberString.Length / 2) |> Int64.Parse
        let secondNumber = numberString.Substring(numberString.Length / 2) |> Int64.Parse
        [| firstNumber; secondNumber |]
    | _ -> [| number * 2024L |]



module Solution1 =
    let processNumber (number: int64) =
        match number with
        | 0L -> [ 1L ]
        | HasEvenNumberDigits numberString ->
            let firstNumber = numberString.Substring(0, numberString.Length / 2) |> Int64.Parse
            let secondNumber = numberString.Substring(numberString.Length / 2) |> Int64.Parse
            [ firstNumber; secondNumber ]
        | _ -> [ number * 2024L ]

    [<TailCall>]
    let rec repeatProcess numbers remainingIterations =
        match remainingIterations with
        | 0 -> numbers
        | _ ->
            let newNumbers = numbers |> List.collect processNumber

            repeatProcess newNumbers (remainingIterations - 1)

    /// First intuitive solution, using F# lists and a width search
    let run maxIterations data =
        let data = data |> Array.toList
        let result = repeatProcess data maxIterations |> List.length
        result |> int64

module Solution2 =
    [<TailCall>]
    let rec repeatProcess numbers remainingIterations =
        match remainingIterations with
        | 0 -> numbers
        | _ ->
            let newNumbers = numbers |> Array.collect processNumber
            repeatProcess newNumbers (remainingIterations - 1)

    /// using arrays instead of F# lists and a width search
    let run maxIterations data =
        let result = repeatProcess data maxIterations |> Array.length
        result |> int64

let parallelRun maxIterations data (run: _ -> _ -> int64) =
    let initialIterations = Math.Min(4, maxIterations)
    let remainingIterations = Math.Max((maxIterations - initialIterations), 0)

    let intermediateResult =
        Solution2.repeatProcess data initialIterations |> Seq.toList

    let groups =
        intermediateResult
        |> List.mapi (fun index element -> (index, element))
        |> List.groupBy (fun (index, _) -> index % 24)
        |> List.map (fun (_, elements) -> elements |> List.map snd)
        |> List.map (fun x -> x |> List.toArray)


    let calculation =
        groups
        |> List.map (fun elements -> async { return run remainingIterations elements })
        |> Async.Parallel

    let result = Async.RunSynchronously calculation |> Array.sum |> int64
    result

module Solution3 =
    [<TailCall>]
    let rec repeatProcess numbers remainingIterations =
        match remainingIterations with
        | 0 -> numbers
        | _ ->
            let newNumbers = numbers |> Seq.collect processNumber
            repeatProcess newNumbers (remainingIterations - 1)

    /// using seq instead of F# lists and a width search
    let run maxIterations data =
        let data = data |> Array.toSeq
        let result = repeatProcess data maxIterations |> Seq.length

        result |> int64

module Solution4 =
    // using seq, a width search but parallize on 24 tasks
    let run maxIterations data =
        parallelRun maxIterations data Solution3.run


let pushOn (stack: Stack<_>) items = items |> Array.iter stack.Push

type Item =
    { RemainingIterations: int
      Number: int64 }

module Solution5 =
    // using a stack with depth first search, imperative while
    let run maxIterations data =
        let stack = Stack()

        data
        |> Array.map (fun number ->
            { RemainingIterations = maxIterations
              Number = number })
        |> pushOn stack

        let mutable counter = 0L

        while stack.Count <> 0 do
            let item = stack.Pop()

            if item.RemainingIterations = 0 then
                counter <- counter + 1L
            else
                processNumber item.Number
                |> Array.map (fun i ->
                    { RemainingIterations = item.RemainingIterations - 1
                      Number = i })
                |> pushOn stack

        counter

module Solution6 =
    [<TailCall>]
    let rec repeatProcess (stack: Stack<_>) counter =
        if stack.Count = 0 then
            counter
        else
            let item = stack.Pop()

            if item.RemainingIterations = 0 then
                repeatProcess stack (counter + 1L)
            else
                processNumber item.Number
                |> Array.map (fun i ->
                    { RemainingIterations = item.RemainingIterations - 1
                      Number = i })
                |> pushOn stack

                repeatProcess stack counter


    // using a stack with depth first search, tail recursion
    let run maxIterations data =
        let stack = Stack()

        data
        |> Array.map (fun number ->
            { RemainingIterations = maxIterations
              Number = number })
        |> pushOn stack

        let result = repeatProcess stack 0
        result

module Solution7 =
    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        parallelRun maxIterations data Solution6.run

module Solution8 =
    type ItemStruct =
        struct
            val mutable RemainingIterations: int
            val mutable Number: int64
        end

    [<TailCall>]
    let rec repeatProcess (stack: Stack<ItemStruct>) counter =
        if stack.Count = 0 then
            counter
        else
            let item = stack.Pop()

            if item.RemainingIterations = 0 then
                repeatProcess stack (counter + 1L)
            else
                processNumber item.Number
                |> Array.map (fun i -> ItemStruct(Number = i, RemainingIterations = item.RemainingIterations - 1))
                |> pushOn stack

                repeatProcess stack counter

    // using a stack with depth first search, Struct instead of Record, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        let run maxIterations data =
            let stack = Stack()

            data
            |> Seq.toArray
            |> Array.map (fun number -> ItemStruct(RemainingIterations = maxIterations, Number = number))
            |> pushOn stack

            repeatProcess stack 0

        parallelRun maxIterations data run

module Solution9 =
    type ItemStruct() =
        [<DefaultValue>]
        val mutable RemainingIterations: int

        [<DefaultValue>]
        val mutable Number: int64

        override this.ToString() =
            $"{this.Number}@{this.RemainingIterations}"

    [<TailCall>]
    let rec repeatProcess (stack: Stack<ItemStruct>) counter =
        let mutable hasItem, item = stack.TryPeek()

        if not hasItem then
            counter
        else if item.RemainingIterations = 0 then
            stack.Pop() |> ignore
            repeatProcess stack (counter + 1L)
        else
            item.RemainingIterations <- item.RemainingIterations - 1

            let newNumber =
                match item.Number with
                | 0L -> 1L
                | HasEvenNumberDigits numberString ->
                    let firstNumber = numberString.Substring(0, numberString.Length / 2) |> Int64.Parse

                    let secondNumber = numberString.Substring(numberString.Length / 2) |> Int64.Parse

                    ItemStruct(RemainingIterations = item.RemainingIterations, Number = secondNumber)
                    |> stack.Push

                    firstNumber
                | _ -> item.Number * 2024L

            item.Number <- newNumber


            repeatProcess stack counter

    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        let stack = Stack()

        data
        |> Seq.toArray
        |> Array.map (fun number -> ItemStruct(RemainingIterations = maxIterations, Number = number))
        |> pushOn stack

        repeatProcess stack 0

module Solution10 =
    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        parallelRun maxIterations data Solution9.run

module Solution11 =
    open Solution9

    [<TailCall>]
    let rec repeatProcess (stack: Stack<ItemStruct>, counter: int64) =
        let mutable hasItem, item = stack.TryPeek()

        if not hasItem then
            counter
        else if item.RemainingIterations = 0 then
            stack.Pop() |> ignore
            repeatProcess (stack, (counter + 1L))
        else
            item.RemainingIterations <- item.RemainingIterations - 1

            let newNumber =
                match item.Number with
                | 0L -> 1L
                | HasEvenNumberDigits numberString ->
                    let firstNumber = numberString.Substring(0, numberString.Length / 2) |> Int64.Parse

                    let secondNumber = numberString.Substring(numberString.Length / 2) |> Int64.Parse

                    ItemStruct(RemainingIterations = item.RemainingIterations, Number = secondNumber)
                    |> stack.Push

                    firstNumber
                | _ -> item.Number * 2024L

            item.Number <- newNumber


            repeatProcess (stack, counter)

    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        let stack = Stack()

        data
        |> Seq.toArray
        |> Array.map (fun number -> ItemStruct(RemainingIterations = maxIterations, Number = number))
        |> pushOn stack

        repeatProcess (stack, 0)


module Solution12 =
    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        parallelRun maxIterations data Solution11.run

module Solution13 =
    open Solution9

    type Range =
        { Lower: int64
          Upper: int64
          Divisor: int64 }

    let inRange x range = x >= range.Lower && x <= range.Upper

    let ranges =
        seq {
            for i in 1..9 do
                let divisor = (pown 10L i)
                let lower = (pown 10L ((i * 2) - 1))
                let upper = lower * 10L - 1L

                yield
                    { Lower = lower
                      Upper = upper
                      Divisor = divisor }
        }
        |> Seq.toArray

    let (|InAnyRange|_|) ranges x = ranges |> Array.tryFind (inRange x)

    [<TailCall>]
    let rec repeatProcess (stack: Stack<ItemStruct>) counter =
        let mutable hasItem, item = stack.TryPeek()

        if not hasItem then
            counter
        else if item.RemainingIterations = 0 then
            stack.Pop() |> ignore
            repeatProcess stack (counter + 1L)
        else
            item.RemainingIterations <- item.RemainingIterations - 1

            let newNumber =
                match item.Number with
                | 0L -> 1L
                | InAnyRange ranges matchingRange ->
                    let leftNumber = item.Number / matchingRange.Divisor
                    let rightNumber = item.Number - (leftNumber * matchingRange.Divisor)

                    ItemStruct(RemainingIterations = item.RemainingIterations, Number = rightNumber)
                    |> stack.Push

                    leftNumber
                | _ -> item.Number * 2024L

            item.Number <- newNumber


            repeatProcess stack counter

    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        let stack = Stack()

        data
        |> Seq.toArray
        |> Array.map (fun number -> ItemStruct(RemainingIterations = maxIterations, Number = number))
        |> pushOn stack

        repeatProcess stack 0


module Solution14 =
    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        parallelRun maxIterations data Solution13.run

module Solution15 =
    open Solution9

    type Range =
        struct
            val Lower: int64
            val Upper: int64
            val Divisor: int64

            new(lower, upper, divisor) =
                { Lower = lower
                  Upper = upper
                  Divisor = divisor }
        end

    let inRange x (range: Range) = x >= range.Lower && x <= range.Upper

    let ranges =
        seq {
            for i in 1..9 do
                let divisor = (pown 10L i)
                let lower = (pown 10L ((i * 2) - 1))
                let upper = lower * 10L - 1L
                yield Range(lower, upper, divisor)
        }
        |> Seq.toArray

    let (|InAnyRange|_|) ranges x = ranges |> Array.tryFind (inRange x)

    [<TailCall>]
    let rec repeatProcess (stack: Stack<ItemStruct>) counter =
        let mutable hasItem, item = stack.TryPeek()

        if not hasItem then
            counter
        else if item.RemainingIterations = 0 then
            stack.Pop() |> ignore
            repeatProcess stack (counter + 1L)
        else
            item.RemainingIterations <- item.RemainingIterations - 1

            let newNumber =
                match item.Number with
                | 0L -> 1L
                | InAnyRange ranges matchingRange ->
                    let leftNumber = item.Number / matchingRange.Divisor
                    let rightNumber = item.Number - (leftNumber * matchingRange.Divisor)

                    ItemStruct(RemainingIterations = item.RemainingIterations, Number = rightNumber)
                    |> stack.Push

                    leftNumber
                | _ -> item.Number * 2024L

            item.Number <- newNumber


            repeatProcess stack counter

    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        let stack = Stack()

        data
        |> Seq.toArray
        |> Array.map (fun number -> ItemStruct(RemainingIterations = maxIterations, Number = number))
        |> pushOn stack

        repeatProcess stack 0

module Solution16 =
    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        parallelRun maxIterations data Solution15.run

module Solution17 =
    open Solution9

    let processNumber (stack: Stack<_>) divisor remainingIterations number =
        let leftNumber = number / divisor
        let rightNumber = number - (leftNumber * divisor)

        ItemStruct(RemainingIterations = remainingIterations, Number = rightNumber)
        |> stack.Push

        leftNumber

    [<TailCall>]
    let rec repeatProcess (stack: Stack<ItemStruct>) counter =
        let mutable hasItem, item = stack.TryPeek()

        if not hasItem then
            counter
        else if item.RemainingIterations = 0 then
            stack.Pop() |> ignore
            repeatProcess stack (counter + 1L)
        else
            item.RemainingIterations <- item.RemainingIterations - 1

            let newNumber =
                match item.Number with
                | 0L -> 1L
                | x when x >= 10L && x <= 99L -> processNumber stack 10L item.RemainingIterations x
                | x when x >= 1000L && x <= 9999L -> processNumber stack 100L item.RemainingIterations x
                | x when x >= 100000L && x <= 999999L -> processNumber stack 1000L item.RemainingIterations x
                | x when x >= 10000000L && x <= 99999999L -> processNumber stack 10000L item.RemainingIterations x
                | x when x >= 1000000000L && x <= 9999999999L -> processNumber stack 100000L item.RemainingIterations x
                | x when x >= 100000000000L && x <= 999999999999L ->
                    processNumber stack 1000000L item.RemainingIterations x
                | x when x >= 10000000000000L && x <= 99999999999999L ->
                    processNumber stack 10000000L item.RemainingIterations x
                | x when x >= 1000000000000000L && x <= 9999999999999999L ->
                    processNumber stack 100000000L item.RemainingIterations x
                | x when x >= 100000000000000000L && x <= 999999999999999999L ->
                    processNumber stack 1000000000L item.RemainingIterations x
                | _ -> item.Number * 2024L

            item.Number <- newNumber


            repeatProcess stack counter

    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        let stack = Stack()

        data
        |> Seq.toArray
        |> Array.map (fun number -> ItemStruct(RemainingIterations = maxIterations, Number = number))
        |> pushOn stack

        repeatProcess stack 0

module Solution18 =
    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        parallelRun maxIterations data Solution17.run

module Solution19 =
    let splitNumber item divisor nextIteration =
        let leftNumber = item.Number / divisor
        let rightNumber = item.Number - (leftNumber * divisor)

        { Number = leftNumber
          RemainingIterations = nextIteration },
        ValueSome
            { Number = rightNumber
              RemainingIterations = nextIteration }

    let processNumber item =
        let nextIteration = item.RemainingIterations - 1

        match item.Number with
        | 0L ->
            { Number = 1L
              RemainingIterations = nextIteration },
            ValueNone
        | x when x >= 10L && x <= 99L -> splitNumber item 10L nextIteration
        | x when x >= 1000L && x <= 9999L -> splitNumber item 100L nextIteration
        | x when x >= 100000L && x <= 999999L -> splitNumber item 1000L nextIteration
        | x when x >= 10000000L && x <= 99999999L -> splitNumber item 10000L nextIteration
        | x when x >= 1000000000L && x <= 9999999999L -> splitNumber item 100000L nextIteration
        | x when x >= 100000000000L && x <= 999999999999L -> splitNumber item 1000000L nextIteration
        | x when x >= 10000000000000L && x <= 99999999999999L -> splitNumber item 10000000L nextIteration
        | x when x >= 1000000000000000L && x <= 9999999999999999L -> splitNumber item 100000000L nextIteration
        | x when x >= 100000000000000000L && x <= 999999999999999999L -> splitNumber item 1000000000L nextIteration
        | _ ->
            { Number = item.Number * 2024L
              RemainingIterations = nextIteration },
            ValueNone

    [<TailCall>]
    let rec repeatProcess numbers counter =
        match numbers with
        | [] -> counter
        | head :: tail ->
            match head.RemainingIterations with
            | 0 -> repeatProcess tail counter
            | _ ->
                let left, right = processNumber head
                let newNumbers = left :: tail

                match right with
                | ValueNone -> repeatProcess newNumbers counter
                | ValueSome item ->
                    let newNumbers = item :: newNumbers
                    repeatProcess newNumbers (counter + 1L)


    let run maxIterations data =
        let input =
            data
            |> Seq.toList
            |> List.map (fun number ->
                { Number = number
                  RemainingIterations = maxIterations })

        let inputLenght = input |> List.length |> int64

        repeatProcess input inputLenght

module Solution20 =
    // using a stack with depth first search, tail recursion, parallize on 24 tasks
    let run maxIterations data =
        parallelRun maxIterations data Solution19.run

module Solution21 =
    let mutable knownResults = Dictionary<Item, int64>()

    let rec repeatProcess (stack: Stack<_>) counter =
        if stack.Count = 0 then
            counter
        else
            let item = stack.Pop()

            knownResults.TryGetValue item
            |> function
                | true, result -> result
                | false, _ ->
                    let count =
                        if item.RemainingIterations = 0 then
                            repeatProcess stack (counter + 1L)
                        else
                            processNumber item.Number
                            |> Array.map (fun i ->
                                { RemainingIterations = item.RemainingIterations - 1
                                  Number = i })
                            |> pushOn stack

                            repeatProcess stack counter

                    knownResults[item] <- count

                    count

    // using a stack with depth first search, tail recursion
    let run maxIterations data =
        let stack = Stack()

        data
        |> Array.map (fun number ->
            { RemainingIterations = maxIterations
              Number = number })
        |> pushOn stack

        let result = repeatProcess stack 0
        result

module Solution22 =
    let rec expandItemAndCount (item: Item) =
        if item.RemainingIterations = 0 then
            1L
        else
            let left, right = Solution19.processNumber item

            match right with
            | ValueNone ->
                let subCount = expandItemAndCount left
                subCount
            | ValueSome item ->
                let leftSubCount = expandItemAndCount left
                let rightSubCount = expandItemAndCount item
                leftSubCount + rightSubCount

    let run maxIterations data =
        data
        |> Array.map (fun n ->
            { Number = n
              RemainingIterations = maxIterations })
        |> Array.map expandItemAndCount
        |> Array.sum

module Solution23 =
    let addIfNotLeafe map item result =
        if item.RemainingIterations = 0 then
            map
        else
            map |> Map.add item result

    let rec expandItemAndCount knownResults (item: Item) =
        if item.RemainingIterations = 0 then
            1L, knownResults
        else
            match knownResults |> Map.tryFind item with
            | Some result -> result, knownResults
            | None ->
                let left, right = Solution19.processNumber item

                let result, newKnownResults =
                    match right with
                    | ValueNone -> expandItemAndCount knownResults left
                    | ValueSome item ->
                        let leftSubCount, knownResults = expandItemAndCount knownResults left
                        let rightSubCount, knownResults = expandItemAndCount knownResults item
                        leftSubCount + rightSubCount, knownResults

                let knownResults = addIfNotLeafe newKnownResults item result
                result, knownResults

    let run maxIterations data =
        data
        |> Array.map (fun n ->
            { Number = n
              RemainingIterations = maxIterations })
        |> Array.fold
            (fun (map, count) item ->
                let result, newMap = expandItemAndCount map item
                (newMap, result + count))
            (Map.empty, 0L)
        |> snd

module Part1 =
    let run () =
        let data = loadData ()
        Solution23.run 25 data

module Part2 =
    let runDevelopment () =
        let iterations = 10
        let day11Data = loadData ()

        let results =
            [ Solution1.run
              Solution2.run
              Solution3.run
              Solution4.run
              Solution5.run
              Solution6.run
              Solution7.run
              Solution8.run
              Solution9.run
              Solution10.run
              Solution11.run
              Solution12.run
              Solution13.run
              Solution14.run
              Solution15.run
              Solution16.run
              Solution17.run
              Solution18.run
              Solution19.run
              Solution20.run
              Solution21.run
              Solution22.run
              Solution23.run ]
            |> List.mapi (fun index solution ->
                let result = solution iterations day11Data
                let solutionNumber = index + 1
                printfn "Day 11 %i: %i" solutionNumber result
                result)

        results
        |> List.groupBy id
        |> List.length
        |> function
            | 1 -> ()
            | _ -> failwith "incorrect solution found"

        let watch = Stopwatch()

        watch.Start()
        let result = Solution23.run 75 day11Data
        watch.Stop()

        printfn "Day 11 Part 2: %i, in %A" result watch.Elapsed

    // do BenchmarkRunner.Run<Day11.Benchmark>() |> ignore

    let run () =
        let data = loadData ()
        Solution23.run 75 data


[<MemoryDiagnoser>]
type Benchmark() =
    let mutable maxIterations = 5
    let data = loadData ()

    [<Params(1, 10, 20, 30, 40)>]
    member val Iterations = 1 with get, set

    [<GlobalSetup>]
    member this.Setup() = maxIterations <- this.Iterations

    // [<Benchmark>]
    // member this.Solution1() = Solution1.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution2() = Solution2.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution3() = Solution3.run maxIterations data
    // //
    // [<Benchmark>]
    // member this.Solution4() = Solution4.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution5() = Solution5.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution6() = Solution6.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution7() = Solution7.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution8() = Solution8.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution9() = Solution9.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution10() = Solution10.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution11() = Solution11.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution12() = Solution12.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution13() = Solution13.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution14() = Solution14.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution15() = Solution15.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution16() = Solution16.run maxIterations data
    //
    // [<Benchmark(Baseline = true)>]
    // member this.Solution17() = Solution17.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution18() = Solution18.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution19() = Solution19.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution20() = Solution20.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution21() = Solution21.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution22() = Solution22.run maxIterations data

    [<Benchmark>]
    member this.Solution23() = Solution23.run maxIterations data
