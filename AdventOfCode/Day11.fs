module AdventOfCode.Day11

open System
open System.Collections.Generic
open System.IO
open AdventOfCode.Day6
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
        result

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
        result

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
    let rec repeatProcess (stack: Stack<ItemStruct>, counter : int64) =
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
    type Range = {
        Lower: int64
        Upper: int64
        Divisor: int64
    }
    
    let inRange x range =        x >= range.Lower && x <= range.Upper
    
    let ranges =
        seq{
            for i in 1..9 do
                let divisor = (pown 10L i)
                let lower = (pown 10L ((i*2)-1))
                let upper = lower * 10L - 1L
                
                yield {Lower = lower; Upper = upper; Divisor = divisor}
        }
        |> Seq.toArray
        
    let (|InAnyRange|_|) ranges x =
        ranges
        |> Array.tryFind (inRange x) 
        
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
            new(lower, upper, divisor) = { Lower = lower; Upper = upper; Divisor = divisor }
        end
    
    let inRange x (range:Range) =        x >= range.Lower && x <= range.Upper
    
    let ranges =
        seq{
            for i in 1..9 do
                let divisor = (pown 10L i)
                let lower = (pown 10L ((i*2)-1))
                let upper = lower * 10L - 1L
                yield Range(lower, upper, divisor)
        }
        |> Seq.toArray
        
    let (|InAnyRange|_|) ranges x =
        ranges
        |> Array.tryFind (inRange x) 
        
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
        
    let processNumber (stack:Stack<_>) divisor remainingIterations number=
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
                | x when x >= 100000000000L && x <= 999999999999L -> processNumber stack 1000000L item.RemainingIterations x
                | x when x >= 10000000000000L && x <= 99999999999999L -> processNumber stack 10000000L item.RemainingIterations x
                | x when x >= 1000000000000000L && x <= 9999999999999999L -> processNumber stack 100000000L item.RemainingIterations x
                | x when x >= 100000000000000000L && x <= 999999999999999999L -> processNumber stack 1000000000L item.RemainingIterations x          
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
        
[<MemoryDiagnoser>]
type Benchmark() =
    let mutable maxIterations = 5
    let data = loadData ()

    [<Params(10,30)>]
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

    [<Benchmark(Baseline = true)>]
    member this.Solution4() = Solution4.run maxIterations data

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
    
    // [<Benchmark>]
    // member this.Solution11() = Solution11.run maxIterations data

    // [<Benchmark>]
    // member this.Solution12() = Solution12.run maxIterations data
    //
    // [<Benchmark>]
    // member this.Solution13() = Solution13.run maxIterations data

    [<Benchmark>]
    member this.Solution14() = Solution14.run maxIterations data
