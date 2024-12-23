// For more information see https://aka.ms/fsharp-console-apps




open AdventOfCode
open BenchmarkDotNet.Running

printfn "Advent of code 2024"
printfn ""

// printfn $"Day 1 part 1: %A{Day1.Part1.run()}"
// printfn $"Day 1 part 2: %A{Day1.Part2.run()}"
//
// printfn $"Day 2 part 1: %A{Day2.Part1.run()}"
// printfn $"Day 2 part 2: %A{Day2.Part2.run()}"
//
// printfn $"Day 3 part 1: %A{Day3.Part1.run()}"
// printfn $"Day 3 part 2: %A{Day3.Part2.run()}"
//
// printfn $"Day 4 part 1: %A{Day4.Part1.run()}"
// printfn $"Day 4 part 2: %A{Day4.Part2.run()}"
//
// printfn $"Day 5 part 1: %A{Day5.Part1.run()}"
// printfn $"Day 5 part 2: %A{Day5.Part2.run()}"

// printfn $"Day 6: %A{Day6.run ()}"

// printfn $"Day 7 part 1: %A{Day7.Part1.run ()}"
// printfn $"Day 7 part 2: %A{Day7.Part2.run ()}"

// printfn $"Day 8 part 1: %A{Day8.Part1.run ()}"
// printfn $"Day 8 part 2: %A{Day8.Part2.run ()}"
//
// printfn $"Day 9 part 1: %A{Day9.Part1.run ()}"
// printfn $"Day 9 part 2: %A{Day9.Part2.run ()}"

// printfn $"Day 10 part 1: %A{Day10.Part1.run ()}"
// printfn $"Day 10 part 2: %A{Day10.Part2.run ()}"

let iterations = 10
let day11Data = Day11.loadData ()

let results =
    [ Day11.Solution1.run
      Day11.Solution2.run
      Day11.Solution3.run
      Day11.Solution4.run
      Day11.Solution5.run
      Day11.Solution6.run
      Day11.Solution7.run
      Day11.Solution8.run
      Day11.Solution9.run
      Day11.Solution10.run
      Day11.Solution11.run
      Day11.Solution12.run
      Day11.Solution13.run
      Day11.Solution14.run
      Day11.Solution15.run
      Day11.Solution16.run
      Day11.Solution17.run
      Day11.Solution18.run ]
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


let _ = BenchmarkRunner.Run<Day11.Benchmark>()


printfn ""
printfn "finished"
