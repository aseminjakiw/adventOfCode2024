module AdventOfCode.Shared

let dump x =
    do printfn "%A" x
    x

let dumpIgnore x = x |> dump |> ignore

let dumpName name x =
    do printfn "%s: %A" name x
    x

let dumpNameIgnore name x = x |> dumpName name |> ignore