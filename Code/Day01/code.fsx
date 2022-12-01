open System.IO

let text = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllText

let calculateCalories (input:string) =
    input.Split("\r\n\r\n")
    |> Array.map (fun g -> g.Split("\r\n") |> Array.sumBy int)

let getMostCalories (input:string) =
    input
    |> calculateCalories
    |> Array.max

let getTopThreeCalories (input:string) =
    input
    |> calculateCalories
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum

let mostCalories  = text |> getMostCalories 
let topThreeCalories = text |> getTopThreeCalories
