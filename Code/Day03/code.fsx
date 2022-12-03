open System.IO

let getPriority (input:char) =
    ['a'..'z'] @ ['A'..'Z'] 
    |> List.findIndex (fun c -> c = input)
    |> (+) 1

let rucksacks = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines

let handleIntersect input =
    input
    |> Seq.map Set.ofSeq
    |> Set.intersectMany
    |> Set.map getPriority

module Part1 =

    let calculate (lines:string[]) =
        lines
        |> Seq.map (Seq.splitInto 2 >> handleIntersect)
        |> Seq.sumBy Set.maxElement

    let result = rucksacks |> calculate

module Part2 =

    let calculate (lines:string[]) =
        lines
        |> Seq.chunkBySize 3
        |> Seq.map handleIntersect
        |> Seq.sumBy Set.maxElement

    let result = rucksacks |> calculate
