open System.IO

let priorityData = ['a'..'z'] @ ['A'..'Z']

let getPriority (input:char) =
    priorityData 
    |> List.findIndex (fun c -> c = input)
    |> (+) 1

let rucksacks = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines

let handleIntersect input =
    input
    |> Seq.map Set.ofSeq
    |> Set.intersectMany
    |> Set.map (fun c -> getPriority c)

module Part1 =

    let calculate (lines:string[]) =
        lines
        |> Seq.map (fun line ->
            line 
            |> Seq.splitInto 2
            |> handleIntersect
        )
        |> Seq.sumBy (fun v -> Set.maxElement v)

    let result = rucksacks |> calculate

module Part2 =

    let calculate (lines:string[]) =
        lines
        |> Seq.chunkBySize 3
        |> Seq.map (fun chunk ->
            chunk
            |> handleIntersect
        )
        |> Seq.sumBy (fun v -> Set.maxElement v)

    let result = rucksacks |> calculate
