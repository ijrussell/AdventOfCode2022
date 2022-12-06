open System.IO

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Seq.toList

let findIndex (windowSize:int) (input:char[]) =
    let rec loop index acc rem =
        match acc, rem with
        | [], [] -> failwith "Unable to find a solution"
        | [], items -> 
            let window = items |> List.take windowSize
            let result = if window |> Set.ofList |> Set.count = windowSize then window else []
            loop (index+1) result items.Tail
        | _, _ -> index, acc // Solution found
    loop (windowSize-1) [] (input |> Array.toList)

// let findIndex (windowSize:int) (input:char[]) =
//     input
//     |> Seq.windowed windowSize 
//     |> Seq.findIndex (fun s -> s |> Set.ofSeq |> Set.count = windowSize)
//     |> (+) windowSize

module Part1 =

    let results =
        data
        |> List.map (fun s -> findIndex 4 (s.ToCharArray()))

module Part2 =

    let results =
        data
        |> List.map (fun s -> findIndex 14 (s.ToCharArray()))

