open System.IO

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Seq.toList

let findIndex (windowSize:int) (input:char list) =
    let rec loop index acc rem =
        match acc, rem with
        | [], [] -> index, acc
        | [], items -> 
            let test = items |> Seq.take windowSize
            let res =
                if test |> Set.ofSeq |> Set.count = windowSize then test |> Seq.toList
                else []
            loop (index+1) (res@[]) items.Tail
        | _, _ -> index, acc
    loop (windowSize-1) [] input

module Part1 =

    let results =
        data
        |> List.map (fun s -> findIndex 4 (s.ToCharArray() |> Array.toList))

module Part2 =

    let results =
        data
        |> List.map (fun s -> findIndex 14 (s.ToCharArray() |> Array.toList))

