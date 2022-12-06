open System.IO

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Seq.toList

let findIndex (input:char list) =
    let rec loop index acc rem =
        match acc, rem with
        | [], [] -> index, acc
        | [], items -> 
            let test = items |> Seq.take 4
            let res =
                if test |> Set.ofSeq |> Set.count = 4 then test |> Seq.toList
                else []
            loop (index+1) (res@[]) items.Tail
        | _, _ -> index, acc
    loop 0 [] input

let results =
    data
    |> List.map (fun s -> findIndex (s.ToCharArray() |> Array.toList) |> fun (i,items) -> i+3, items)

