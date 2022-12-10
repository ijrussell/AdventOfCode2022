open System.IO

let readLine (input:string) =
    match input.Substring(0, 4) with
    | "addx" -> 
        let n = input.Substring(4) |> int
        ("A",0)::("A",n)::[]
    | "noop" -> ("N",0)::[]
    | _ -> failwith $"Unknown operation: {input}"

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Seq.toList
    |> Seq.map readLine
    |> Seq.concat
    |> Seq.fold (fun acc item ->
        let index, current, all = acc
        let score = current + snd item
        (index+1, score, (index+1, snd item, score)::all)
    ) (1, 1, [])
    |> fun (_,_,xs) -> xs
    |> Seq.rev
    |> Seq.toArray

let inspectionPoints =
    20 
    |> Seq.unfold (fun state -> if state > 220 then None else Some (state, state + 40))
    |> Seq.toList

let calcualteSignalStrength (input:(int * int * int) array) = 
    inspectionPoints 
    |> List.map (fun index -> 
        (index, input |> Array.find (fun (i,_, _) -> i = index))
    )
    |> List.sumBy (fun (i, (_, _, v)) -> i * v)
    
let result = data |> calcualteSignalStrength
