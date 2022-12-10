open System.IO

let readLine (input:string) =
    match input.Substring(0, 4) with
    | "addx" -> 
        let n = input.Substring(4) |> int
        0::n::[]
    | "noop" -> 0::[]
    | _ -> failwith $"Unknown operation: {input}"

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Seq.map readLine
    |> Seq.concat    
    |> Seq.toList

let processInput (input:int list) =
    input 
    |> List.scan (fun acc item -> acc + item) 1
    |> List.toArray

let inspectionPoints =
    20 
    |> Seq.unfold (fun state -> if state > 220 then None else Some (state, state + 40))
    |> Seq.toList

let calcualteSignalStrength (input:int array) = 
    inspectionPoints 
    |> List.map (fun index -> (index, input[index-1]))
    |> List.sumBy (fun (i, v) -> i * v)
    
let result = data |> processInput |> calcualteSignalStrength
