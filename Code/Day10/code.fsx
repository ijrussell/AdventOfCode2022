open System.IO

let readLine (input:string) =
    match input.Substring(0, 4) with
    | "addx" -> [0; input.Substring(4) |> int]
    | "noop" -> [0]
    | _ -> failwith $"Unknown operation: {input}"

let data = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines
    |> Seq.map readLine
    |> Seq.concat    
    |> Seq.toList

let calculateCummulativeValues (input:int list) =
    input 
    |> List.scan (fun acc item -> acc + item) 1
    |> List.toArray

module Part1 =

    let inspectionPoints initialValue =
        initialValue 
        |> Seq.unfold (fun state -> if state >= 240 then None else Some (state, state + 40))
        |> Seq.toList
    
    let calcualteSignalStrength (input:int array) = 
        inspectionPoints 20
        |> List.map (fun index -> (index, input[index-1]))
        |> List.sumBy (fun (i, v) -> i * v)
        
    let result = data |> calculateCummulativeValues |> calcualteSignalStrength

module Part2 =

    let showSprites (input:int array) =
        input
        |> Array.mapi (fun i v -> if abs (v - (i % 40)) < 2 then '#' else '.')
        |> Array.iteri (fun i c ->
            printf "%c" c
            if i % 40 = 39 then printfn ""
        )    

    let result = data |> calculateCummulativeValues |> showSprites
