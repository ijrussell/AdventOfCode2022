open System.IO

type Choice = Rock | Paper | Scissors

let text = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines

module Part1 =

    let convert (row:string) =
        let player1 = match row[0] with 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors | _ -> failwith "Unknown player1"
        let player2 = match row[2] with 'X' -> Rock | 'Y' -> Paper | 'Z' -> Scissors | _ -> failwith "Unknown player2"
        player1, player2

    let gameScore (game:Choice * Choice) =
        match game with
        | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6 // Win
        | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3 // Draw
        | Rock, Scissors | Paper, Rock | Scissors, Paper -> 0 // Lose

    let choiceScore choice =
        match choice with Rock -> 1 | Paper -> 2 | Scissors -> 3

    let calculate (input:string[]) =
        input
        |> Array.map (fun s -> 
            let game = convert s
            gameScore game + choiceScore (snd game)
        )
        |> Array.sum

    let totalScore = text |> calculate

module Part2 =

    type Result = Lose | Draw | Win  
    
    let convert (row:string) =
        let player1 = match row[0] with 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors | _ -> failwith "Unknown player1"
        let result = match row[2] with 'X' -> Lose | 'Y' -> Draw | 'Z' -> Win | _ -> failwith "Unknown result"
        player1, result

    let gameScore (game:Choice * Result) =
        match game with
        | Rock, Draw | Paper, Lose | Scissors, Win -> 1 // Rock
        | Rock, Win | Paper, Draw | Scissors, Lose  -> 2 // Paper
        | Rock, Lose | Paper, Win | Scissors, Draw -> 3 // Scissors

    let resultScore result =
        match result with Lose -> 0 | Draw -> 3 | Win -> 6

    let calculate (input:string[]) =
        input
        |> Array.map (fun s -> 
            let game = convert s
            gameScore game + resultScore (snd game)
        )
        |> Array.sum

    let totalScore = text |> calculate

