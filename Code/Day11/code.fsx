open System.IO
open System.Text.RegularExpressions

type Monkey = { 
    Items: int64 list
    Operation: int64 -> int64
    Test: int64 -> int
    TestDivisor: int64
    InspectionCount: int 
}

let data path = 
    path
    |> File.ReadAllLines
    |> Array.chunkBySize 7

// Hardcoded but should be calculated from input data
let maxWorryLevel = 2L * 3L * 5L * 7L * 11L * 13L * 17L * 19L * 23L

let (|Match|_|) (pattern: string) (input: string) =
    let m = Regex.Match(input, pattern) in
    if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ])
    else None

let createMonkey relief (input: string[]) =
    let items =
        match input[1] with
        | Match ".*: (.*)" [ s ] -> s.Split ", " |> Seq.map int64 |> Seq.toList
        | _ -> failwith $"Unknown initial items: {input[3]}"
    
    let operation =
        match input[2] with
        | Match ".* old \* old" [] -> fun x -> x * x
        | Match ".* old \* ([0-9]*)" [ s ] -> let y = int64 s in fun x -> x * y
        | Match ".* old \+ ([0-9]*)" [ s ] -> let y = int64 s in fun x -> x + y
        | _ -> failwith $"Unknown operation: {input[2]}"
        |> fun op ->
            if relief then fun x -> op x / 3L
            else fun x -> op x % maxWorryLevel

    let test =
        let condition =
            match input[3] with
            | Match ".* divisible by ([0-9]*)" [ s ] -> let d = int64 s in fun x -> x % d = 0L
            | _ -> failwith $"Unknown test condition: {input[3]}"

        let trueMonkeyId =
            match input[4] with
            | Match ".* If true: throw to monkey ([0-9]*)" [ s ] -> int s
            | _ -> failwith $"Unknown true monkey: {input[4]}"

        let falseMonkeyId =
            match input[5] with
            | Match ".* If false: throw to monkey ([0-9]*)" [ s ] -> int s
            | _ -> failwith $"Unknown false monkey: {input[5]}"

        fun x -> if condition x then trueMonkeyId else falseMonkeyId

    let testDivisor =
        match input[3] with
        | Match ".* divisible by ([0-9]*)" [ s ] -> int64 s
        | _ -> failwith $"Unknown test divisor: {input[3]}"

    { 
        Items = items
        Operation = operation
        Test = test
        TestDivisor = testDivisor
        InspectionCount = 0 
    }

let calculate hasRelief maxRounds (data:string[][]) =
    let monkeys =
        data 
        |> Array.map (createMonkey hasRelief) 
        |> Seq.indexed 
        |> Map.ofSeq

    let doRound monkeys =
        monkeys
        |> Map.keys  
        |> Seq.sort
        |> Seq.fold (fun map index ->
            let monkey = Map.find index map
            monkey.Items
            |> List.map (fun w -> 
                let newWorry = monkey.Operation w
                let movedTo = monkey.Test newWorry
                (movedTo, newWorry)
            )
            |> Seq.fold (fun map (mid, w) -> map |> Map.add mid { map[mid] with Items = map[mid].Items @ [ w ] }) map
            |> Map.add
                index
                { monkey with
                    InspectionCount = monkey.InspectionCount + List.length monkey.Items
                    Items = [] }) monkeys

    let inspections =
        [ 1..maxRounds ] 
        |> List.scan (fun s _ -> doRound s) monkeys
        |> Seq.last 
        |> Map.map (fun _ x -> x.InspectionCount) 
        |> Map.toSeq

    inspections
    |> Seq.map snd
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.toArray
    |> fun xs -> int64 xs[0] * int64 xs[1]

let path = Path.Combine(__SOURCE_DIRECTORY__, "data.txt")

module Part1 =

    let result = path |> data |> calculate true 20

module Part2 =

    let result = path |> data |> calculate false 10000
