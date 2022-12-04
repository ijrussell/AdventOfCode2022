open System.IO

let sectionAssignments = 
    Path.Combine(__SOURCE_DIRECTORY__, "data.txt")
    |> File.ReadAllLines

let convertToSet (input:string) =
    match input.Split("-") with
    | [|start;finish|] -> Set [int start..int finish]
    | _ -> failwith "Invalid part"

let createSets (input:string) =
    match input.Split(",") with
    | [|first;second|] -> convertToSet first, convertToSet second
    | _ -> failwith "Invalid input"
    
let calculate f (lines:string[]) =
    lines
    |> Seq.choose (createSets >> f)
    |> Seq.length

module Part1 =

    let isSubSet (assignments:Set<int> * Set<int>) =
        if Set.isSubset (fst assignments) (snd assignments) || Set.isSubset (snd assignments) (fst assignments) then Some ()
        else None

    let result = sectionAssignments |> calculate isSubSet

module Part2 =

    let intersect (assignments:Set<int> * Set<int>) =
        if Set.intersect (fst assignments) (snd assignments) <> Set.empty then Some ()
        else None

    let result = sectionAssignments |> calculate intersect

