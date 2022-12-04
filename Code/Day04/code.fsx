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
    
module Part1 =

    let isSubSet (assignments:Set<int> * Set<int>) =
        if Set.isSubset (fst assignments) (snd assignments) || Set.isSubset (snd assignments) (fst assignments) then Some ()
        else None

    let calculate (lines:string[]) =
        lines
        |> Seq.choose (createSets >> isSubSet)
        |> Seq.length

    let result = sectionAssignments |> calculate

module Part2 =

    let intersect (assignments:Set<int> * Set<int>) =
        if Set.intersect (fst assignments) (snd assignments) <> Set.empty then Some ()
        else None

    let calculate (lines:string[]) =
        lines
        |> Seq.choose (createSets >> intersect)
        |> Seq.length

    let result = sectionAssignments |> calculate

