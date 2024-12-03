open System
// For more information see https://aka.ms/fsharp-console-apps
let lines = IO.File.ReadLines("input.txt")

// read each line
let (first_list, second_list) =
    lines
    |> Seq.map (fun str ->
        let parts = str.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
        (int parts.[0], int parts.[1]))
    |> Seq.toList
    |> List.unzip

let sorted_first_list = first_list |> List.sort
let sorted_second_list = second_list |> List.sort

// iterate over the second list, create a hashmap where the number is the key and the value is the number of times that value appears
let counts = sorted_second_list |> Seq.countBy id |> Map.ofSeq
// ok for each input in the first list, we need to determine how many times it appears in the second list
// iterate over the first list, for each element in the first list
let res =
    sorted_first_list
    |> Seq.map (fun k ->
        match Map.tryFind k counts with
        | Some count -> k * count
        | None -> 0)
    |> Seq.sum


printfn "%d" res
