open System

let lines = IO.File.ReadLines("input.txt")

let (first_list, second_list) =
    lines
    |> Seq.map (fun str ->
        let parts = str.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
        (int parts.[0], int parts.[1]))
    |> Seq.toList
    |> List.unzip

let sorted_first_list = first_list |> List.sort
let sorted_second_list = second_list |> List.sort

let counts = sorted_second_list |> Seq.countBy id |> Map.ofSeq
let res =
    sorted_first_list
    |> Seq.map (fun k ->
        match Map.tryFind k counts with
        | Some count -> k * count
        | None -> 0)
    |> Seq.sum


printfn "%d" res
