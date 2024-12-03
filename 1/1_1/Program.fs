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

let diff_list = List.zip sorted_first_list sorted_second_list

let res = diff_list |> Seq.map (fun (a, b) -> Math.Abs(a - b)) |> Seq.sum

printfn "%d" res
