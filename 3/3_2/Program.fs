open System
open System.Text.RegularExpressions

let remove_between str =
    let pattern = @"don\'t\(\).*?do\(\)"
    Regex.Replace(str, pattern, "")

let remove_last str =
    let pattern = @"don\'t\(\).*"
    Regex.Replace(str, pattern, "")

let regex_str = @"mul\((\d+),(\d+)\)"

let parse_vals regex str =
    Regex.Matches(str, regex)
    |> Seq.map (fun m -> (int m.Groups.[1].Value, int m.Groups.[2].Value))
    |> Seq.toList

let input =
    IO.File.ReadLines("input.txt")
    |> String.concat "\r"
    |> remove_between
    |> remove_last

let vals = parse_vals regex_str input
// vals |> List.iter (fun (a, b) -> printfn "%d %d" a b)
let ans = vals |> List.sumBy (fun (a, b) -> int a * int b)

printfn "%d" ans
