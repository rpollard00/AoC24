open System
open System.Text.RegularExpressions

let input = IO.File.ReadLines("input.txt") |> String.concat "\n"

let parse_vals regex str =
    Regex.Matches(str, regex)
    |> Seq.map (fun m -> (int m.Groups.[1].Value, int m.Groups.[2].Value))
    |> Seq.toList

let regex_str = @"mul\((\d+),(\d+)\)"

let vals = parse_vals regex_str input

vals |> List.iter (fun (a, b) -> printfn "%d %d" a b)
let ans = parse_vals regex_str input |> List.sumBy (fun (a, b) -> int a * int b)

printfn "%d" ans
