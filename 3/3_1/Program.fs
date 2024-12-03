﻿open System
open System.Text.RegularExpressions


let regex_str = @"mul\((\d+),(\d+)\)"

let parse_vals regex str =
    Regex.Matches(str, regex)
    |> Seq.map (fun m -> (int m.Groups.[1].Value, int m.Groups.[2].Value))
    |> Seq.toList

let input = IO.File.ReadLines("input.txt") |> String.concat "\r"
let vals = parse_vals regex_str input
let ans = parse_vals regex_str input |> List.sumBy (fun (a, b) -> int a * int b)

printfn "%d" ans
