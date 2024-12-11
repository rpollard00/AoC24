open System
open System.IO

let split_stone (stone: string) =
    let one = stone[0 .. stone.Length / 2 - 1]

    let two =
        let s = stone[stone.Length / 2 .. stone.Length]

        if s.Length > 0 then
            let trimmed_s = s.TrimStart('0')
            if trimmed_s.Length = 0 then "0" else trimmed_s
        else
            s

    [ one; two ]

let multiply_stone stone =
    let stone_val = UInt64.Parse(stone)
    let new_stone = stone_val * 2024UL
    [ string new_stone ]

let transform stone =
    match stone with
    | s when s = "0" -> [ "1" ]
    | s when s.Length % 2 = 0 -> split_stone s
    | s -> multiply_stone s

let transform_once stones =
    stones |> List.map (fun s -> transform s) |> List.concat

let rec execute num_transforms stones =
    match num_transforms with
    | 0 -> stones
    | _ -> execute (num_transforms - 1) (transform_once stones)

let input = File.ReadLines("input.txt") |> Seq.head
let input_list = input.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
let stones = input_list |> List.ofArray

let ans = stones |> execute 25 |> List.length

printfn "%d" ans
