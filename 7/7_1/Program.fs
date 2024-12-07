open System.IO
open System

let input =
    File.ReadLines("input.txt")
    |> Seq.map (fun e -> e.Trim().Split(':', StringSplitOptions.RemoveEmptyEntries))

let lines =
    input
    |> Seq.map (fun a ->
        let result = int64 a[0]

        let values =
            a[1].Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (fun v -> int64 v)
            |> List.ofSeq

        (int64 result, values))
    |> List.ofSeq

let list_prt lst =
    lst |> List.iter (fun v -> printfn "%d" v)

let rec test_ops target (values: list<int64>) =
    match values with
    | [] -> target = 0L
    | [ last ] -> target = last
    | v1 :: v2 :: rest ->
        let add_res = v1 + v2
        let mul_res = v1 * v2

        test_ops target (add_res :: rest) || test_ops target (mul_res :: rest)

let valid =
    lines
    |> List.map (fun (res, vals) ->
        if test_ops res vals then
            printfn "%d %A valid" res vals
            res
        else
            printfn "%d %A invalid" res vals
            0)
    |> List.sum

printfn "%d" valid
