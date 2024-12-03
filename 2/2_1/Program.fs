open System

let magnitude_in_range (a: int) (b: int) min max =
    let diff = Math.Abs(a - b)
    if diff >= min && diff <= max then true else false

let is_increasing (arr: array<int>) =
    arr
    |> Array.pairwise
    |> Array.forall (fun (a, b) -> a < b && (magnitude_in_range a b 1 3))

let is_decreasing (arr: array<int>) =
    arr
    |> Array.pairwise
    |> Array.forall (fun (a, b) -> a > b && (magnitude_in_range a b 1 3))

let rec is_monotonic_increasing (input_array: array<int>) =
    if input_array.Length <= 1 then
        1
    else if
        input_array.[0] < input_array.[1]
        && magnitude_in_range input_array.[0] input_array.[1] 1 3
    then
        is_monotonic_increasing input_array.[1..]
    else
        0

let rec is_monotonic_decreasing (input_array: array<int>) =
    if input_array.Length <= 1 then
        1
    else if
        input_array.[0] > input_array.[1]
        && magnitude_in_range input_array.[0] input_array.[1] 1 3
    then
        is_monotonic_increasing input_array.[1..]
    else
        0
// each report should be a list of ints
let reports =
    IO.File.ReadLines("input.txt")
    |> Seq.map (fun s ->
        let line = s.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)
        Array.map (fun a -> int a) line)

// reports |> Seq.iter (fun a -> Array.iter (fun i -> printfn "%d" i) a)
let printArray arr =
    Array.map string arr |> String.concat " " |> printfn "%s"

let res =
    reports
    |> Seq.map (fun a ->
        printArray a
        let increasing = is_increasing a
        let decreasing = is_decreasing a

        let ans = if increasing then 1 else 0 + if decreasing then 1 else 0

        printfn "This answer: %d" ans

        ans)
    |> Seq.sum

printfn "%d" res
