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

let remove_el (arr: array<int>) i =
    let len = arr.Length

    if i < 0 || i >= len then arr
    elif i = 0 then arr.[1..]
    elif i = len - 1 then arr.[0 .. len - 2]
    else Array.concat [ arr.[0 .. i - 1]; arr.[i + 1 ..] ]


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
        let a_s = [ 0 .. a.Length - 1 ] |> List.map (fun i -> remove_el a i)

        let decreasing = a_s |> List.exists (fun a -> is_decreasing a)
        let increasing = a_s |> List.exists (fun a -> is_increasing a)

        let ans = if increasing then 1 else 0 + if decreasing then 1 else 0

        printfn "This answer: %d" ans

        ans)
    |> Seq.sum

printfn "%d" res
