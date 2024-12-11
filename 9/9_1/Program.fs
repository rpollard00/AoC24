open System.IO

let id_from_idx n d =
    match d with
    | 0UL -> 0UL
    | _ -> n / d


let swap (arr: 'a array) i j =
    let temp = arr[i]
    arr[i] <- arr[j]
    arr[j] <- temp

let uncompressed_filesystem =
    File.ReadLines("input.txt")
    |> Seq.head
    |> List.ofSeq
    |> List.indexed
    |> List.map (fun (i, c) ->
        if i % 2 = 0 then
            List.replicate (int c - int '0') (string (id_from_idx (uint64 i) 2UL))
        else
            List.replicate (int c - int '0') "."

    )
    |> List.concat

let fs_string = uncompressed_filesystem |> List.map (fun f -> sprintf "%s" f)

// printfn "%s" fs_string
let uf_arr = uncompressed_filesystem |> Array.ofList

let mutable e = (uf_arr.Length - 1)

for s in 0 .. uf_arr.Length - 1 do
    if uf_arr[s] = "." then
        while uf_arr[e] = "." && e > s do
            e <- (e - 1)

        if e > s then
            swap uf_arr s e

let ans =
    uf_arr
    |> Array.indexed
    |> Array.sumBy (fun (i, v) ->
        if v <> "." then
            uint64 i * (System.UInt64.Parse(v))
        else
            0UL)


printfn "%d" ans
