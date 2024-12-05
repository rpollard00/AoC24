open System.IO

let input = File.ReadLines("input_test.txt") |> Seq.toList
let break_pt = input |> List.findIndex (fun e -> e.Length = 0)
let (rules, pagesr) = input |> List.splitAt break_pt
let pages = pagesr[1..] |> List.map (fun pr -> pr.Split(','))

let rules_tups =
    rules
    |> List.map (fun r ->
        let lst = r.Split '|'
        (lst[0], lst[1]))

let rules_map =
    rules_tups
    |> List.groupBy fst
    |> List.map (fun (l, r) -> l, List.map snd r)
    |> Map.ofList

let has_next_page k next (mp: Map<string, list<string>>) =
    match Map.tryFind k mp with
    | Some v -> List.contains next v
    | None -> false

let rec valid_pgs (pgs: string array) =
    if pgs.Length <= 1 then
        true
    else
        let has_next = has_next_page pgs[0] pgs[1] rules_map
        if has_next = true then valid_pgs pgs[1..] else false

let valid_pages = pages |> List.filter (valid_pgs)
let middle_page_sum = valid_pages |> List.sumBy (fun p -> int p[p.Length / 2])

printfn "%d" middle_page_sum
