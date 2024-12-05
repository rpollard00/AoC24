open System.IO

let input = File.ReadLines("input.txt") |> Seq.toList
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

let rev_rules_map =
    rules_tups
    |> List.groupBy snd
    |> List.map (fun (l, r) -> l, List.map fst r)
    |> Map.ofList

let has_next_page k next (mp: Map<string, list<string>>) =
    match Map.tryFind k mp with
    | Some v -> List.contains next v
    | None -> false

let num_matches row lst =
    row |> Array.filter (fun i -> List.contains i lst) |> Array.length

let get_pos pg row rmap =
    match Map.tryFind pg rmap with
    | Some v -> num_matches row rmap[pg]
    | None -> 0

let rec valid_filter (pgs: string array) =
    if pgs.Length <= 1 then
        true
    else
        let has_next = has_next_page pgs[0] pgs[1] rules_map
        if has_next = true then valid_filter pgs[1..] else false

let invalid_filter pgs = valid_filter pgs = false
let invalid_pages = pages |> List.filter (invalid_filter)

let res =
    invalid_pages
    |> List.sumBy (fun p ->
        let mid_idx = p.Length / 2

        let mid_val =
            p
            |> Array.find (fun v ->
                let pos = get_pos v p rev_rules_map
                pos = mid_idx)

        int mid_val)

printfn "%d" res
