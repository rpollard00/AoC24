open System.IO

let id_from_idx n d =
    match d with
    | 0UL -> 0UL
    | _ -> n / d

let fstf (v, _, _, _) = v
let sndf (_, v, _, _) = v
let thd (_, _, v, _) = v
let frt (_, _, _, v) = v

let insert_in_space lst itm =
    let rec build acc remaining =
        match remaining with
        | [] -> None
        | head :: tail ->
            let (curr_index, curr_size, _, curr_occupied) = head
            let (_, itm_size, itm_id, itm_occupied) = itm

            if not (itm_occupied) || curr_occupied || itm_size > curr_size then
                build (head :: acc) tail
            else
                let moved_itm = (curr_index, itm_size, itm_id, itm_occupied)

                if curr_size > itm_size then
                    let space = (curr_index + itm_size, curr_size - itm_size, "", false)
                    Some(List.rev acc @ [ moved_itm; space ] @ tail)
                else
                    Some(List.rev acc @ [ moved_itm ] @ tail)

    build [] lst

let replace_with_space idx lst =
    let rec build acc remaining =
        match remaining with
        | [] -> List.rev acc
        | head :: tail ->
            let (i, s, _, _) = head

            if i = idx then
                let space = (i, s, "", false)
                build (space :: acc) tail
            else
                build (head :: acc) tail

    build [] lst

let combine_adjacent_free lst =
    let rec build acc remaining =
        match remaining with
        | [] -> List.rev acc
        | [ last ] -> List.rev (last :: acc)
        | a :: b :: tail ->
            let (a_i, a_s, _, a_o) = a
            let (b_i, b_s, _, b_o) = b

            if not (a_o) && not (b_o) then
                let combined = (a_i, a_s + b_s, "", a_o)
                build acc (combined :: tail)
            else
                build (a :: acc) (b :: tail)

    build [] lst

let multiply_idx_size idx size id =
    let range = [ idx .. (idx + size - 1) ]
    range |> List.fold (fun acc v -> acc + (uint64 v * id)) 0UL

let try_compact_list lst =
    let rec try_next original remaining processed =
        match remaining with
        | [] ->
            // printfn "nothing to process"
            original
        | last :: rest ->
            let list_without_item = original |> replace_with_space (fstf last)

            // printfn "candidate item %A" last
            // printfn "try_compact %A" list_without_item

            match insert_in_space list_without_item last with
            | Some new_list ->
                // printfn "compacted. new list: %A" new_list
                try_next (combine_adjacent_free new_list) rest (last :: processed)
            | None ->
                // printfn "not compacted item %A" last
                try_next (combine_adjacent_free original) rest (last :: processed)

    // printfn "Starting list: %A" lst
    // printfn "Reversed list for processing: %A" (List.rev lst)
    let reversed = List.rev lst
    try_next lst reversed []

// (index, size, ID, occupied)
let filesystem =
    File.ReadLines("input.txt")
    |> Seq.head
    |> List.ofSeq
    |> List.indexed
    |> List.fold
        (fun (acc, offset) (i, c) ->
            let size = int c - int '0'

            if i % 2 = 0 then
                let item = (offset, size, (string (id_from_idx (uint64 i) 2UL)), true)
                (item :: acc, offset + size)
            else
                let item = (offset, size, "", false)
                (item :: acc, offset + size))
        ([], 0)
    |> fst
    |> List.rev
    |> List.filter (fun i -> (sndf i) <> 0)

let compacted = filesystem |> try_compact_list

let ans =
    compacted
    |> List.sumBy (fun c ->
        let (i, s, ident, o) = c

        if o then
            multiply_idx_size i s (System.UInt64.Parse(ident))
        else
            0UL)

printfn "%d" ans
