open System.IO

let grid = File.ReadLines("input.txt") |> Seq.toArray

let width = grid.[0].Length - 1
let height = grid.Length - 1

let nw = (-1, -1)
let ne = (-1, 1)
let sw = (1, -1)
let se = (1, 1)

type card = list<char * (int * int)>
type cards = list<card>
let l_r_a: card = [ 'M', nw; 'S', se ]
let l_r_b: card = [ 'S', nw; 'M', se ]
let r_l_a: card = [ 'M', ne; 'S', sw ]
let r_l_b: card = [ 'S', ne; 'M', sw ]

let l_card: cards = [ l_r_a; l_r_b ]
let r_card: cards = [ r_l_a; r_l_b ]

let a_s =
    seq {
        for r in 0..height do
            for c in 0..width do
                if grid.[r].[c] = 'A' then
                    yield (r, c)
    }
    |> List.ofSeq

let is_char x (r, c) =
    match r, c with
    | _ when r < 0 || c < 0 || r > width || c > height -> false
    | _ -> grid.[r].[c] = x

let is_half_mas sr sc (cards: cards) =
    cards
    |> List.exists (fun card -> card |> List.forall (fun (v, (r, c)) -> is_char v (sr + r, sc + c)))

let is_xmas sr sc =
    if is_half_mas sr sc l_card && is_half_mas sr sc r_card then
        1
    else
        0

let res = a_s |> List.sumBy (fun (xr, xc) -> is_xmas xr xc)

printfn "%d" res
