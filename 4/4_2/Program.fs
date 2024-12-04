open System.IO

let grid = File.ReadLines("input.txt") |> Seq.toArray

let width = grid.[0].Length - 1
let height = grid.Length - 1

// ok so instead we need to find the 'A's
// then from a given A we need to check the diagonals
// we can have a couple permutations
// check nw and se -> if nw is S then se must be M or if nw is M then se must be S
// check ne and sw -> if ne is S then sw must be M or if nw is S then se must be M
// both the above make a valid xmas
//let xmas = [ 'M'; 'A'; 'S' ]
//let n = [ (-1, 0); (-2, 0); (-3, 0) ]
//let s = [ (1, 0); (2, 0); (3, 0) ]
//let e = [ (0, 1); (0, 2); (0, 3) ]
//let w = [ (0, -1); (0, -2); (0, -3) ]
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

let a_s_collector = ResizeArray<int * int>()

for r in 0..height do
    for c in 0..width do
        if grid.[r].[c] = 'A' then
            a_s_collector.Add((r, c))

let a_s = a_s_collector |> List.ofSeq

let is_char (x: char) ((r, c): int * int) =
    if r > width || c > height then false
    elif r < 0 || c < 0 then false
    elif grid.[r].[c] = x then true
    else false

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
