open System.IO

let grid = File.ReadLines("input.txt") |> Seq.toArray

let width = grid.[0].Length - 1
let height = grid.Length - 1

let xmas = [ 'M'; 'A'; 'S' ]
let n = [ (-1, 0); (-2, 0); (-3, 0) ]
let s = [ (1, 0); (2, 0); (3, 0) ]
let e = [ (0, 1); (0, 2); (0, 3) ]
let w = [ (0, -1); (0, -2); (0, -3) ]
let nw = [ (-1, -1); (-2, -2); (-3, -3) ]
let ne = [ (-1, 1); (-2, 2); (-3, 3) ]
let sw = [ (1, -1); (2, -2); (3, -3) ]
let se = [ (1, 1); (2, 2); (3, 3) ]

let card = [ n; s; e; w; nw; ne; sw; se ]
let xmas_card = card |> List.map (fun dir -> List.zip xmas dir)

let x_s =
    seq {
        for r in 0..height do
            for c in 0..width do
                if grid.[r].[c] = 'X' then
                    yield (r, c)
    }
    |> List.ofSeq


let is_char (x: char) ((r, c): int * int) =
    if r > width || c > height then false
    elif r < 0 || c < 0 then false
    elif grid.[r].[c] = x then true
    else false


let is_xmas dir sr sc =
    dir |> List.forall (fun (v, (r, c)) -> is_char v (sr + r, sc + c))

let sum_xmas xr xc =
    xmas_card |> List.sumBy (fun d -> if is_xmas d xr xc then 1 else 0)

let res = x_s |> List.sumBy (fun (xr, xc) -> sum_xmas xr xc)

printfn "%d" res
