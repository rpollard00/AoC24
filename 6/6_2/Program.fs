open System.IO

open FSharp.Collections.ParallelSeq

type Move =
    | Cycle
    | Go
    | Stop

type Direction =
    | North
    | East
    | South
    | West

let str_grid_to_array2D (strs: list<string>) =
    let length = strs.Length
    let width = strs[0].Length
    Array2D.init length width (fun r c -> strs[r][c])

let grid_prt grd =
    grd
    |> Array2D.iteri (fun _ c v ->
        printf "%c" v

        if c = Array2D.length2 grd - 1 then
            printfn "")

let next_dir =
    function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let dir_char =
    function
    | North -> 'N'
    | East -> 'E'
    | South -> 'S'
    | West -> 'W'

let is_dir_char ch =
    match ch with
    | 'N'
    | 'E'
    | 'S'
    | 'W' -> true
    | _ -> false

let rot_cw (grd: char[,]) =
    let rows = Array2D.length1 grd
    let cols = Array2D.length2 grd
    Array2D.init cols rows (fun i j -> grd[j, cols - 1 - i])

let rot_cw_dir (dir: Direction) (grd: char[,]) =
    let grd = rot_cw grd
    let new_dir = next_dir dir
    (grd, new_dir)

let rot_until_dir (target_dir: Direction) (dir: Direction, grd: char[,]) =
    if target_dir = dir then
        grd
    else
        let rec rot_until_cond g d =
            if d = target_dir then
                g
            else
                let new_gr, new_dir = rot_cw_dir d g
                rot_until_cond new_gr new_dir

        rot_until_cond grd dir


let rot_ccw (grd: char[,]) =
    let rows = Array2D.length1 grd
    let cols = Array2D.length2 grd
    Array2D.init cols rows (fun i j -> grd[rows - 1 - j, i])

let get_cw_rotated_pos (r: int) (c: int) (grd: char[,]) =
    let cols = Array2D.length2 grd
    (cols - 1 - c, r)

let get_ccw_rotated_pos (r: int) (c: int) (grd: char[,]) =
    let rows = Array2D.length1 grd
    (c, rows - 1 - r)

let move_right_until_collision sr sc ch (grd: char[,]) =
    let width = Array2D.length2 grd

    let rec check_pos c =
        if c >= width then
            (sr, c - 1, abs ((c - 1) - sc), Stop)
        elif grd[sr, c] = ch then
            (sr, c - 1, abs ((c - 1) - sc), Cycle)
        elif (grd[sr, c] = '#') then
            (sr, c - 1, abs ((c - 1) - sc), Go)
        else
            if not (is_dir_char grd[sr, c]) then
                grd[sr, c] <- ch

            check_pos (c + 1)

    check_pos sc


let find2D prd (grd: char[,]) =
    let rows = Array2D.length1 grd
    let cols = Array2D.length2 grd

    seq {
        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                if prd grd[r, c] then
                    yield (r, c)
    }


let p grd =
    grd |> Array2D.iter (fun l -> printfn "%s" l)


let rec move_until_exit sr sc dir move grd =
    if move = Stop then
        grd, dir
    else
        let grd_rot, new_dir = rot_cw_dir dir grd
        let rr, rc = get_cw_rotated_pos sr sc grd_rot
        let ch = dir_char new_dir

        // printfn "Rotated start: %d %d" rr rc
        let (nsr, nsc, moves, mv) = move_right_until_collision rr rc ch grd_rot
        move_until_exit nsr nsc new_dir mv grd_rot

let rec move_until_cycle sr sc dir move grd =
    if move = Stop then
        0
    elif move = Cycle then
        1
    else
        let grd_rot, new_dir = rot_cw_dir dir grd
        let rr, rc = get_cw_rotated_pos sr sc grd_rot
        let ch = dir_char new_dir

        let (nsr, nsc, moves, mv) = move_right_until_collision rr rc ch grd_rot
        move_until_cycle nsr nsc new_dir mv grd_rot

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let grid =
    File.ReadLines("input.txt")
    |> Seq.toList
    |> str_grid_to_array2D
    |> rot_ccw
    |> rot_ccw

let start = grid |> find2D (fun a -> a = '^') |> Seq.head
let sr, sc = start

let marked_grid_tup = move_until_exit sr sc North Go grid
let (marked_grid, marked_grid_dir) = marked_grid_tup

let visited = marked_grid |> find2D (fun c -> is_dir_char c)
let ans = visited |> Seq.length

let candidates =
    List.allPairs [ 0 .. Array2D.length1 grid - 1 ] [ 0 .. Array2D.length2 grid - 1 ]
    |> List.filter (fun l -> l <> start)

printfn "Candidates count %d" (Seq.length candidates)

let base_marked_grid = rot_until_dir North (marked_grid_dir, marked_grid)

let test_for_cycle candidate =
    let test_grid =
        Array2D.init (Array2D.length1 grid) (Array2D.length2 grid) (fun r c -> grid[r, c])

    let r, c = candidate
    test_grid[r, c] <- '#'

    move_until_cycle sr sc North Go test_grid

let candidates_list = candidates |> Seq.toList

let res = candidates_list |> PSeq.sumBy (fun c -> test_for_cycle c)

printfn "%d" res
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalSeconds
