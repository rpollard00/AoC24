open System.IO

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

let rot_cw (grd: char[,]) =
    let rows = Array2D.length1 grd
    let cols = Array2D.length2 grd
    Array2D.init cols rows (fun i j -> grd[j, cols - 1 - i])

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

type Move =
    | Go
    | Stop

let move_right_until_collision sr sc (grd: char[,]) =
    let width = Array2D.length2 grd

    let rec check_pos c =
        if c >= width then
            (sr, c - 1, abs ((c - 1) - sc), Stop)
        elif grd[sr, c] = '#' then
            // printfn "found # at r %d, c %d" sr c
            (sr, c - 1, abs ((c - 1) - sc), Go)
        else
            grd[sr, c] <- 'X'
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

let grid =
    File.ReadLines("input_test.txt")
    |> Seq.toList
    |> str_grid_to_array2D
    |> rot_ccw
    |> rot_ccw

let p grd =
    grd |> Array2D.iter (fun l -> printfn "%s" l)

let start = grid |> find2D (fun a -> a = '^') |> Seq.head
let sr, sc = start
grid[sr, sc] <- 'X'

let rec move_until_exit sr sc acc move grd =
    if move = Stop then
        grd
    else
        let grd_rot = grd |> rot_cw
        let rr, rc = get_cw_rotated_pos sr sc grd_rot
        // printfn "Rotated start: %d %d" rr rc
        let (nsr, nsc, moves, mv) = move_right_until_collision rr rc grd_rot
        grd_rot |> grid_prt
        printfn "  "
        move_until_exit nsr nsc moves mv grd_rot

let marked_grid = move_until_exit sr sc 0 Go grid

let ans = marked_grid |> find2D (fun c -> c = 'X') |> Seq.length
printfn "%d" ans
