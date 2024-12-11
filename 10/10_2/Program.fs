open System.IO

let input_to_array2D (input: seq<string>) =
    let strs = input |> Seq.toList
    let length = strs.Length
    let width = strs[0].Length
    Array2D.init length width (fun r c -> int (strs[r][c]) - int '0')

let grid_prt grd =
    grd
    |> Array2D.iteri (fun _ c v ->
        printf "%d" v

        if c = Array2D.length2 grd - 1 then
            printfn "")

let filteri_array2D cond grid =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    seq {
        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                if cond grid[r, c] then
                    yield (r, c, grid[r, c])

    }
    |> List.ofSeq

let find_trails start_pos (grid: int[,]) =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    let rec check_trail pos last =
        let (_, _, lv) = last
        let (r, c, _) = pos

        if r < 0 || c < 0 || c > cols - 1 || r > rows - 1 then
            0
        else
            let curr = grid[r, c]

            if lv + 1 <> curr && lv <> 99 then
                0
            elif curr = 9 then
                1
            else
                let n = (r - 1, c, -1)
                let s = (r + 1, c, -1)
                let e = (r, c + 1, -1)
                let w = (r, c - 1, -1)

                let cp = (r, c, curr)
                check_trail n cp + check_trail s cp + check_trail e cp + check_trail w cp

    check_trail start_pos (0, 0, 99)

let grid = File.ReadLines("input.txt") |> input_to_array2D
let start_positions = grid |> filteri_array2D (fun c -> c = 0)
let ans = start_positions |> List.sumBy (fun position -> find_trails position grid)

printfn "%d" ans
