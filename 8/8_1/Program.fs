open System.IO
open System

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


let antenna_pos (grd: char[,]) =
    let height = Array2D.length1 grd - 1
    let width = Array2D.length2 grd - 1

    seq {
        for r in 0..height do
            for c in 0..width do
                let ch = grd[r, c]
                let isMatch = Char.IsAsciiLetterOrDigit(ch)

                if isMatch then
                    yield (ch, r, c)
    }
    |> List.ofSeq


let addvec v1 v2 =
    let (r1, c1) = v1
    let (r2, c2) = v2

    (r1 + r2, c1 + c2)

let subvec v1 v2 =
    let (r1, c1) = v1
    let (r2, c2) = v2

    (r1 - r2, c1 - c2)

let equalvec v1 v2 =
    let (r1, c1) = v1
    let (r2, c2) = v2

    r1 = r2 && c1 = c2

let (+^) v1 v2 = addvec v1 v2
let (-^) v1 v2 = subvec v1 v2
let (=^) v1 v2 = equalvec v1 v2

let dist_btw_pts v1 v2 =
    let (r1, c1) = v1
    let (r2, c2) = v2

    let dr = abs (r1 - r2)
    let dc = abs (c1 - c2)

    (dr, dc)

let get_lines_from_pts (pts: list<int * int>) =
    seq {
        for i in 0 .. List.length pts - 1 do
            for j in i .. List.length pts - 1 do
                if not (equalvec pts[i] pts[j]) then
                    let dst = dist_btw_pts pts[i] pts[j]
                    // printfn "lines from pts %A %A %A" pts[i] pts[j] dst
                    (pts[i], pts[j], dst)
    }
    |> List.ofSeq

let get_signed_dst (line: (int * int) * (int * int) * (int * int)) =
    let (p1, p2, d) = line
    let (dir_r, dir_c) = p2 -^ p1
    let (dr, dc) = d

    (sign dir_r * dr, sign dir_c * dc)

let gen_pts_along_line mx (line: (int * int) * (int * int) * (int * int)) =
    let (p1, p2, dst) = line
    let signed_dst = get_signed_dst line

    let negated_dst =
        let dr, dc = signed_dst
        -dr, -dc

    let (maxr, maxc) = mx

    let next_pt1 = p1 +^ signed_dst
    let next_pt2 = p2 +^ signed_dst
    let prev_pt1 = p1 +^ negated_dst
    let prev_pt2 = p2 +^ negated_dst

    let pts_along_line =
        next_pt1 :: next_pt2 :: prev_pt1 :: prev_pt2 :: []
        |> List.filter (fun p -> p <> p1 && p <> p2)
        |> List.filter (fun (r, c) -> r <= maxr && c <= maxc && r >= 0 && c >= 0)

    pts_along_line

let grid = File.ReadLines("input.txt") |> Seq.toList |> str_grid_to_array2D


let antenna_positions = antenna_pos grid

let antenna_position_map =
    antenna_positions
    |> List.groupBy (fun (ch, _, _) -> ch)
    |> Map.ofList
    |> Map.map (fun _ v -> v |> List.map (fun (_, r, c) -> (r, c)))

let antinodes =
    antenna_position_map
    |> Map.map (fun k v ->
        let lines = v |> get_lines_from_pts

        let pts_along_line =
            lines
            |> List.map (fun l -> l |> gen_pts_along_line (Array2D.length1 grid - 1, Array2D.length2 grid - 1))
            |> List.concat

        pts_along_line)
    |> Map.values
    |> List.concat
    |> List.distinct

let answer = antinodes.Length

printfn "%d" answer
