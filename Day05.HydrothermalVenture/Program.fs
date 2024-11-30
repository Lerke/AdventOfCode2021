open System
open System.IO

printfn "--- Day 05: Hydrothermal Venture ---"

type Position2 = (Int32 * Int32)

type Line(x1, y1, x2, y2) =
    member val P1: Position2 = (x1, y1)
    member val P2: Position2 = (x2, y2)

let IntersectionMap(lines: Line array) =
    0

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        let input =
            File.ReadAllLines file
            |> Seq.map _.Split("->")
            |> Seq.map (fun f -> f |> Array.filter (fun p -> String.IsNullOrWhiteSpace(p) = false))
            |> Seq.map (fun f ->
                seq { 0..1 }
                |> Seq.map (fun p -> f[p].Split(",") |> Array.map (Int32.Parse))
                |> Seq.toArray)
            |> Seq.map (fun f -> (f[0], f[1]))
            |> Seq.map (fun (p1, p2) -> Line(p1[0], p1[1], p2[0], p2[1]))

        printfn "A"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
