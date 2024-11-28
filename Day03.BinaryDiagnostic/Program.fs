open System
open System.Collections.Generic
open System.IO

type DiagnosticReport(rows: int array array) =
    member val Rows = rows

    member this.Column(i: int) : int seq =
        seq { 0 .. this.Rows.Length - 1 } |> Seq.map (fun f -> this.Rows[f][i])

    member this.CountBitsForColumn(i: int) : Dictionary<int, int> =
        this.Column(i)
        |> Seq.fold
            (fun acc curr ->
                match acc.TryGetValue curr with
                | (true, f) ->
                    (acc[curr |> int] <- f + 1)
                    acc
                | (false, _) ->
                    (acc.Add(curr |> int, 1))
                    acc)
            (Dictionary<int, int>())


let ReadPuzzleInput file =
    file
    |> File.ReadAllLines
    |> Array.where (fun f -> (String.IsNullOrEmpty(f) |> not))
    |> Array.map(fun f -> f |> Seq.map(fun s -> s |> int) |> Seq.toArray)
    |> DiagnosticReport

[<EntryPoint>]
let main argv =
    printfn "--- Day 3: Binary Diagnostic ---"

    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            let input = ReadPuzzleInput argv[0]
            let colOne = input.CountBitsForColumn(0)
            printf "A"
            0
        | _ -> failwith "File not found"
    | _ -> failwith "Usage: ./dotnet <path-to-puzzle-input>"
