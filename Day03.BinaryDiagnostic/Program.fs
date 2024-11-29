open System
open System.Collections.Generic
open System.IO

let BinToDec (binary: int seq) : int =
    binary
    |> Seq.rev
    |> Seq.zip (seq { 0 .. (binary |> Seq.length) - 1 })
    |> Seq.fold (fun acc curr -> acc ||| ((curr |> snd) <<< (curr |> fst))) 0

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
                    (acc[curr] <- f + 1)
                    acc
                | (false, _) ->
                    (acc.Add(curr, 1))
                    acc)
            (Dictionary<int, int>())

    member this.MostCommonBitsPerColumn() : int[] =
        seq { 0 .. this.Rows[0].Length - 1 }
        |> Seq.map (fun f ->
            let count = this.CountBitsForColumn(f)

            match ((count.TryGetValue 0), (count.TryGetValue 1)) with
            | (true, z), (true, o) -> if z > o then 0 else 1
            | (false, _), (true, _) -> 1
            | (true, _), (false, _) -> 0
            | (false, _), (false, _) -> failwithf "empty column")
        |> Seq.toArray

    member this.Gamma() : int =
        this.MostCommonBitsPerColumn() |> BinToDec

    member this.Epsilon() : int =
        this.MostCommonBitsPerColumn() |> Seq.map (fun f -> f ^^^ 1) |> BinToDec
    
    member this.PowerConsumption() : int =
        this.Gamma() * this.Epsilon()

let ReadPuzzleInput file =
    file
    |> File.ReadAllLines
    |> Array.where (fun f -> (String.IsNullOrEmpty(f) |> not))
    |> Array.map (fun f -> f |> Seq.map (fun s -> Int32.Parse(s |> string)) |> Seq.toArray)
    |> DiagnosticReport

[<EntryPoint>]
let main argv =
    printfn "--- Day 3: Binary Diagnostic ---"

    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            let input = ReadPuzzleInput argv[0]
            printfn "Gamma %A" (input.Gamma())
            printfn "Epsilon %A" (input.Epsilon())
            printfn "Power Consumption: %A" (input.PowerConsumption())
            0
        | _ -> failwith "File not found"
    | _ -> failwith "Usage: ./dotnet <path-to-puzzle-input>"
