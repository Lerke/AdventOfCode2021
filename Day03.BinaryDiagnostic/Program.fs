open System
open System.Collections.Generic
open System.IO

let BinToDec (binary: int seq) : int =
    binary
    |> Seq.rev
    |> Seq.zip (seq { 0 .. (binary |> Seq.length) - 1 })
    |> Seq.fold (fun acc curr -> acc ||| ((curr |> snd) <<< (curr |> fst))) 0

let Column (i: int, rows: int array array) : int seq =
    seq { 0 .. rows.Length - 1 } |> Seq.map (fun f -> rows[f][i])

let MostCommonBit (count: Dictionary<int, int>) : int =
    match ((count.TryGetValue 0), (count.TryGetValue 1)) with
    | (true, z), (true, o) when z > o -> 0
    | (true, z), (true, o) when z < o -> 1
    | (true, z), (true, o) when z = o -> 1
    | (false, _), (true, _) -> 1
    | (true, _), (false, _) -> 0
    | (false, _), (false, _) -> failwithf "empty column"
    | _ -> failwith "Unsupported"

let CountBitsForColumn (i: int, rows: int array array) : Dictionary<int, int> =
    Column(i, rows)
    |> Seq.fold
        (fun acc curr ->
            match acc.TryGetValue curr with
            | true, f ->
                (acc[curr] <- f + 1)
                acc
            | false, _ ->
                acc.Add(curr, 1)
                acc)
        (Dictionary<int, int>())

type DiagnosticReport(rows: int array array) =
    member val Rows = rows

    member this.MostCommonBitsPerColumn() : int[] =
        seq { 0 .. this.Rows[0].Length - 1 }
        |> Seq.map (fun f ->
            let count = CountBitsForColumn(f, this.Rows)
            MostCommonBit(count))
        |> Seq.toArray

    member this.Gamma() : int =
        this.MostCommonBitsPerColumn() |> BinToDec

    member this.Epsilon() : int =
        this.MostCommonBitsPerColumn() |> Seq.map (fun f -> f ^^^ 1) |> BinToDec

    member this.PowerConsumption() : int = this.Gamma() * this.Epsilon()

    member this.LifeSupportRatingComponent(selector: int -> int -> bool, ?rows: int array array, ?column: int) =
        let rowsToUse =
            match rows with
            | Some r -> r
            | None -> this.Rows

        let columnToUse =
            match column with
            | Some c -> c
            | None -> 0

        match rowsToUse with
        | [| x |] -> BinToDec x
        | x ->
            rowsToUse
            |> (fun f ->
                let count = CountBitsForColumn(columnToUse, f)

                match MostCommonBit(count) with
                | i ->
                    this.LifeSupportRatingComponent(
                        selector,
                        x |> Array.filter (fun f -> selector f[columnToUse] i),
                        columnToUse + 1
                    ))

    member this.OxygenGeneratorRating() = this.LifeSupportRatingComponent((=))

    member this.Co2ScrubberRating() = this.LifeSupportRatingComponent((<>))

    member this.LifeSupportRating() =
        this.OxygenGeneratorRating() * this.Co2ScrubberRating()

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
            printfn $"⭐\tPower Consumption:\t%A{input.PowerConsumption()}"
            printfn $"⭐⭐\tLife Support Rating:\t%A{input.LifeSupportRating()}"
            0
        | _ -> failwith "File not found"
    | _ -> failwith "Usage: ./dotnet <path-to-puzzle-input>"
