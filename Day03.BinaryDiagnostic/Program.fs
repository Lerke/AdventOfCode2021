open System
open System.IO

let BitCountAtPosition (input:char list list) position =
    input
    |> List.map (fun f -> f[position])
    |> List.groupBy id

let ReadPuzzleInput file =
    file
    |> File.ReadAllLines
    |> Array.where (fun f -> (String.IsNullOrEmpty(f) |> not))
    |> Array.map  List.ofSeq
    |> Array.toList

let bInt (input: char list) =
    (Convert.ToInt32((String.Concat(Array.ofList(input))), 2))

[<EntryPoint>]
let main argv =
    printfn "--- Day 3: Binary Diagnostic ---"
    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            let input = ReadPuzzleInput argv[0]
            let bits = seq { 0 .. (input |> List.head |> List.length) - 1 } |> Seq.toList
                       |> List.map (BitCountAtPosition input)
            let mostCommonBits = bits
                                 |> List.map (List.maxBy (snd >> List.length))
                                 |> List.map (fst)
            let leastCommonBits = bits
                                 |> List.map (List.minBy (snd >> List.length))
                                 |> List.map (fst)

            printfn "Most common bits: %i in decimal\nLeast common bits: %i in decimal\nPower consumption: %i"
                (mostCommonBits |> bInt)
                (leastCommonBits |> bInt)
                ((mostCommonBits |> bInt) * (leastCommonBits |> bInt))
            0
        | _ -> failwith "File not found"
    | _ -> failwith "Usage: ./dotnet <path-to-puzzle-input>"
