open System.IO

let ReadPuzzleInput file =
    file
    |> File.ReadAllLines
    |> Array.map (int)
    |> Array.toList

let NumberOfIncreases (input: int list) =
    input
    |> List.skip 1
    |> List.zip (input |> List.rev |> List.skip 1 |> List.rev)
    |> List.fold (fun acc (previous, current) -> if current > previous then acc + 1 else acc) 0

[<EntryPoint>]
let main argv =
    printfn "*- Advent of Code 2021 -*"
    printfn "|  Day 01  Sonar Sweep  |"
    printfn "-------------------------"
    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            let numberOfIncreases = NumberOfIncreases (ReadPuzzleInput argv[0])
            printfn $"[*] Number of increases {numberOfIncreases}"
            0
        | _ -> failwith "File not found"
    | _ -> failwith "Usage: ./dotnet <path-to-puzzle-input>"
