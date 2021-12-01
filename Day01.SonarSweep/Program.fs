open System.IO

let ReadPuzzleInput file =
    file
    |> File.ReadAllLines
    |> Array.map int
    |> Array.toList

let rec CreateSlidingWindows input =
    match input with
    | x::x1::x2::xs -> ([ x; x1; x2 ] :: CreateSlidingWindows (x1::x2::xs))
    | _ -> []

let NumberOfIncreases input =
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
            let input = ReadPuzzleInput argv[0]
            let numberOfIncreases = NumberOfIncreases input

            let slidingWindows = CreateSlidingWindows input
            let sums = slidingWindows |> List.map(List.sum)
            let numberOfIncreasesWindowed = NumberOfIncreases sums

            printfn $"[*] Number of increases {numberOfIncreases}"
            printfn $"[**] Number of increased (windowed) {numberOfIncreasesWindowed}"
            0
        | _ -> failwith "File not found"
    | _ -> failwith "Usage: ./dotnet <path-to-puzzle-input>"
