open System.IO
open System.Text.RegularExpressions

type SubmarineDirection =
    | Up of depth: int
    | Down of depth: int
    | Forward of movement: int

let (|Distance|_|) str =
   match Regex("(?<distance>\d+)").Match(str) with
   | m when m.Success -> Some(int m.Groups["distance"].Value)
   | _ -> None

let (|ParseDirection|) str =
    match str with
    | Distance value when str.StartsWith("forward") -> Forward value
    | Distance value when str.StartsWith("up") -> Down value
    | Distance value when str.StartsWith("down") -> Up value
    | _ -> failwith $"Could not parse {str}"

let CalculatePosition directions =
    directions
    |> List.fold
        (fun acc current -> match current with
                            | Up x -> {| HorizontalPosition = acc.HorizontalPosition; Depth = acc.Depth; Aim = acc.Aim + x |}
                            | Down x -> {| HorizontalPosition = acc.HorizontalPosition; Depth = acc.Depth; Aim = acc.Aim - x |}
                            | Forward x -> {| HorizontalPosition = acc.HorizontalPosition + x; Depth = acc.Depth + (acc.Aim * x); Aim = acc.Aim |} ) {| HorizontalPosition = 0; Depth = 0; Aim = 0  |}

let ReadPuzzleInput file =
    file
    |> File.ReadAllLines
    |> Array.where (fun f -> (System.String.IsNullOrEmpty(f) |> not))
    |> Array.map (fun f -> match f with
                           | ParseDirection f -> f)
    |> Array.toList

[<EntryPoint>]
let main argv =
    printfn "*- Advent of Code 2021 -*"
    printfn "|     Day 02   Dive     |"
    printfn "-------------------------"
    match Array.length argv with
    | x when x = 1 ->
        match File.Exists(argv[0]) with
        | true ->
            let input = ReadPuzzleInput argv[0]
            let finalPosition = CalculatePosition input
            printf $"[**]\nHorizontal Position: {finalPosition.HorizontalPosition},\nDepth: {finalPosition.Depth}\nAim: {finalPosition.Aim}\nMultiplied: {finalPosition.HorizontalPosition * finalPosition.Depth}\n"
            0
        | _ -> failwith "File not found"
    | _ -> failwith "Usage: ./dotnet <path-to-puzzle-input>"
