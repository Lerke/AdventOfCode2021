open System
open System.IO

let Column (col: int, arr: 'T array array) = arr |> Array.map (fun f -> f[col])

type BingoBoard(board, markedBoard) =
    member val private Board: int array array = board
    member val private MarkedBoard: bool array array = markedBoard

    member val private IndexNumbers: (int * int * int) array =
        (board
         |> Seq.zip (seq { 0 .. board.Length - 1 })
         |> Seq.map (fun (x, f) -> f |> Seq.zip (seq { 0 .. f.Length - 1 }) |> Seq.map (fun (y, n) -> (x, y, n)))
         |> Seq.collect id
         |> Seq.toArray)

    new(board: string array) =
        let numberBoard =
            board
            |> Array.map (fun f ->
                f.Split(" ")
                |> Array.filter (fun s -> String.IsNullOrWhiteSpace(s) = false)
                |> Array.map Int32.Parse)

        let markedBoard =
            numberBoard |> Array.map (fun f -> f |> Array.map (fun _ -> false))

        BingoBoard(numberBoard, markedBoard)

    member this.NumberIndices(number: int) =
        this.IndexNumbers
        |> Array.filter (fun (_, _, z) -> z = number)
        |> Array.map (fun (x, y, _) -> (x, y))

    member this.MarkNumber(number: int) =
        this.NumberIndices(number)
        |> Array.iter (fun (x, y) -> markedBoard[x][y] <- true)

    member this.IsWinner() =
        let rowWinner =
            this.MarkedBoard
            |> Array.exists (fun f -> f |> Array.forall (fun p -> p = true))

        match rowWinner with
        | true -> true
        | false ->
            let columnWinner =
                seq { 0 .. this.MarkedBoard[0].Length - 1 }
                |> Seq.map (fun f -> Column(f, this.MarkedBoard))
                |> Seq.exists (fun f -> f |> Array.forall (fun p -> p = true))

            match columnWinner with
            | true -> true
            | false -> false

    member this.BoardScore() =
        this.IndexNumbers
        |> Array.filter (fun (x, y, _) -> this.MarkedBoard[x][y] = false)
        |> Array.sumBy (fun (_, _, z) -> z)

type BingoGame(boards: BingoBoard array) =
    member val private Boards = boards

    member private this.DrawNumber(number: int) =
        boards |> Array.iter _.MarkNumber(number)

    member private this.WinningBoards() = boards |> Array.filter _.IsWinner()

    member this.FindFirstWinningBoard(draw: int array, ?performedDraw: int list) =
        let pDraw =
            match performedDraw with
            | Some p -> p
            | None -> []

        match draw with
        | x when x.Length > 0 ->
            this.DrawNumber(x[0])

            match this.WinningBoards() with
            | [||] -> this.FindFirstWinningBoard(draw |> Array.skip 1, pDraw @ [ x[0] ])
            | boards ->
                Some(
                    {| Winners = boards[0]
                       Draw = pDraw @ [ x[0] ] |}
                )
        | _ -> None

printfn "--- Day 04: Giant Squid ---"

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        let lines = File.ReadAllLines file
        let draw = lines[0].Split(",") |> Array.map Int32.Parse

        let boards =
            lines
            |> Array.skip 2
            |> Array.filter (fun f -> String.IsNullOrWhiteSpace(f) = false)
            |> Array.chunkBySize 5
            |> Array.map BingoBoard

        let game = BingoGame boards
        let winners = game.FindFirstWinningBoard(draw)

        let winningSum =
            match winners with
            | Some x -> x.Winners.BoardScore() * (x.Draw |> List.last)
            | None -> 0
            
        printfn $"⭐\Final score:\t%A{winningSum}"

        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
