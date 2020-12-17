module GameModule

open BoardModule
open System

type Player = PlayerX | PlayerO

type GameState = {Board: Board; CurrentPlayer: Player;}

type Winner = PlayerXWon | PlayerOWon | Draw

type PlayTurnResult =
    | GameEnded of result: Winner * board: Board
    | GameNotEneded of newGameState: GameState * message: string option

let getWinner board =
    if anyLineIsFullOf board CellStatus.HasX then
        Some PlayerXWon
    elif anyLineIsFullOf board CellStatus.HasO then
        Some PlayerOWon
    elif isFull board then
        Some Draw
    else
        None

let switchPlayer player =
    if player = PlayerX then PlayerO else PlayerX

let playTurn state rowIndex columnIndex =
    let cellValue = getCell rowIndex columnIndex state.Board
    if cellValue <> CellStatus.Empty then
        GameNotEneded (state, Some "Cell is not empty")
    else
        let newCellValue = if state.CurrentPlayer = PlayerX then HasX else HasO
        let updatedBoard = updateCell state.Board rowIndex columnIndex newCellValue
        let winResult = getWinner updatedBoard
        match winResult with
        | Some result -> GameEnded(result, updatedBoard)
        | None -> GameNotEneded({Board = updatedBoard; CurrentPlayer = switchPlayer state.CurrentPlayer}, None)

let rec readIntFromConsole readConsole writeConsole message validate =
    let handleInvalid () = 
        writeConsole "Invalid value"
        readIntFromConsole readConsole writeConsole message validate
    writeConsole message
    let userInput: string = readConsole ()
    match Int32.TryParse userInput with
    | true, i ->
        if validate i then
            i
        else
            handleInvalid ()
    | _ ->
        handleInvalid ()

let toIndex i =
    match i with
    | 1 -> One
    | 2 -> Two
    | 3 -> Three
    | _ -> raise (Exception "Invalid value")

let playTurn2 state readConsole writeConsole  =
    let writeLine line = writeConsole (line + Environment.NewLine)
    let validateIndex i = i > 0 && i < 4
    let playerSymbol = if state.CurrentPlayer = PlayerX then "X" else "O"
    writeLine ("Player " + playerSymbol + "'s turn")
    let row = readIntFromConsole readConsole writeLine "Please specify row:" validateIndex |> toIndex
    let column = readIntFromConsole readConsole writeLine "Please specify column:" validateIndex |> toIndex
    playTurn state row column
    
let playGame readConsole writeConsole =
    let writeLine line = writeConsole (line + Environment.NewLine)
        
    let rec next state =
        match playTurn2 state readConsole writeConsole with
        | GameEnded (winner, board) ->
            writeBoard board writeConsole
            writeLine "Game ended"
            let winMessage = match winner with
                             | PlayerXWon -> "Player X won"
                             | PlayerOWon -> "Player O won"
                             | Draw -> "No winner!" 
            writeLine winMessage
            
        | GameNotEneded (newState, message) ->
            writeBoard newState.Board writeConsole
            match message with
            | Some m -> writeLine m
            | _ -> ()
            next newState
    next {Board = emptyBoard; CurrentPlayer = PlayerX}

