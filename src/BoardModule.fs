module BoardModule

open System

type CellStatus = Empty | HasX | HasO
type Index = One | Two | Three

type Row = {Cells: Map<Index, CellStatus>}

type Board = {Rows: Map<Index, Row>}

let emptyRow = {Cells = Map.empty}

let getRow index board =
    board.Rows |> Map.tryFind index |> Option.defaultValue emptyRow

let getCellInRow index row  =
    row.Cells |> Map.tryFind index |> Option.defaultValue Empty

let getCell rowIndex columnIndex board = 
    let row = getRow rowIndex board
    getCellInRow columnIndex row

let allCellsInRow row =
    seq {
        yield getCellInRow One row
        yield getCellInRow Two row
        yield getCellInRow Three row
        }

let allCells board =
    seq {
        yield! getRow One board |> allCellsInRow
        yield! getRow Two board |> allCellsInRow
        yield! getRow Three board |> allCellsInRow
        }

let updateCellInRow row index newStatus =
    {Cells = Map.add index newStatus row.Cells}

let updateRowInBoard board index newRow =
    {Rows = Map.add index newRow board.Rows}

let updateCell board rowIndex columnIndex newValue =
    let updatedRow = updateCellInRow (getRow rowIndex board) columnIndex newValue 
    updateRowInBoard board rowIndex updatedRow

let emptyBoard =
    {Rows = Map.empty}

let isFull board =
    board |> allCells |> Seq.forall (fun c -> c = HasX || c = HasO)

let formatCell cell =
    match cell with
    | HasX -> "X"
    | HasO -> "O"
    | Empty -> "_"

let writeBoard board write =
    let writeRow rowIndex =
        let row = getRow rowIndex board
        getCellInRow One row |> formatCell |> write
        write " "
        getCellInRow Two row |> formatCell |> write
        write " "
        getCellInRow Three row |> formatCell |> write
    writeRow One
    write Environment.NewLine
    writeRow Two
    write Environment.NewLine
    writeRow Three
    write Environment.NewLine

let allLinesIndexes =
    seq {
            //Horizontal
            yield [One, One; One, Two; One, Three ]
            yield [Two, One; Two, Two; Two, Three ]
            yield [Three, One; Three, Two; Three, Three ]

            //Vertical
            yield [One, One; Two, One; Three, One ]
            yield [One, Two; Two, Two; Three, Two ]
            yield [One, Three; Two, Three; Three, Three ]

            //Diagonal
            yield [One, One; Two, Two; Three, Three ]
            yield [One, Three; Two, Two; Three, One ]
        }

let allLineValues board =
    let cell rowIndex columnIndex = getCell rowIndex columnIndex board
    allLinesIndexes |> Seq.map (fun line -> line |> List.map (fun (r, c) -> cell r c))

let lineIsFullOf line status =
    line |> List.forall (fun s -> s = status)

let anyLineIsFullOf board status =
    board |> allLineValues |> Seq.exists (fun line -> lineIsFullOf line status)

