open System

type Tableau = {
    Data: double[,]
    Rows: int
    Cols: int
    VarNames: string[]
    RowNames: string[]
}

let printTableau (tableau: Tableau) =
    let colWidth = 10

    printfn ""
    printf "%-*s" colWidth ""
    for name in tableau.VarNames do
        printf "%*s " colWidth name
    printfn ""

    printfn "%s" (String.replicate ((colWidth+1) * (tableau.Cols+1)) "─")

    for i in 0 .. tableau.Rows - 1 do
        printf "%-*s" colWidth tableau.RowNames.[i]
        for j in 0 .. tableau.Cols - 1 do
            printf "%*.3f " colWidth tableau.Data.[i, j]
        printfn ""

    printfn "%s" (String.replicate ((colWidth+1) * (tableau.Cols+1)) "─")
    printfn ""

let pivot (tableau: Tableau) (pivotRow: int) (pivotCol: int) =
    let newData = Array2D.copy tableau.Data
    let pivotElement = tableau.Data.[pivotRow, pivotCol]

    for j in 0 .. tableau.Cols - 1 do
        newData.[pivotRow, j] <- tableau.Data.[pivotRow, j] / pivotElement

    for i in 0 .. tableau.Rows - 1 do
        if i <> pivotRow then
            let factor = tableau.Data.[i, pivotCol]
            for j in 0 .. tableau.Cols - 1 do
                newData.[i, j] <- tableau.Data.[i, j] - factor * newData.[pivotRow, j]

    let newRowNames = Array.copy tableau.RowNames
    newRowNames.[pivotRow] <- tableau.VarNames.[pivotCol]

    { tableau with Data = newData; RowNames = newRowNames }

let findEnteringColumn (tableau: Tableau) =
    let lastRow = tableau.Rows - 1
    let mutable minVal = 0.0
    let mutable colIdx = -1
    for j in 0 .. tableau.Cols - 2 do
        let v = tableau.Data.[lastRow, j]
        if v < minVal then
            minVal <- v
            colIdx <- j
    colIdx

let findLeavingRow (tableau: Tableau) (pivotCol: int) =
    let rhsCol = tableau.Cols - 1
    let mutable minRatio = Double.PositiveInfinity
    let mutable rowIdx = -1
    for i in 0 .. tableau.Rows - 2 do
        let entry = tableau.Data.[i, pivotCol]
        let rhs = tableau.Data.[i, rhsCol]
        if entry > 1e-9 then
            let ratio = rhs / entry
            if ratio < minRatio then
                minRatio <- ratio
                rowIdx <- i
    rowIdx

let rec solve (tableau: Tableau) =
    printfn "Current Tableau:"
    printTableau tableau

    let pivotCol = findEnteringColumn tableau
    if pivotCol = -1 then
        printfn "Optimal solution reached."
        tableau
    else
        printfn "Entering Variable: %s" tableau.VarNames.[pivotCol]

        let pivotRow = findLeavingRow tableau pivotCol
        if pivotRow = -1 then
            failwith "Unbounded Solution"

        printfn "Leaving Variable: %s" tableau.RowNames.[pivotRow]
        printfn "Pivot Element: %f" tableau.Data.[pivotRow, pivotCol]

        let nextTableau = pivot tableau pivotRow pivotCol
        solve nextTableau

[<EntryPoint>]
let main argv =
    let initialData = array2D [
        [ -1.0;  1.0; 1.0; 0.0; 0.0; 11.0 ]
        [  1.0;  1.0; 0.0; 1.0; 0.0; 27.0 ]
        [  2.0;  5.0; 0.0; 0.0; 1.0; 90.0 ]
        [ -4.0; -6.0; 0.0; 0.0; 0.0;  0.0 ]
    ]

    let varNames = [| "x1"; "x2"; "s1"; "s2"; "s3"; "RHS" |]
    let rowNames = [| "s1"; "s2"; "s3"; "z" |]

    let tableau = {
        Data = initialData
        Rows = 4
        Cols = 6
        VarNames = varNames
        RowNames = rowNames
    }

    // NOTE: This program only works for any Linear Program (LP) with the following criteria:
    // Maximization problem
    // All constraints are <=
    // All variables are non-negative
    // Does not have >= or = constraints
    // Does not include artificial variables

    let finalTableau = solve tableau

    printfn "Final Tableau:"
    printTableau finalTableau

    printfn "Optimal Value z = %f" finalTableau.Data.[finalTableau.Rows - 1, finalTableau.Cols - 1]

    0
