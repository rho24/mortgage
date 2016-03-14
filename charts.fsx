(**
Charts
=================
*)

(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
open XPlot.GoogleCharts
open XPlot.Plotly

(*** define-output:chart1 ***)
Chart.Line [ for x in 0. .. 0.5 .. 6.3 -> x, sin x ]
|> Chart.WithSize (1000, 250)
(*** include-it:chart1 ***)



(*** define-output:chart2 ***)
let trace1 =
    Scatter(
        x = [1; 2; 3; 4],
        y = [10; 15; 13; 17],
        mode = "markers"
    )

let trace2 =
    Scatter(
        x = [2; 3; 4; 5],
        y = [16; 5; 11; 9],
        mode = "lines"
    )

let trace3 =
    Scatter(
        x = [1; 2; 3; 4],
        y = [12; 9; 15; 12],
        mode = "lines+markers"
    )

[trace1; trace2; trace3]
|> Plotly.Plot
(*** include-it:chart2 ***)