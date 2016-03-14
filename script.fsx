#load "packages/FsLab/FsLab.fsx"

open FSharp.Charting

let ( ** ) d n = 
    match n with
    | n when n = 0 -> 1M
    | _ ->
    let v = List.replicate (abs n) d |> List.reduce (*)
    if sign n = 1 then v else 1M/v
    
let monthlyRepayment rate numOfRepayments total = (rate * total) / (1M - ((1M + rate) ** -numOfRepayments))
let aprToMonth a = a / 12M / 100M
let yearsToMonths years = years * 12

let ratesGood i = if i < 24 then 2M else 4M
let ratesMiddle i = if i < 24 then 2.49M else 5M
let ratesBad i = if i < 24 then 3M else 6M

let payExpectedPerMonth i expectedPayment = expectedPayment
let pay2000PerMonth i expectedPayment = 2000M 

let mortgageValue = 280000.00M
let years = 25
let r = yearsToMonths years

let zeroInterestAmount = mortgageValue / decimal r

let initialMonthlyRepayment = monthlyRepayment (aprToMonth 2M) r mortgageValue
let laterMonthlyRepayment = monthlyRepayment (aprToMonth 6M) r mortgageValue

type repayment = {
    index : int
    mortgageLeft : decimal;
    payment : decimal;
    net : decimal;
    interest : decimal;
}


let calcStats years (calcRate:int -> decimal) calcRepayement mortgageValue =
    let months = yearsToMonths years
    let initial:repayment = {index=0;mortgageLeft=mortgageValue;payment=0M;net=0M;interest=0M} 
    [1 .. months]
    |> List.scan (fun s m -> 
        match s.mortgageLeft with
        | 0M -> {index=m;mortgageLeft=0M;payment=0M;net=0M;interest=0M;}
        | _ ->   
        let rate = calcRate m |> aprToMonth
        let expectedMonthlyRepayment = monthlyRepayment rate (months - m + 1) s.mortgageLeft
        let payment = calcRepayement m expectedMonthlyRepayment
        let interest = rate * s.mortgageLeft
        let net = payment - interest
        match s.mortgageLeft - net with
        | newLeft when newLeft > 0M ->
            {index=m;mortgageLeft=newLeft;payment=payment;net=net;interest=interest;}
        | _ ->
            {index=m;mortgageLeft=0M;payment=s.mortgageLeft+interest;net=s.mortgageLeft;interest=interest;}
        
    ) initial
    |> List.tail

let mortgageCharts (title, data) =
    let paymentTotal = data |> List.map (fun p -> p.payment) |> List.sum;
    let netTotal = data |> List.map (fun p -> p.net) |> List.sum;
    let interestTotal = data |> List.map (fun p -> p.interest) |> List.sum;

    let repayments = 
        Chart.Combine [
            Chart.Line (data |> List.map (fun p -> float p.index/12.,p.payment),Name="Repayments");
            Chart.Line (data |> List.map (fun p -> float p.index/12.,p.interest),Name="Interest");
            Chart.Line (data |> List.map (fun p -> float p.index/12.,p.net),Name="Net");
            ]
        |> Chart.WithTitle title
        |> Chart.WithXAxis(Title="Years")
        |> Chart.WithYAxis(Title="GBP per month")
        |> Chart.WithLegend(Title="Legend", Docking=ChartTypes.Docking.Bottom, InsideArea=false)
    
    let totals =
        Chart.Pie [sprintf "Mortgage GBP%.2f" netTotal,netTotal; sprintf "Interest GBP%.2f" interestTotal,interestTotal;]
    
    Chart.Columns [repayments;totals]
    

(*** define-output:chart ***)

(*  
mortgageCharts ("Good Rates", calcStats 25 ratesGood payExpectedPerMonth mortgageValue) |> Chart.Show
mortgageCharts ("Middle Rates", calcStats 25 ratesMiddle payExpectedPerMonth mortgageValue) |> Chart.Show
mortgageCharts ("Bad Rates", calcStats 25 ratesBad payExpectedPerMonth mortgageValue) |> Chart.Show
*)

mortgageCharts ("Middle Rates - pay normal", calcStats 25 ratesMiddle payExpectedPerMonth mortgageValue)
mortgageCharts ("Middle Rates - page £2000", calcStats 25 ratesMiddle pay2000PerMonth mortgageValue)

(*** include-it:chart ***)