(*** hide ***)
#load "packages/FsLab/FsLab.fsx"
(**

Mortgages
=================


*)

(*** hide ***)
open XPlot.GoogleCharts

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


let mortgageTable scenarios =
    let getAxis field (title,data) = title,(data |> List.map field |> List.sum |> (fun t -> System.String.Format("{0:C}", t)))
    
    [
        scenarios |> List.map (getAxis (fun d-> d.net) );
        scenarios |> List.map (getAxis (fun d-> d.payment) );
        scenarios |> List.map (getAxis (fun d-> d.interest) );
    ]
    |> Chart.Table
    |> Chart.WithLabels ["Name";"Net";"Payments";"Interest"]
    
let mortgageCharts (title, data) =
    let opts = 
        Options( 
            vAxes = [|Axis(title="GBP per month");Axis(title="Mortgage left")|],
            series = [|
                for _ in 0 .. 2 -> Series(targetAxisIndex = 0)
                yield Series(targetAxisIndex = 1)
                |])
    Chart.Line [
        data |> List.map (fun p -> float p.index/12.,p.payment);
        data |> List.map (fun p -> float p.index/12.,p.interest);
        data |> List.map (fun p -> float p.index/12.,p.net);
        data |> List.map (fun p -> float p.index/12.,p.mortgageLeft);
        ]
    |> Chart.WithOptions opts
    |> Chart.WithTitle title
    |> Chart.WithLabels ["Repayments";"Interest";"Net";"Left"]

(*  
mortgageCharts ("Good Rates", calcStats 25 ratesGood payExpectedPerMonth mortgageValue) |> Chart.Show
mortgageCharts ("Middle Rates", calcStats 25 ratesMiddle payExpectedPerMonth mortgageValue) |> Chart.Show
mortgageCharts ("Bad Rates", calcStats 25 ratesBad payExpectedPerMonth mortgageValue) |> Chart.Show
*)

let scenarios = [
    "Good Rates - pay normal", calcStats 25 ratesGood payExpectedPerMonth mortgageValue;
    "Good Rates - pay £2000", calcStats 25 ratesGood pay2000PerMonth mortgageValue;
    "Middle Rates - pay normal", calcStats 25 ratesMiddle payExpectedPerMonth mortgageValue;
    "Middle Rates - pay £2000", calcStats 25 ratesMiddle pay2000PerMonth mortgageValue;
    "Bad Rates - pay normal", calcStats 25 ratesBad payExpectedPerMonth mortgageValue;
    "Bad Rates - pay £2000", calcStats 25 ratesBad pay2000PerMonth mortgageValue;
]
1.0.ToString()
(*** define-output:table1 ***)
mortgageTable  scenarios
(*** include-it:table1 ***)

(*** define-output:chart1 ***)
mortgageCharts scenarios.[0]
(*** include-it:chart1 ***)

(*** define-output:chart2 ***)
mortgageCharts scenarios.[1]
(*** include-it:chart2 ***)

(*** define-output:chart3 ***)
mortgageCharts scenarios.[2]
(*** include-it:chart3 ***)

(*** define-output:chart4 ***)
mortgageCharts scenarios.[3]
(*** include-it:chart4 ***)

(*** define-output:chart5 ***)
mortgageCharts scenarios.[4]
(*** include-it:chart5 ***)

(*** define-output:chart6 ***)
mortgageCharts scenarios.[5]
(*** include-it:chart6 ***)
