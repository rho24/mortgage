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
let ratesMiddle = List.append [2.49M;2.49M] (List.replicate 23 5M)
let ratesBad = List.append [3M;3M] (List.replicate 23 6M)

let mortgageValue = 280000.00M
let years = 25
let r = yearsToMonths years

let zeroInterestAmoug = mortgageValue / decimal r

let initialMonthlyRepayment = monthlyRepayment (aprToMonth 2M) r mortgageValue
let laterMonthlyRepayment = monthlyRepayment (aprToMonth 6M) r mortgageValue

type repayment = {
    index : int
    mortgageLeft : decimal;
    payment : decimal;
    net : decimal;
    interest : decimal;
}


let calcStats years (calcRate:int -> decimal) mortgageValue =
    let months = yearsToMonths years
    let initial:repayment = {index=0;mortgageLeft=mortgageValue;payment=0M;net=0M;interest=0M} 
    [1 .. months]
    |> List.scan (fun s m -> 
        let rate = calcRate m |> aprToMonth
        let payment = monthlyRepayment rate (months - m + 1) s.mortgageLeft
        let interest = rate * s.mortgageLeft
        let net = payment - interest
        let newLeft = s.mortgageLeft - net
        {index=m;mortgageLeft=newLeft;payment=payment;net=net;interest=interest; }   
    ) initial
    
let res = calcStats 25 ratesGood mortgageValue |> List.tail

let paymentTotal = res |> List.map (fun p -> p.payment) |> List.sum;
let netTotal = res |> List.map (fun p -> p.net) |> List.sum;
let interestTotal = res |> List.map (fun p -> p.interest) |> List.sum;

Chart.Combine [
    // res |> List.map (fun p -> p.index,p.mortgageLeft) |> Chart.Line;
    res |> List.map (fun p -> p.index,p.payment) |> Chart.Line;
    res |> List.map (fun p -> p.index,p.interest) |> Chart.Line;
    res |> List.map (fun p -> p.index,p.net) |> Chart.Line;
    ]
// List.init (int numberOfPayments) (fun y -> y)
// |> List.scan (fun amountLeft month -> amountLeft + (initialMonthlyRate * amountLeft) - initialMonthlyRepayment) mortgageValue
// |> Chart.Line
