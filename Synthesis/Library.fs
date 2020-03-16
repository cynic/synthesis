module Synthesis


let abelar a =
    (a%12 = 0) && (a > 12) && (a < 3097)
    

let area b h =
    match ((b >= 0.00) && (h >= 0.00)) with
   | true -> 0.5 * b * h
   | false -> failwith "Values below 0"
    

let zollo x =
    match (x > 0) with
    | true -> (x * 2)
    | _ -> (x * -1)
    

let min no1 no2 =
    match (no1 > no2) with
    | true -> no2
    | false -> no1
    

let max no1 no2 =
    match (no1 > no2) with 
    | true -> no1
    | false -> no2

let ofTime hours minutes seconds =
     (hours * 60 * 60) + (minutes * 60) + seconds

let toTime seconds =
    match (seconds <= 0) with
    |true -> (0,0,0)
    |false -> (seconds/3600),((seconds-(seconds/3600)*3600)/60),((seconds-(seconds/3600)*3600)-(((seconds-(seconds/3600)*3600))/60)*60)
    

let digits n =
    let rec numdigits a b =
        match (a / 10) = 0 with
            |true -> b
            |_ -> numdigits (a / 10) (b + 1)
                    
    match (n < 10 && n > -10) with 
         |true -> 1
         |_ -> numdigits n 1
    

let minmax (a,b,c,d) =
    let smallest = min a b |> min c |> min d
    let largest = max a b |> max c |> max d
    (smallest,largest)
    

let isLeap year = 
        match year >= 1582 with
            |true -> match (year % 100 <> 0 && year % 4 = 0) with
                        |true -> true
                        | _ -> year % 400 = 0
            | _ -> failwith "Invalid year"
        
       
   

let month x =
    match x with
    | 1 -> "January", 31
    | 2 -> "February", 28
    | 3 -> "March", 31
    | 4 -> "April", 30
    | 5 -> "May", 31
    | 6 -> "June", 30
    | 7 -> "July", 31
    | 8 -> "August", 31
    | 9 -> "September", 30
    | 10 -> "October", 31
    | 11 -> "November", 30
    | 12 -> "December", 31
    | _ -> failwith "fail"

let toBinary n = 
    let rec binary number string=
        match number < 0 with
        |true-> failwith "Number below 0"
        | _ -> match (number = 0 && string = "") with
            |true-> string
            |false-> match (number % 2 <> 0) with
                     |true-> binary (number/2) ("1"+ string)
                     |_-> binary (number/2) ("0"+ string)
    binary n ""

let bizFuzz n =
    let rec func i (a,b,c) = 
        match i > n,  (i % 3) = 0, (i % 5) = 0 with 
            |true, _, _ -> (a,b,c)
            |false, true, true -> func (i + 1) (a + 1, b + 1, c + 1)
            |false, true, false -> func (i + 1) (a + 1, b, c)
            |false, false, true -> func (i + 1) (a, b + 1, c)
            |_, _, _ -> func (i + 1) (a,b,c)
    func 1 (0,0,0)
    

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"

