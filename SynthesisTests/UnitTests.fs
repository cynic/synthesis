module Synthesis.Tests
open NUnit.Framework
open FsUnit
open Preamble

(*
NOTE: These tests have been written so that they will be easy for you to read.
Real testing might use NUnit's TestCaseData, and would test only one aspect
per test.  Here, we are testing correct values, incorrect values, corner cases,
exceptions, and so on, all in the same function.  THIS IS THE WRONG WAY
TO DO TESTING, but it is convenient for this prac.
*)

[<Test>]
let ``abelar`` () =
    abelar 0 |> should equal false
    abelar 12 |> should equal false
    abelar 13 |> should equal false
    abelar 25 |> should equal false
    abelar 3097 |> should equal false
    abelar 3098 |> should equal false
    abelar 3096 |> should equal true
    abelar 24 |> should equal true

[<Test>]
let ``area`` () =
    area 15.0 23.0 |> should equal 172.5
    (fun () -> area -4.0 -10.0) |> shouldFail
    (fun () -> area -5.0 11.0) |> shouldFail
    (fun () -> area 3.0 -9.0) |> shouldFail
    (fun () -> area 0.0 -8.0) |> shouldFail
    (fun () -> area -12.0 0.0) |> shouldFail
    area 4.0 10.0 |> should equal 20.0
    area 0.0 190.0 |> should equal 0.0
    area 85.2 12.6 |> should equal 536.76
    area 9.0 0.0 |> should equal 0.0

[<Test>]
let ``zollo`` () =
    zollo 10 |> should equal 20
    zollo 0 |> should equal 0
    zollo -1 |> should equal 1
    zollo -34 |> should equal 34
    zollo 107 |> should equal 214

[<Test>]
let ``min`` () =
    min 0 0 |> should equal 0
    min 5 5 |> should equal 5
    min 5 -5 |> should equal -5
    min -5 -5 |> should equal -5
    min -7 -5 |> should equal -7
    min -5 -7 |> should equal -7
    min -8 5 |> should equal -8
    min 5 -8 |> should equal -8
    min 80 73 |> should equal 73
    min 70 79 |> should equal 70

[<Test>]
let ``max`` () =
    max 0 0 |> should equal 0
    max 5 5 |> should equal 5
    max 5 -5 |> should equal 5
    max -5 -5 |> should equal -5
    max -7 -5 |> should equal -5
    max -5 -7 |> should equal -5
    max -8 5 |> should equal 5
    max 5 -8 |> should equal 5
    max 80 73 |> should equal 80
    max 70 79 |> should equal 79

[<Test>]
let ``ofTime`` () =
    ofTime 0 0 0 |> should equal 0
    ofTime 5 0 0 |> should equal 18000
    ofTime 0 5 0 |> should equal 300
    ofTime 0 0 5 |> should equal 5
    ofTime 5 0 5 |> should equal 18005
    ofTime 5 5 0 |> should equal 18300
    ofTime 5 5 5 |> should equal 18305
    ofTime 0 5 5 |> should equal 305
    ofTime 3 2 1 |> should equal 10921

[<Test>]
let ``toTime`` () =
    toTime 0 |> should equal (0,0,0)
    toTime 18000 |> should equal (5,0,0)
    toTime 300 |> should equal (0,5,0)
    toTime 5 |> should equal (0,0,5)
    toTime 18005 |> should equal (5,0,5)
    toTime 18300 |> should equal (5,5,0)
    toTime 18305 |> should equal (5,5,5)
    toTime 305 |> should equal (0,5,5)
    toTime 10921 |> should equal (3,2,1)
    toTime -15 |> should equal (0,0,0)

[<Test>]
let ``digits`` () =
    digits 0 |> should equal 1
    digits 1 |> should equal 1
    digits 10 |> should equal 2
    digits 2 |> should equal 1
    digits 3 |> should equal 1
    digits 103 |> should equal 3
    digits 3000 |> should equal 4
    digits 1025 |> should equal 4
    digits 1024 |> should equal 4
    digits 9824356 |> should equal 7
    digits 100001 |> should equal 6
    digits -18274 |> should equal 5
    digits -8000002 |> should equal 7
    digits -1 |> should equal 1
    digits -2 |> should equal 1

[<Test>]
let ``minmax`` () =
    minmax (6,6,6,6) |> should equal (6,6)
    minmax (4,6,8,10) |> should equal (4,10)
    minmax (9,7,5,3) |> should equal (3,9)
    minmax (2,1,3,4) |> should equal (1,4)
    minmax (3,1,2,4) |> should equal (1,4)
    minmax (1,3,2,4) |> should equal (1,4)
    minmax (2,3,1,4) |> should equal (1,4)
    minmax (3,2,1,4) |> should equal (1,4)
    minmax (3,2,4,1) |> should equal (1,4)
    minmax (2,3,4,1) |> should equal (1,4)
    minmax (4,3,2,1) |> should equal (1,4)
    minmax (3,4,2,1) |> should equal (1,4)
    minmax (2,4,3,1) |> should equal (1,4)
    minmax (4,2,3,1) |> should equal (1,4)
    minmax (4,1,3,2) |> should equal (1,4)
    minmax (1,4,3,2) |> should equal (1,4)
    minmax (3,4,1,2) |> should equal (1,4)
    minmax (4,3,1,2) |> should equal (1,4)
    minmax (1,3,4,2) |> should equal (1,4)
    minmax (3,1,4,2) |> should equal (1,4)
    minmax (2,1,4,3) |> should equal (1,4)
    minmax (1,2,4,3) |> should equal (1,4)
    minmax (4,2,1,3) |> should equal (1,4)
    minmax (2,4,1,3) |> should equal (1,4)
    minmax (1,4,2,3) |> should equal (1,4)
    minmax (4,1,2,3) |> should equal (1,4)

[<Test>]
let ``isLeap`` () =
    (fun () -> isLeap 1581) |> shouldFail
    (fun () -> isLeap -3) |> shouldFail
    isLeap 1582 |> should equal false
    isLeap 1583 |> should equal false
    isLeap 1584 |> should equal true
    isLeap 1585 |> should equal false
    isLeap 1586 |> should equal false
    isLeap 1588 |> should equal true
    isLeap 1600 |> should equal true
    isLeap 1700 |> should equal false
    isLeap 1800 |> should equal false
    isLeap 1900 |> should equal false
    isLeap 2000 |> should equal true
    isLeap 2200 |> should equal false
    isLeap 2400 |> should equal true
    isLeap 2016 |> should equal true
    isLeap 2018 |> should equal false
    isLeap 2019 |> should equal false
    isLeap 2020 |> should equal true

[<Test>]
let ``month`` () =
    month 1 |> should equal ("January", 31)
    month 2 |> should equal ("February", 28)
    month 3 |> should equal ("March", 31)
    month 4 |> should equal ("April", 30)
    month 5 |> should equal ("May", 31)
    month 6 |> should equal ("June", 30)
    month 7 |> should equal ("July", 31)
    month 8 |> should equal ("August", 31)
    month 9 |> should equal ("September", 30)
    month 10 |> should equal ("October", 31)
    month 11 |> should equal ("November", 30)
    month 12 |> should equal ("December", 31)
    (fun () -> month 0) |> shouldFail
    (fun () -> month 13) |> shouldFail
    (fun () -> month -4) |> shouldFail

[<Test>]
let ``toBinary`` () =
    toBinary 0 |> should equal "0"
    toBinary 1 |> should equal "1"
    (fun () -> toBinary -1) |> shouldFail
    toBinary 2 |> should equal "10"
    toBinary 3 |> should equal "11"
    toBinary 4 |> should equal "100"
    toBinary 10 |> should equal "1010"
    toBinary 9688659 |> should equal "100100111101011001010011"
    toBinary 2147483647 |> should equal "1111111111111111111111111111111"

[<Test>]
let ``bizFuzz`` () =
    bizFuzz 1 |> should equal (0,0,0)
    bizFuzz 3 |> should equal (1,0,0)
    bizFuzz 5 |> should equal (1,1,0)
    bizFuzz 10 |> should equal (3,2,0)
    bizFuzz -8 |> should equal (0,0,0)
    bizFuzz 200 |> should equal (66,40,13)
    bizFuzz 99186 |> should equal (33062, 19837, 6612)

[<Test>]
let ``monthDay`` () =
    (fun () -> monthDay 0 1700) |> shouldFail
    (fun () -> monthDay 0 1600) |> shouldFail
    (fun () -> monthDay 366 1700) |> shouldFail
    (fun () -> monthDay 367 1600) |> shouldFail
    (fun () -> monthDay 1 1581) |> shouldFail
    monthDay 1 1582 |> should equal "January"
    monthDay 1 1582 |> should equal "January"
    monthDay 365 1700 |> should equal "December"
    monthDay 366 1600 |> should equal "December"
    monthDay 90 2019 |> should equal "March"
    monthDay 91 2019 |> should equal "April"
    monthDay 31 2019 |> should equal "January"
    monthDay 32 2019 |> should equal "February"
    monthDay 59 2019 |> should equal "February"
    monthDay 60 2019 |> should equal "March"
    monthDay 334 2019 |> should equal "November"
    monthDay 335 2019 |> should equal "December"
    monthDay 90 2020 |> should equal "March"
    monthDay 91 2020 |> should equal "March"
    monthDay 92 2020 |> should equal "April"
    monthDay 31 2020 |> should equal "January"
    monthDay 32 2020 |> should equal "February"
    monthDay 59 2020 |> should equal "February"
    monthDay 60 2020 |> should equal "February"
    monthDay 61 2020 |> should equal "March"
    monthDay 334 2020 |> should equal "November"
    monthDay 335 2020 |> should equal "November"
    monthDay 336 2020 |> should equal "December"

[<Test>]
let ``circle`` () =
    let dist = coord >> (fun (x,_) -> x)
    let within = coord >> (fun (_,x) -> x)
    dist (3.0, 2.5) (1.5, 9.4) |> should (equalWithin 0.001) 7.061
    dist (0.0, 0.0) (3.0, 4.0) |> should equal 5.0
    dist (0.0, 0.0) (-3.0, -4.0) |> should equal 5.0
    within (2.0, 1.8) (1.0, 2.5) 1.3 2.0 |> should equal true
    within (-3.1, -3.4) (-3.9, -2.5) 3.0 2.0 |> should equal true
    within (-4.8, -3.4) (-3.9, -2.5) 3.0 2.0 |> should equal false
