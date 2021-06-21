/// <summary>
/// Tests for BigDecimal operations
/// </summary>
/// <remarks>
/// Many of the tests here are generated from the test suite available at:
/// http://speleotrove.com/decimal/dectest.zip
/// 
/// All the files in that test suite contain the following:
/// <code>
/// -- Copyright (c) IBM Corporation, 1981, 2008.  All rights reserved.   --
/// ------------------------------------------------------------------------
/// -- Please see the document "General Decimal Arithmetic Testcases"     --
/// -- at http://www2.hursley.ibm.com/decimal for the description of      --
/// -- these testcases.                                                   --
/// --                                                                    --
/// -- These testcases are experimental ('beta' versions), and they       --
/// -- may contain errors.  They are offered on an as-is basis.  In       --
/// -- particular, achieving the same results as the tests here is not    --
/// -- a guarantee that an implementation complies with any Standard      --
/// -- or specification.  The tests are not exhaustive.                   --
/// --                                                                    --
/// -- Please send comments, suggestions, and corrections to the author:  --
/// --   Mike Cowlishaw, IBM Fellow                                       --
/// --   IBM UK, PO Box 31, Birmingham Road, Warwick CV34 5JL, UK         --
/// --   mfc@uk.ibm.com                                                   --
/// ------------------------------------------------------------------------
/// </code>
/// </remarks>
module Tests

open Expecto
open Clojure.Numerics
open System.Numerics


[<Tests>]
let basicParsingList =
    testList "basic string parsing" [


        testCase "parsing empty string returns false" <| fun _ ->
            let f input = let (ok,_) = BigDecimal.TryParse(input) in ok
            Expect.isFalse (f "") "Expect false return parsing empty string"

        testCase "parsing sign-only returns false" <| fun _ ->
            let f input = let (ok,_) = BigDecimal.TryParse(input) in ok
            [| "+"; "-" |] 
            |> Seq.iter (fun v -> Expect.isFalse (f v) "Expect false return parsing sign only.")

        testCase "parsing input with missing exponent digits returns false" <| fun _ ->
            let f input = let (ok,_) = BigDecimal.TryParse(input) in ok
            [| "0E"; "0e"; "0E+"; "0E-";  "0e+";  "0e-" |]
            |> Seq.iter (fun v -> Expect.isFalse (f v) "Expect false return parsing with missing exponent digits.")           
    ]

let zeroParsingTests = 
    [ ( "0",0);  ( "0000",0); ( "00.00", -2); 
      ("+0",0);  ("+0000",0); ("+00.00", -2);
      ("-0",0);  ("-0000",0); ("-00.00", -2) ]
    |> List.map (fun (input, expectedExponent) ->         
        testCase (sprintf "parsing '%s' yields 0 with exponent %i and precision 1" input expectedExponent) <| fun _ ->
            let ok, bd = BigDecimal.TryParse(input)
            let si = input.ToString()
            Expect.isTrue ok (sprintf "TryParse on %s should return true" si)
            Expect.isTrue bd.Coefficient.IsZero (sprintf "coefficient of %s should be 0" si)
            Expect.equal bd.Exponent expectedExponent (sprintf "Bad exponent for %s" si)
            Expect.equal bd.Precision 1u (sprintf "Precision of %s should be 1" si)
        )

[<Tests>]
let zeroParsingList = testList "zero string parsing"  zeroParsingTests

let simpleIntTest decString intString exponent precision =
    let ok, bd = BigDecimal.TryParse(decString)
    Expect.isTrue ok (sprintf "TryParse on %s should return true" decString)
    Expect.equal bd.Coefficient (System.Numerics.BigInteger.Parse(intString)) (sprintf "Bad coefficient for %s" decString)
    Expect.equal bd.Exponent exponent (sprintf "Bad exponent for %s" decString)
    Expect.equal bd.Precision precision (sprintf "Bad precision for %s" decString)

let createIntTests data =
    data
    |> List.map (fun (decString, intString, exponent, precision) ->
           testCase (sprintf "parsing '%s' yields %s with exponent %i and precision %u" decString intString exponent precision) <| fun _ -> 
               simpleIntTest decString intString exponent precision
           )

let basicIntParsingTests =
    [   ("1", "1", 0, 1u);     
        ("01", "1", 0, 1u);
        ("12", "12", 0, 2u);
        ("123", "123", 0, 3u);
        ("123.", "123", 0, 3u);
        ("123.0", "1230", -1, 4u);
        ("123.00", "12300", -2, 5u);
        ("123456789123456789.", "123456789123456789", 0, 18u);
        ("12345678912345678.9", "123456789123456789", -1, 18u);
        ("1234567891234567.89", "123456789123456789", -2, 18u);
        ("123456789.123456789", "123456789123456789", -9, 18u);
        ("1.23456789123456789", "123456789123456789", -17, 18u);
        (".123456789123456789", "123456789123456789", -18, 18u)  ]

let basicNegativeIntParsingTests =
    [   ("-1", "-1", 0, 1u);     
        ("-01", "-1", 0, 1u);
        ("-12", "-12", 0, 2u);
        ("-123", "-123", 0, 3u);
        ("-123.", "-123", 0, 3u);
        ("-123.0", "-1230", -1, 4u);
        ("-123.00", "-12300", -2, 5u);
        ("-123456789123456789.", "-123456789123456789", 0, 18u);
        ("-12345678912345678.9", "-123456789123456789", -1, 18u);
        ("-1234567891234567.89", "-123456789123456789", -2, 18u);
        ("-123456789.123456789", "-123456789123456789", -9, 18u);
        ("-1.23456789123456789", "-123456789123456789", -17, 18u);
        ("-.123456789123456789", "-123456789123456789", -18, 18u)  ]

let basicPositiveIntParsingTests =
    [   ("+1", "1", 0, 1u);     
        ("+01", "1", 0, 1u);
        ("+12", "12", 0, 2u);
        ("+123", "123", 0, 3u);
        ("+123.", "123", 0, 3u);
        ("+123.0", "1230", -1, 4u);
        ("+123.00", "12300", -2, 5u);
        ("+123456789123456789.", "123456789123456789", 0, 18u);
        ("+12345678912345678.9", "123456789123456789", -1, 18u);
        ("+1234567891234567.89", "123456789123456789", -2, 18u);
        ("+123456789.123456789", "123456789123456789", -9, 18u);
        ("+1.23456789123456789", "123456789123456789", -17, 18u);
        ("+.123456789123456789", "123456789123456789", -18, 18u)  ]

let basicExponentParsingTests = 
    [   ("1E0", "1", 0, 1u);
        ("1E1", "1", 1, 1u);
        ("1E2", "1", 2, 1u);
        ("1E20", "1", 20, 1u);

        ("1.0E0", "10", -1, 2u);
        ("1.00E0", "100", -2, 3u);
        ("1.000E0", "1000", -3, 4u);

        ("1.0E1", "10", 0, 2u);
        ("1.00E1", "100", -1, 3u);
        ("1.000E1", "1000", -2, 4u);

        ("1.0E2", "10", 1, 2u);
        ("1.00E2", "100", 0, 3u);
        ("1.000E2", "1000", -1, 4u); 

        ("1.0E20", "10", 19, 2u);
        ("1.00E20", "100", 18, 3u);
        ("1.000E20", "1000", 17, 4u) ]


let basicPostitiveExponentParsingTests = 
    [   ("1E+0", "1", 0, 1u);
        ("1E+1", "1", 1, 1u);
        ("1E+2", "1", 2, 1u);
        ("1E+20", "1", 20, 1u);

        ("1.0E+0", "10", -1, 2u);
        ("1.00E+0", "100", -2, 3u);
        ("1.000E+0", "1000", -3, 4u);

        ("1.0E+1", "10", 0, 2u);
        ("1.00E+1", "100", -1, 3u);
        ("1.000E+1", "1000", -2, 4u);

        ("1.0E+2", "10", 1, 2u);
        ("1.00E+2", "100", 0, 3u);
        ("1.000E+2", "1000", -1, 4u); 

        ("1.0E+20", "10", 19, 2u);
        ("1.00E+20", "100", 18, 3u);
        ("1.000E+20", "1000", 17, 4u) ]

let basicNegativeExponentParsingTests = 
    [   ("1E-0", "1", 0, 1u);
        ("1E-1", "1", -1, 1u);
        ("1E-2", "1", -2, 1u);
        ("1E-20", "1", -20, 1u);

        ("1.0E-0", "10", -1, 2u);
        ("1.00E-0", "100", -2, 3u);
        ("1.000E-0", "1000", -3, 4u);

        ("1.0E-1", "10", -2, 2u);
        ("1.00E-1", "100", -3, 3u);
        ("1.000E-1", "1000", -4, 4u);

        ("1.0E-2", "10", -3, 2u);
        ("1.00E-2", "100", -4, 3u);
        ("1.000E-2", "1000", -5, 4u);

        ("1.0E-20", "10", -21, 2u);
        ("1.00E-20", "100", -22, 3u);
        ("1.000E-20", "1000", -23, 4u); ]


let javaDocParsingTests =
    [   ("0", "0", 0, 1u);
        ("0.00", "0", -2, 1u);
        ("123", "123", 0, 3u);
        ("-123", "-123", 0, 3u);
        ("1.23E3", "123", 1, 3u);
        ("1.23E+3", "123", 1, 3u);
        ("12.3E+7", "123", 6, 3u);
        ("12.0", "120", -1, 3u);
        ("12.3", "123", -1, 3u);
        ("0.00123", "123", -5, 3u);
        ("-1.23E-12", "-123", -14, 3u);
        ("1234.5E-4", "12345", -5, 5u);
        ("0E+7", "0", 7, 1u);
        ("-0", "0", 0, 1u);   ]

let specParsingTests = 
    [   ("0", "0",0,1u);
        ("0.00", "0",-2,1u);
        ("123", "123",0,3u);
        ("-123", "-123",0,3u);
        ("1.23E3", "123",1,3u);
        ("1.23E+3", "123",1,3u);
        ("12.3E+7", "123",6,3u);
        ("12.0", "120",-1,3u);
        ("12.3", "123",-1,3u);
        ("0.00123", "123",-5,3u);
        ("-1.23E-12", "-123",-14,3u);
        ("1234.5E-4", "12345",-5,5u);
        ("-0", "0",0,1u);
        ("-0.00", "0",-2,1u);
        ("0E+7", "0",7,1u);
        ("-0E-7", "0",-7,1u);   ]


[<Tests>]
let basicIntParsingList = testList "basic int parsing"  (createIntTests basicIntParsingTests)

[<Tests>]
let basicNegativeIntParsingList = testList "basic negative int parsing"  (createIntTests basicNegativeIntParsingTests)

[<Tests>]
let basicPositiveIntParsingList = testList "basic positive int parsing"  (createIntTests basicPositiveIntParsingTests)

[<Tests>]
let basicExponentParsingList = testList "basic exponent parsing"  (createIntTests basicExponentParsingTests)

[<Tests>]
let basicPostiveExponentParsingList = testList "basic positive exponent parsing"  (createIntTests basicPostitiveExponentParsingTests)

[<Tests>]
let basicNegativeExponentParsingList = testList "basic negative exponent parsing"  (createIntTests basicNegativeExponentParsingTests)

[<Tests>]
let javaDocParsingList = testList "Java doc parsing examples"  (createIntTests javaDocParsingTests)

[<Tests>]
let specParsingList = testList "Spec parsing examples"  (createIntTests specParsingTests)


let scientificStringTest biStr exp outStr =
    let bi = BigInteger.Parse(biStr)
    let bd = BigDecimal.Create(bi,exp)
    let result = bd.ToScientificString()
    Expect.equal result outStr (sprintf "Result from %s x 10^%i" biStr exp)

let createToStringTests data =
    data
    |> List.map (fun (biStr, exponent, outStr) ->
           testCase (sprintf "printing '%s' with exponent %i yields '%s;" biStr exponent outStr ) <| fun _ -> 
               scientificStringTest biStr exponent outStr
           )

let basicToStringTests = 
    [   ("123",0,"123");
        ("-123",0, "-123");
        ("123",1, "1.23E+3");
        ("123",3, "1.23E+5");
        ("123",-1, "12.3");
        ("123",-5, "0.00123");
        ("123",-10, "1.23E-8");
        ("-123",-12, "-1.23E-10");
        ("0",0, "0");
        ("0",-2, "0.00");
        ("0",2, "0E+2");
        ("5",-6, "0.000005");
        ("50",-7, "0.0000050");
        ("5",-7, "5E-7");    ]

let specToStringTests = 
    [   ("123",0,"123");
        ("-123",0, "-123");
        ("123",1, "1.23E+3");
        ("123",3, "1.23E+5");
        ("123",-1, "12.3");
        ("123",-5, "0.00123");
        ("123",-10, "1.23E-8");
        ("-123",-12, "-1.23E-10");
        ("0",0, "0");
        ("0",-2, "0.00");
        ("0",2, "0E+2");
        ("5",-6, "0.000005");
        ("50",-7, "0.0000050");
        ("5",-7, "5E-7");   ]

let toStringPointPlacementTests = 
    [   ("123456789", -2, "1234567.89");
        ("-123456789", -2, "-1234567.89");  ]



[<Tests>]
let basicToStringList = testList "Basic ToString examples"  (createToStringTests basicToStringTests)

[<Tests>]
let specToStringList = testList "Spec ToString examples"  (createToStringTests specToStringTests)

[<Tests>]
let toStringPointPlacementList = testList "point placement ToString examples"  (createToStringTests toStringPointPlacementTests)

let precisionTest str exponent precision =
    let bd = BigDecimal.Create(BigInteger.Parse(str),exponent)
    Expect.equal bd.Precision precision (sprintf "Precision for %s exponent %i" str exponent)

let createPrecisionTests data =
    data
    |> List.map (fun (str, exponent, precision) ->
           testCase (sprintf "BD from '%s' with exponent %i has precision '%u;" str exponent precision ) <| fun _ -> 
               precisionTest str exponent precision
           )

let precisionTests =
    [   ("0", 0, 1u);
        ("2", 0, 1u);
        ("-2", 0, 1u);
        ("100", 0, 3u);
        ("999999999", 0, 9u);
        ("1000000000", 0, 10u);
        ("123456789123456789", -12, 18u);
        ("123456789123456789", 40, 18u);      ]

[<Tests>]
let precisionTestList = testList "precision examples"  (createPrecisionTests precisionTests)

[<Tests>]
let contantsTests = testList "tests of contants" [
    testCase "BigDecimal.Zero is correct" <| fun _ ->
        Expect.equal BigDecimal.Zero.Coefficient BigInteger.Zero "Zero should have coefficient 0"
        Expect.equal BigDecimal.Zero.Exponent 0 "Zero should have exponent 0"
        Expect.equal BigDecimal.Zero.Precision 1u "Zero should have precision 1"
        
    testCase "BigDecimal.One is correct" <| fun _ ->
        Expect.equal BigDecimal.One.Coefficient BigInteger.One "One should have coefficient 0"
        Expect.equal BigDecimal.One.Exponent 0 "One should have exponent 0"
        Expect.equal BigDecimal.One.Precision 1u "One should have precision 1"

    testCase "BigDecimal.Ten is correct" <| fun _ ->
        Expect.equal BigDecimal.Ten.Coefficient (BigInteger(10)) "Ten should have coefficient 0"
        Expect.equal BigDecimal.Ten.Exponent 0 "Ten should have exponent 0"
        Expect.equal BigDecimal.Ten.Precision 2u "Ten should have precision 2"
    ]

let doubleTest (v:double) (expectStr:string) (c:Context) =
    let d = BigDecimal.CreateC(v,c)
    let gotStr = d.ToScientificString()
    Expect.equal gotStr expectStr  "Wrong representation"

    if v <> 0.0 
    then 
        let d = BigDecimal.CreateC(-v,c)
        let gotStr = d.ToScientificString()
        Expect.equal gotStr ("-"+expectStr) "Wrong representation"
    else ()


let createDoubleTests data =
    data
    |> List.mapi (fun i (v, expect, context) ->
            testCase (sprintf "BD from double #%i '%s' with context %s" i expect (context.ToString())) <| fun _ -> 
                doubleTest v expect context
            )

let c9hu = Context.Create(9u, RoundingMode.HalfUp)

let doubleTests =
    [   (0.0, "0", c9hu);
        (1.0, "1", c9hu);
        (10.0, "10.0000000", c9hu);
        (100.0, "100.000000", c9hu);
        (1000.0, "1000.00000", c9hu);          
        (10000.0, "10000.0000", c9hu);
        (100000.0, "100000.000", c9hu);
        (1000000.0, "1000000.00", c9hu);
        (10000000.0, "10000000.0", c9hu);
        (100000000.0, "100000000", c9hu);
        (1000000000.0, "1.00000000E+9", c9hu);
        (10000000000.0, "1.00000000E+10", c9hu);
        (100000000000.0, "1.00000000E+11", c9hu);
        (1000000000000.0, "1.00000000E+12", c9hu);
        (10000000000000.0, "1.00000000E+13", c9hu);
        (100000000000000.0, "1.00000000E+14", c9hu);
        (1.0E10, "1.00000000E+10", c9hu);
        (1.0E20, "1.00000000E+20", c9hu);
        (1.0E30, "1.00000000E+30", c9hu);
        (1.0E40, "1.00000000E+40", c9hu);
        (1.0E50, "1.00000000E+50", c9hu);
        (1.0E60, "1.00000000E+60", c9hu);
        (1.0E70, "1.00000000E+70", c9hu);
        (0.1, "0.100000000", c9hu);
        (0.01, "0.0100000000", c9hu);
        (0.001, "0.00100000000", c9hu);
        (0.0001, "0.000100000000", c9hu);
        (0.00001, "0.0000100000000", c9hu);
        (0.000001, "0.00000100000000", c9hu);
        (0.0000001, "1.00000000E-7", c9hu);
        (0.00000001, "1.00000000E-8", c9hu);
        (0.000000001, "1.00000000E-9", c9hu);
        (0.0000000001, "1.00000000E-10", c9hu);
        (0.00000000001, "1.00000000E-11", c9hu);
        (0.000000000001, "1.00000000E-12", c9hu);
        (0.0000000000001, "1.00000000E-13", c9hu);
        (0.00000000000001, "1.00000000E-14", c9hu);
        (1.0E-10, "1.00000000E-10", c9hu);
        (1.0E-20, "1.00000000E-20", c9hu);
        (1.0E-30, "1.00000000E-30", c9hu);
        (1.0E-40, "1.00000000E-40", c9hu);
        (1.0E-50, "1.00000000E-50", c9hu);
        (1.0E-60, "1.00000000E-60", c9hu);
        (1.0E-70, "1.00000000E-70", c9hu);        

             // POwers of two have their own representations.
        (2.0, "2", c9hu);
        (4.0, "4", c9hu);
        (8.0, "8", c9hu);
        (16.0, "16", c9hu);
        (32.0, "32", c9hu);
        (64.0, "64", c9hu);
        (128.0, "128", c9hu);
        (256.0, "256", c9hu);
        (512.0, "512", c9hu);
        (1024.0, "1024", c9hu);
        (1024.0 * 1024.0, "1048576", c9hu);
        (1024.0 * 1024.0 * 1024.0, "1.07374182E+9", c9hu);

        (1.0 / 2.0, "0.5", c9hu);
        (1.0 / 4.0, "0.25", c9hu);
        (1.0 / 8.0, "0.125", c9hu);
        (1.0 / 16.0, "0.0625", c9hu);
        (1.0 / 32.0, "0.03125", c9hu);
        (1.0 / 64.0, "0.015625", c9hu);
        (1.0 / 128.0, "0.0078125", c9hu);
        (1.0 / 256.0, "0.00390625", c9hu);
        (1.0 / 512.0, "0.001953125", c9hu);
        (1.0 / 1024.0, "0.0009765625", c9hu);
        (1.0 / 2048.0, "0.00048828125", c9hu);
        (1.0 / 4096.0, "0.000244140625", c9hu);
        (1.0 / 8192.0, "0.000122070313", c9hu);
        (1.0 / 16384.0, "0.0000610351563", c9hu);
        (1.0 / (1024.0 * 1024.0), "9.53674316E-7", c9hu);

        // random goodness
        (1.23456789, "1.23456789", c9hu);
        (1.234567896, "1.23456790", c9hu);

        (1.234567891, "1.23456789", c9hu);
        (12.34567891, "12.3456789", c9hu);
        (123.4567891, "123.456789", c9hu);
        (1234.567891, "1234.56789", c9hu);
        (12345.67891, "12345.6789", c9hu);
        (123456.7891, "123456.789", c9hu);
        (1234567.891, "1234567.89", c9hu);
        (12345678.91, "12345678.9", c9hu);
        (123456789.1, "123456789", c9hu);
        (1234567891.0, "1.23456789E+9", c9hu);
        (12345678910.0, "1.23456789E+10", c9hu);
        (123456789100.00, "1.23456789E+11", c9hu);

        (1.234567896, "1.23456790", c9hu);
        (12.34567896, "12.3456790", c9hu);
        (123.4567896, "123.456790", c9hu);
        (1234.567896, "1234.56790", c9hu);
        (12345.67896, "12345.6790", c9hu);
        (123456.7896, "123456.790", c9hu);
        (1234567.896, "1234567.90", c9hu);
        (12345678.96, "12345679.0", c9hu);
        (123456789.6, "123456790", c9hu);
        (1234567896.0, "1.23456790E+9", c9hu);
        (12345678960.0, "1.23456790E+10", c9hu);
        (123456789600.00, "1.23456790E+11", c9hu);

        (0.1234567891, "0.123456789", c9hu);
        (0.01234567891, "0.0123456789", c9hu);
        (0.001234567891, "0.00123456789", c9hu);
        (0.0001234567891, "0.000123456789", c9hu);
        (0.00001234567891, "0.0000123456789", c9hu);
        (0.000001234567891, "0.00000123456789", c9hu);
        (0.0000001234567891, "1.23456789E-7", c9hu);
        (0.00000001234567891, "1.23456789E-8", c9hu);
        (0.000000001234567891, "1.23456789E-9", c9hu);
        (0.0000000001234567891, "1.23456789E-10", c9hu);
        (0.00000000001234567891, "1.23456789E-11", c9hu);
        (0.000000000001234567891, "1.23456789E-12", c9hu);
        (0.0000000000001234567891, "1.23456789E-13", c9hu);

        (0.1234567896, "0.123456790", c9hu);
        (0.01234567896, "0.0123456790", c9hu);
        (0.001234567896, "0.00123456790", c9hu);
        (0.0001234567896, "0.000123456790", c9hu);
        (0.00001234567896, "0.0000123456790", c9hu);
        (0.000001234567896, "0.00000123456790", c9hu);
        (0.0000001234567896, "1.23456790E-7", c9hu);
        (0.00000001234567896, "1.23456790E-8", c9hu);
        (0.000000001234567896, "1.23456790E-9", c9hu);
        (0.0000000001234567896, "1.23456790E-10", c9hu);
        (0.00000000001234567896, "1.23456790E-11", c9hu);
        (0.000000000001234567896, "1.23456790E-12", c9hu);
        (0.0000000000001234567896, "1.23456790E-13", c9hu);

        
        ]

    
[<Tests>]
let doubleList = testList "creation from double examples"  (createDoubleTests doubleTests)


let basicRoundingTest bdStr precision mode biStr exponent =
    let mc = Context.Create(precision,mode)
    let bd = BigDecimal.Parse(bdStr)
    let br = BigDecimal.Round(bd,mc)
    Expect.equal br.Coefficient (BigInteger.Parse(biStr)) "coefficient"
    Expect.equal br.Exponent exponent "exponent"

let createRoundingTests data =
    data
    |> List.map (fun (bdStr, precision, mode, biStr, exponent) ->
            testCase (sprintf "Rounding %s to %u %s yield %s %i" bdStr precision (mode.ToString()) biStr exponent) <| fun _ ->
                basicRoundingTest bdStr precision mode biStr exponent
            )

let basicRoundingTests = 
    [   ("123.456", 0u, RoundingMode.HalfUp, "0", 3);
        ("123.456", 1u, RoundingMode.HalfUp, "1", 2);
        ("123.456", 2u, RoundingMode.HalfUp, "12", 1);
        ("123.456", 3u, RoundingMode.HalfUp, "123", 0);
        ("123.456", 4u, RoundingMode.HalfUp, "1235", -1);
        ("123.456", 5u, RoundingMode.HalfUp, "12346", -2);
        ("123.456", 6u, RoundingMode.HalfUp, "123456", -3);
        ("123.456", 7u, RoundingMode.HalfUp, "123456", -3);    ]

let javaDocRoundingTests = 
    [
        ("5.5", 1u, RoundingMode.Up, "6", 0);
        ("5.5", 1u, RoundingMode.Down, "5", 0);
        ("5.5", 1u, RoundingMode.Ceiling, "6", 0);
        ("5.5", 1u, RoundingMode.Floor, "5", 0);
        ("5.5", 1u, RoundingMode.HalfUp, "6", 0);
        ("5.5", 1u, RoundingMode.HalfDown, "5", 0);
        ("5.5", 1u, RoundingMode.HalfEven, "6", 0);

        ("2.5", 1u, RoundingMode.Up, "3", 0);
        ("2.5", 1u, RoundingMode.Down, "2", 0);
        ("2.5", 1u, RoundingMode.Ceiling, "3", 0);
        ("2.5", 1u, RoundingMode.Floor, "2", 0);
        ("2.5", 1u, RoundingMode.HalfUp, "3", 0);
        ("2.5", 1u, RoundingMode.HalfDown, "2", 0);
        ("2.5", 1u, RoundingMode.HalfEven, "2", 0);

        ("1.6", 1u, RoundingMode.Up, "2", 0);
        ("1.6", 1u, RoundingMode.Down, "1", 0);
        ("1.6", 1u, RoundingMode.Ceiling, "2", 0);
        ("1.6", 1u, RoundingMode.Floor, "1", 0);
        ("1.6", 1u, RoundingMode.HalfUp, "2", 0);
        ("1.6", 1u, RoundingMode.HalfDown, "2", 0);
        ("1.6", 1u, RoundingMode.HalfEven, "2", 0);

        ("1.1", 1u, RoundingMode.Up, "2", 0);
        ("1.1", 1u, RoundingMode.Down, "1", 0);
        ("1.1", 1u, RoundingMode.Ceiling, "2", 0);
        ("1.1", 1u, RoundingMode.Floor, "1", 0);
        ("1.1", 1u, RoundingMode.HalfUp, "1", 0);
        ("1.1", 1u, RoundingMode.HalfDown, "1", 0);
        ("1.1", 1u, RoundingMode.HalfEven, "1", 0);

        ("1.0", 1u, RoundingMode.Up, "1", 0);
        ("1.0", 1u, RoundingMode.Down, "1", 0);
        ("1.0", 1u, RoundingMode.Ceiling, "1", 0);
        ("1.0", 1u, RoundingMode.Floor, "1", 0);
        ("1.0", 1u, RoundingMode.HalfUp, "1", 0);
        ("1.0", 1u, RoundingMode.HalfDown, "1", 0);
        ("1.0", 1u, RoundingMode.HalfEven, "1", 0);
        ("1.0", 1u, RoundingMode.Unnecessary, "1", 0);

        ("-1.0", 1u, RoundingMode.Up, "-1", 0);
        ("-1.0", 1u, RoundingMode.Down, "-1", 0);
        ("-1.0", 1u, RoundingMode.Ceiling, "-1", 0);
        ("-1.0", 1u, RoundingMode.Floor, "-1", 0);
        ("-1.0", 1u, RoundingMode.HalfUp, "-1", 0);
        ("-1.0", 1u, RoundingMode.HalfDown, "-1", 0);
        ("-1.0", 1u, RoundingMode.HalfEven, "-1", 0);
        ("-1.0", 1u, RoundingMode.Unnecessary, "-1", 0);

        ("-1.1", 1u, RoundingMode.Up, "-2", 0);
        ("-1.1", 1u, RoundingMode.Down, "-1", 0);
        ("-1.1", 1u, RoundingMode.Ceiling, "-1", 0);
        ("-1.1", 1u, RoundingMode.Floor, "-2", 0);
        ("-1.1", 1u, RoundingMode.HalfUp, "-1", 0);
        ("-1.1", 1u, RoundingMode.HalfDown, "-1", 0);
        ("-1.1", 1u, RoundingMode.HalfEven, "-1", 0);

        ("-1.6", 1u, RoundingMode.Up, "-2", 0);
        ("-1.6", 1u, RoundingMode.Down, "-1", 0);
        ("-1.6", 1u, RoundingMode.Ceiling, "-1", 0);
        ("-1.6", 1u, RoundingMode.Floor, "-2", 0);
        ("-1.6", 1u, RoundingMode.HalfUp, "-2", 0);
        ("-1.6", 1u, RoundingMode.HalfDown, "-2", 0);
        ("-1.6", 1u, RoundingMode.HalfEven, "-2", 0);

        ("-2.5", 1u, RoundingMode.Up, "-3", 0);
        ("-2.5", 1u, RoundingMode.Down, "-2", 0);
        ("-2.5", 1u, RoundingMode.Ceiling, "-2", 0);
        ("-2.5", 1u, RoundingMode.Floor, "-3", 0);
        ("-2.5", 1u, RoundingMode.HalfUp, "-3", 0);
        ("-2.5", 1u, RoundingMode.HalfDown, "-2", 0);
        ("-2.5", 1u, RoundingMode.HalfEven, "-2", 0);

        ("-5.5", 1u, RoundingMode.Up, "-6", 0);
        ("-5.5", 1u, RoundingMode.Down, "-5", 0);
        ("-5.5", 1u, RoundingMode.Ceiling, "-5", 0);
        ("-5.5", 1u, RoundingMode.Floor, "-6", 0);
        ("-5.5", 1u, RoundingMode.HalfUp, "-6", 0);
        ("-5.5", 1u, RoundingMode.HalfDown, "-5", 0);
        ("-5.5", 1u, RoundingMode.HalfEven, "-6", 0);

        ("0.19", 3u, RoundingMode.Floor, "19", -2);
        (".190909", 3u, RoundingMode.Floor, "190", -3);
        ("999.9", 3u, RoundingMode.Up, "100", 1);
    ]


[<Tests>]
let basicRoundingList = testList "basic rounding" (createRoundingTests basicRoundingTests)

[<Tests>]
let javaDocRoundingList = testList "javaDoc rounding" (createRoundingTests javaDocRoundingTests)


// support functions for parsing the standardized tests


let stripSingleQuotes (str:string) =
    let start, len = 
        match str.[0] = '\'', str.[str.Length-1] = '\''with
        | true, true -> 1, str.Length-2
        | true, false -> 1, str.Length-1
        | false, true -> 0, str.Length-1
        | false, false -> 0, str.Length

    str.Substring(start,len)


let getTwoArgs (test:string) =
    let atoms : string array = test.Split(System.Array.Empty<char>(), System.StringSplitOptions.RemoveEmptyEntries)
    stripSingleQuotes(atoms.[2]),  stripSingleQuotes(atoms.[4])

let getThreeArgs (test:string) =
    let atoms : string array = test.Split(System.Array.Empty<char>(), System.StringSplitOptions.RemoveEmptyEntries)
    stripSingleQuotes(atoms.[2]),  stripSingleQuotes(atoms.[3]), stripSingleQuotes(atoms.[5])


// quantization tests

let testQuantize lhsStr rhsStr mode expectStr =
    let lhs = BigDecimal.Parse(lhsStr)
    let rhs = BigDecimal.Parse(rhsStr)
    let result = BigDecimal.Quantize(lhs,rhs,mode)
    let resultStr = result.ToScientificString();
    Expect.equal resultStr expectStr "Quantization incorrect"


let createTestQuantizeTests data =
    data
    |> List.map (fun (lhsStr, rhsStr, mode, expectStr) ->
           testCase (sprintf "quantize'%s' with using '%s' with mode %s " lhsStr rhsStr (mode.ToString()) ) <| fun _ -> 
                testQuantize lhsStr rhsStr mode expectStr
           )


let TQ test mode =
    let lhs, rhs, result = getThreeArgs test
    testQuantize lhs rhs mode result



let createTQTests data =
    data
    |> List.map (fun (str, mode) ->
           testCase (sprintf "quantize'%s' with mode %s " str (mode.ToString()) ) <| fun _ -> 
               TQ str mode
           )


// The following tests are taken from the spec test cases.

let mhu = RoundingMode.HalfUp

let quantizeSpecSanityChecks = 
    [   
        ("0", "1e0", mhu, "0");
        ("1", "1e0", mhu, "1");
        ("0.1", "1e+2", mhu, "0E+2");
        ("0.1", "1e+1", mhu, "0E+1");
        ("0.1", "1e0", mhu, "0");
        ("0.1", "1e-1", mhu, "0.1");
        ("0.1", "1e-2", mhu, "0.10");
        ("0.1", "1e-3", mhu, "0.100");
        ("0.9", "1e+2", mhu, "0E+2");
        ("0.9", "1e+1", mhu, "0E+1");
        ("0.9", "1e+0", mhu, "1");
        ("0.9", "1e-1", mhu, "0.9");
        ("0.9", "1e-3", mhu, "0.900");
    ]

let quantizeExamplesFromSpec = 
    [
        ("2.17", "0.001", mhu, "2.170");
        ("2.17", "0.01", mhu, "2.17");
        ("2.17", "0.1", mhu, "2.2");
        ("2.17", "1e+0", mhu, "2");
        ("2.17", "1e+1", mhu, "0E+1");
        ("-0.1", "1", mhu, "0");
        ("0", "1e+5", mhu, "0E+5");
        ("217", "1e-1", mhu, "217.0");
        ("217", "1e+0", mhu, "217");
        ("217", "1e+1", mhu, "2.2E+2");
        ("217", "1e+2", mhu, "2E+2");
        ("+35236450.6", "1e-2", mhu, "35236450.60");
        ("-35236450.6", "1e-2", mhu, "-35236450.60");        
    ]

[<Tests>]
let quantizeSpecSanityCheckList = testList "quantize spec sanity check" (createTestQuantizeTests quantizeSpecSanityChecks)

[<Tests>]
let quantizeExamplesFromCheckList = testList "quantize examples from spec" (createTestQuantizeTests quantizeExamplesFromSpec)

let quantizeSpecNegatives = 
    [
        ("quax021 quantize -0      1e0   -> 0", mhu);
        ("quax022 quantize -1      1e0   -> -1", mhu);
        ("quax023 quantize -0.1   1e+2   -> 0E+2 Inexact Rounded", mhu);
        ("quax025 quantize -0.1   1e+1   -> 0E+1 Inexact Rounded", mhu);
        ("quax026 quantize -0.1    1e0   -> 0 Inexact Rounded", mhu);
        ("quax027 quantize -0.1   1e-1   -> -0.1", mhu);
        ("quax028 quantize -0.1   1e-2   -> -0.10", mhu);
        ("quax029 quantize -0.1   1e-3   -> -0.100", mhu);
        ("quax030 quantize -0.9   1e+2   -> 0E+2 Inexact Rounded", mhu);
        ("quax031 quantize -0.9   1e+1   -> 0E+1 Inexact Rounded", mhu);
        ("quax032 quantize -0.9   1e+0   -> -1 Inexact Rounded", mhu);
        ("quax033 quantize -0.9   1e-1   -> -0.9", mhu);
        ("quax034 quantize -0.9   1e-2   -> -0.90", mhu);
        ("quax035 quantize -0.9   1e-3   -> -0.900", mhu);
        ("quax036 quantize -0.5   1e+2   -> 0E+2 Inexact Rounded", mhu);
        ("quax037 quantize -0.5   1e+1   -> 0E+1 Inexact Rounded", mhu);
        ("quax038 quantize -0.5   1e+0   -> -1 Inexact Rounded", mhu);
        ("quax039 quantize -0.5   1e-1   -> -0.5", mhu);
        ("quax040 quantize -0.5   1e-2   -> -0.50", mhu);
        ("quax041 quantize -0.5   1e-3   -> -0.500", mhu);
        ("quax042 quantize -0.9   1e+2   -> 0E+2 Inexact Rounded", mhu);
        ("quax043 quantize -0.9   1e+1   -> 0E+1 Inexact Rounded", mhu);
        ("quax044 quantize -0.9   1e+0   -> -1 Inexact Rounded", mhu);
        ("quax045 quantize -0.9   1e-1   -> -0.9", mhu);
        ("quax046 quantize -0.9   1e-2   -> -0.90", mhu);
        ("quax047 quantize -0.9   1e-3   -> -0.900", mhu);    
    ]

let quantizeSpecGeneral = 
    [
        ("quax089 quantize 12     1e+4   -> 0E+4 Inexact Rounded", mhu);
        ("quax090 quantize 12     1e+3   -> 0E+3 Inexact Rounded", mhu);
        ("quax091 quantize 12     1e+2   -> 0E+2 Inexact Rounded", mhu);
        ("quax092 quantize 12     1e+1   -> 1E+1 Inexact Rounded", mhu);
        ("quax093 quantize 1.2345 1e-2   -> 1.23 Inexact Rounded", mhu);
        ("quax094 quantize 1.2355 1e-2   -> 1.24 Inexact Rounded", mhu);
        ("quax095 quantize 1.2345 1e-6   -> 1.234500", mhu);
        ("quax096 quantize 9.9999 1e-2   -> 10.00 Inexact Rounded", mhu);
        ("quax097 quantize 0.0001 1e-2   -> 0.00 Inexact Rounded", mhu);
        ("quax098 quantize 0.001  1e-2   -> 0.00 Inexact Rounded", mhu);
        ("quax099 quantize 0.009  1e-2   -> 0.01 Inexact Rounded", mhu);
        ("quax100 quantize 92     1e+2   -> 1E+2 Inexact Rounded", mhu);
        ("quax101 quantize -1      1e0   ->  -1", mhu);
        ("quax102 quantize -1     1e-1   ->  -1.0", mhu);
        ("quax103 quantize -1     1e-2   ->  -1.00", mhu);
        ("quax104 quantize  0      1e0   ->  0", mhu);
        ("quax105 quantize  0     1e-1   ->  0.0", mhu);
        ("quax106 quantize  0     1e-2   ->  0.00", mhu);
        ("quax107 quantize  0.00   1e0   ->  0", mhu);
        ("quax108 quantize  0     1e+1   ->  0E+1", mhu);
        ("quax109 quantize  0     1e+2   ->  0E+2", mhu);
        ("quax110 quantize +1      1e0   ->  1", mhu);
        ("quax111 quantize +1     1e-1   ->  1.0", mhu);
        ("quax112 quantize +1     1e-2   ->  1.00", mhu);
        ("quax120 quantize   1.04  1e-3 ->  1.040", mhu);
        ("quax121 quantize   1.04  1e-2 ->  1.04", mhu);
        ("quax122 quantize   1.04  1e-1 ->  1.0 Inexact Rounded", mhu);
        ("quax123 quantize   1.04   1e0 ->  1 Inexact Rounded", mhu);
        ("quax124 quantize   1.05  1e-3 ->  1.050", mhu);
        ("quax125 quantize   1.05  1e-2 ->  1.05", mhu);
        ("quax126 quantize   1.05  1e-1 ->  1.1 Inexact Rounded", mhu);
        ("quax131 quantize   1.05   1e0 ->  1 Inexact Rounded", mhu);
        ("quax132 quantize   1.06  1e-3 ->  1.060", mhu);
        ("quax133 quantize   1.06  1e-2 ->  1.06", mhu);
        ("quax134 quantize   1.06  1e-1 ->  1.1 Inexact Rounded", mhu);
        ("quax135 quantize   1.06   1e0 ->  1 Inexact Rounded", mhu);
        ("quax140 quantize   -10    1e-2  ->  -10.00", mhu);
        ("quax141 quantize   +1     1e-2  ->  1.00", mhu);
        ("quax142 quantize   +10    1e-2  ->  10.00", mhu);
        ("quax143 quantize   1E+10  1e-2  ->  10000000000.00 Invalid_operation", mhu); // modded from NaN
        ("quax144 quantize   1E-10  1e-2  ->  0.00 Inexact Rounded", mhu);
        ("quax145 quantize   1E-3   1e-2  ->  0.00 Inexact Rounded", mhu);
        ("quax146 quantize   1E-2   1e-2  ->  0.01", mhu);
        ("quax147 quantize   1E-1   1e-2  ->  0.10", mhu);
        ("quax148 quantize   0E-10  1e-2  ->  0.00", mhu);
        ("quax150 quantize   1.0600 1e-5 ->  1.06000", mhu);
        ("quax151 quantize   1.0600 1e-4 ->  1.0600", mhu);
        ("quax152 quantize   1.0600 1e-3 ->  1.060 Rounded", mhu);
        ("quax153 quantize   1.0600 1e-2 ->  1.06 Rounded", mhu);
        ("quax154 quantize   1.0600 1e-1 ->  1.1 Inexact Rounded", mhu);
        ("quax155 quantize   1.0600  1e0 ->  1 Inexact Rounded", mhu);
    
    ]

let quantizeSpecBaseTestsWithNonOneCoeffs =
    [
        ("quax161 quantize 0      -9e0   -> 0", mhu);
        ("quax162 quantize 1      -7e0   -> 1", mhu);
        ("quax163 quantize 0.1   -1e+2   -> 0E+2 Inexact Rounded", mhu);
        ("quax165 quantize 0.1    0e+1   -> 0E+1 Inexact Rounded", mhu);
        ("quax166 quantize 0.1     2e0   -> 0 Inexact Rounded", mhu);
        ("quax167 quantize 0.1    3e-1   -> 0.1", mhu);
        ("quax168 quantize 0.1   44e-2   -> 0.10", mhu);
        ("quax169 quantize 0.1  555e-3   -> 0.100", mhu);
        ("quax170 quantize 0.9 6666e+2   -> 0E+2 Inexact Rounded", mhu);
        ("quax171 quantize 0.9 -777e+1   -> 0E+1 Inexact Rounded", mhu);
        ("quax172 quantize 0.9  -88e+0   -> 1 Inexact Rounded", mhu);
        ("quax173 quantize 0.9   -9e-1   -> 0.9", mhu);
        ("quax174 quantize 0.9    0e-2   -> 0.90", mhu);
        ("quax175 quantize 0.9  1.1e-3   -> 0.9000", mhu);
        //-- negatives
        ("quax181 quantize -0    1.1e0   -> 0.0", mhu);  // neg zero
        ("quax182 quantize -1     -1e0   -> -1", mhu);
        ("quax183 quantize -0.1  11e+2   -> 0E+2 Inexact Rounded", mhu); // neg zero
        ("quax185 quantize -0.1 111e+1   -> 0E+1 Inexact Rounded", mhu); // neg zero
        ("quax186 quantize -0.1   71e0   -> 0 Inexact Rounded", mhu);// neg zero
        ("quax187 quantize -0.1 -91e-1   -> -0.1", mhu);
        ("quax188 quantize -0.1 -.1e-2   -> -0.100", mhu);
        ("quax189 quantize -0.1  -1e-3   -> -0.100", mhu);
        ("quax190 quantize -0.9   0e+2   -> 0E+2 Inexact Rounded", mhu);// neg zero
        ("quax191 quantize -0.9  -0e+1   -> 0E+1 Inexact Rounded", mhu);
        ("quax192 quantize -0.9 -10e+0   -> -1 Inexact Rounded", mhu);
        ("quax193 quantize -0.9 100e-1   -> -0.9", mhu);
        ("quax194 quantize -0.9 999e-2   -> -0.90", mhu);        
    ]

let quantizeSpecTestPosExponents =
    [
        ("quax201 quantize   -1   1e+0 ->  -1", mhu);
        ("quax202 quantize   -1   1e+1 ->  0E+1 Inexact Rounded", mhu); // mod: neg zero
        ("quax203 quantize   -1   1e+2 ->  0E+2 Inexact Rounded", mhu); // mod: neg zero
        ("quax204 quantize    0   1e+0 ->  0", mhu);
        ("quax205 quantize    0   1e+1 ->  0E+1", mhu);
        ("quax206 quantize    0   1e+2 ->  0E+2", mhu);
        ("quax207 quantize   +1   1e+0 ->  1", mhu);
        ("quax208 quantize   +1   1e+1 ->  0E+1 Inexact Rounded", mhu);
        ("quax209 quantize   +1   1e+2 ->  0E+2 Inexact Rounded", mhu);

        ("quax220 quantize   1.04 1e+3 ->  0E+3 Inexact Rounded", mhu);
        ("quax221 quantize   1.04 1e+2 ->  0E+2 Inexact Rounded", mhu);
        ("quax222 quantize   1.04 1e+1 ->  0E+1 Inexact Rounded", mhu);
        ("quax223 quantize   1.04 1e+0 ->  1 Inexact Rounded", mhu);
        ("quax224 quantize   1.05 1e+3 ->  0E+3 Inexact Rounded", mhu);
        ("quax225 quantize   1.05 1e+2 ->  0E+2 Inexact Rounded", mhu);
        ("quax226 quantize   1.05 1e+1 ->  0E+1 Inexact Rounded", mhu);
        ("quax227 quantize   1.05 1e+0 ->  1 Inexact Rounded", mhu);
        ("quax228 quantize   1.05 1e+3 ->  0E+3 Inexact Rounded", mhu);
        ("quax229 quantize   1.05 1e+2 ->  0E+2 Inexact Rounded", mhu);
        ("quax230 quantize   1.05 1e+1 ->  0E+1 Inexact Rounded", mhu);
        ("quax231 quantize   1.05 1e+0 ->  1 Inexact Rounded", mhu);
        ("quax232 quantize   1.06 1e+3 ->  0E+3 Inexact Rounded", mhu);
        ("quax233 quantize   1.06 1e+2 ->  0E+2 Inexact Rounded", mhu);
        ("quax234 quantize   1.06 1e+1 ->  0E+1 Inexact Rounded", mhu);
        ("quax235 quantize   1.06 1e+0 ->  1 Inexact Rounded", mhu);

        ("quax240 quantize   -10   1e+1  ->  -1E+1 Rounded", mhu);
        ("quax241 quantize   +1    1e+1  ->  0E+1 Inexact Rounded", mhu);
        ("quax242 quantize   +10   1e+1  ->  1E+1 Rounded", mhu);
        ("quax243 quantize   1E+1  1e+1  ->  1E+1          -- underneath this is E+1", mhu);
        ("quax244 quantize   1E+2  1e+1  ->  1.0E+2        -- underneath this is E+1", mhu);
        ("quax245 quantize   1E+3  1e+1  ->  1.00E+3       -- underneath this is E+1", mhu);
        ("quax246 quantize   1E+4  1e+1  ->  1.000E+4      -- underneath this is E+1", mhu);
        ("quax247 quantize   1E+5  1e+1  ->  1.0000E+5     -- underneath this is E+1", mhu);
        ("quax248 quantize   1E+6  1e+1  ->  1.00000E+6    -- underneath this is E+1", mhu);
        ("quax249 quantize   1E+7  1e+1  ->  1.000000E+7   -- underneath this is E+1", mhu);
        ("quax250 quantize   1E+8  1e+1  ->  1.0000000E+8  -- underneath this is E+1", mhu);
        ("quax251 quantize   1E+9  1e+1  ->  1.00000000E+9 -- underneath this is E+1", mhu);
            //             // -- next one tries to add 9 zeros --  This fails in the original due to precision=9 limit
        ("quax252 quantize   1E+10 1e+1  ->  1.000000000E+10 Invalid_operation", mhu); // mod from NaN
        ("quax253 quantize   1E-10 1e+1  ->  0E+1 Inexact Rounded", mhu);
        ("quax254 quantize   1E-2  1e+1  ->  0E+1 Inexact Rounded", mhu);
        ("quax255 quantize   0E-10 1e+1  ->  0E+1", mhu);
        ("quax256 quantize  -0E-10 1e+1  -> 0E+1", mhu); // mod: neg zero
        ("quax257 quantize  -0E-1  1e+1  -> 0E+1", mhu); // mod: neg zero
        ("quax258 quantize  -0     1e+1  -> 0E+1", mhu); // mod: neg zero
        ("quax259 quantize  -0E+1  1e+1  -> 0E+1", mhu); // mod: neg zero

        ("quax260 quantize   -10   1e+2  ->  0E+2 Inexact Rounded", mhu); // mod: neg zero
        ("quax261 quantize   +1    1e+2  ->  0E+2 Inexact Rounded", mhu);
        ("quax262 quantize   +10   1e+2  ->  0E+2 Inexact Rounded", mhu);
        ("quax263 quantize   1E+1  1e+2  ->  0E+2 Inexact Rounded", mhu);
        ("quax264 quantize   1E+2  1e+2  ->  1E+2", mhu);
        ("quax265 quantize   1E+3  1e+2  ->  1.0E+3", mhu);
        ("quax266 quantize   1E+4  1e+2  ->  1.00E+4", mhu);
        ("quax267 quantize   1E+5  1e+2  ->  1.000E+5", mhu);
        ("quax268 quantize   1E+6  1e+2  ->  1.0000E+6", mhu);
        ("quax269 quantize   1E+7  1e+2  ->  1.00000E+7", mhu);
        ("quax270 quantize   1E+8  1e+2  ->  1.000000E+8", mhu);
        ("quax271 quantize   1E+9  1e+2  ->  1.0000000E+9", mhu);
        ("quax272 quantize   1E+10 1e+2  ->  1.00000000E+10", mhu);
        ("quax273 quantize   1E-10 1e+2  ->  0E+2 Inexact Rounded", mhu);
        ("quax274 quantize   1E-2  1e+2  ->  0E+2 Inexact Rounded", mhu);
        ("quax275 quantize   0E-10 1e+2  ->  0E+2", mhu);

        ("quax280 quantize   -10   1e+3  ->  0E+3 Inexact Rounded", mhu); // mod: neg zero
        ("quax281 quantize   +1    1e+3  ->  0E+3 Inexact Rounded", mhu);
        ("quax282 quantize   +10   1e+3  ->  0E+3 Inexact Rounded", mhu);
        ("quax283 quantize   1E+1  1e+3  ->  0E+3 Inexact Rounded", mhu);
        ("quax284 quantize   1E+2  1e+3  ->  0E+3 Inexact Rounded", mhu);
        ("quax285 quantize   1E+3  1e+3  ->  1E+3", mhu);
        ("quax286 quantize   1E+4  1e+3  ->  1.0E+4", mhu);
        ("quax287 quantize   1E+5  1e+3  ->  1.00E+5", mhu);
        ("quax288 quantize   1E+6  1e+3  ->  1.000E+6", mhu);
        ("quax289 quantize   1E+7  1e+3  ->  1.0000E+7", mhu);
        ("quax290 quantize   1E+8  1e+3  ->  1.00000E+8", mhu);
        ("quax291 quantize   1E+9  1e+3  ->  1.000000E+9", mhu);
        ("quax292 quantize   1E+10 1e+3  ->  1.0000000E+10", mhu);
        ("quax293 quantize   1E-10 1e+3  ->  0E+3 Inexact Rounded", mhu);
        ("quax294 quantize   1E-2  1e+3  ->  0E+3 Inexact Rounded", mhu);
        ("quax295 quantize   0E-10 1e+3  ->  0E+3", mhu);    
    ]

let quantizeSpecRoundUpFromBelow = 
    [
        ("quax300 quantize   0.0078 1e-5 ->  0.00780", mhu);
        ("quax301 quantize   0.0078 1e-4 ->  0.0078", mhu);
        ("quax302 quantize   0.0078 1e-3 ->  0.008 Inexact Rounded", mhu);
        ("quax303 quantize   0.0078 1e-2 ->  0.01 Inexact Rounded", mhu);
        ("quax304 quantize   0.0078 1e-1 ->  0.0 Inexact Rounded", mhu);
        ("quax305 quantize   0.0078  1e0 ->  0 Inexact Rounded", mhu);
        ("quax306 quantize   0.0078 1e+1 ->  0E+1 Inexact Rounded", mhu);
        ("quax307 quantize   0.0078 1e+2 ->  0E+2 Inexact Rounded", mhu);
    
        ("quax310 quantize  -0.0078 1e-5 -> -0.00780", mhu);
        ("quax311 quantize  -0.0078 1e-4 -> -0.0078", mhu);
        ("quax312 quantize  -0.0078 1e-3 -> -0.008 Inexact Rounded", mhu);
        ("quax313 quantize  -0.0078 1e-2 -> -0.01 Inexact Rounded", mhu);
        ("quax314 quantize  -0.0078 1e-1 -> 0.0 Inexact Rounded", mhu);  // mod: neg zero
        ("quax315 quantize  -0.0078  1e0 -> 0 Inexact Rounded", mhu);  // mod: neg zero
        ("quax316 quantize  -0.0078 1e+1 -> 0E+1 Inexact Rounded", mhu);  // mod: neg zero
        ("quax317 quantize  -0.0078 1e+2 -> 0E+2 Inexact Rounded", mhu);  // mod: neg zero
    
        ("quax320 quantize   0.078 1e-5 ->  0.07800", mhu);
        ("quax321 quantize   0.078 1e-4 ->  0.0780", mhu);
        ("quax322 quantize   0.078 1e-3 ->  0.078", mhu);
        ("quax323 quantize   0.078 1e-2 ->  0.08 Inexact Rounded", mhu);
        ("quax324 quantize   0.078 1e-1 ->  0.1 Inexact Rounded", mhu);
        ("quax325 quantize   0.078  1e0 ->  0 Inexact Rounded", mhu);
        ("quax326 quantize   0.078 1e+1 ->  0E+1 Inexact Rounded", mhu);
        ("quax327 quantize   0.078 1e+2 ->  0E+2 Inexact Rounded", mhu);
    
        ("quax330 quantize  -0.078 1e-5 -> -0.07800", mhu);
        ("quax331 quantize  -0.078 1e-4 -> -0.0780", mhu);
        ("quax332 quantize  -0.078 1e-3 -> -0.078", mhu);
        ("quax333 quantize  -0.078 1e-2 -> -0.08 Inexact Rounded", mhu);
        ("quax334 quantize  -0.078 1e-1 -> -0.1 Inexact Rounded", mhu);
        ("quax335 quantize  -0.078  1e0 -> 0 Inexact Rounded", mhu);  // mod: neg zero
        ("quax336 quantize  -0.078 1e+1 -> 0E+1 Inexact Rounded", mhu);  // mod: neg zero
        ("quax337 quantize  -0.078 1e+2 -> 0E+2 Inexact Rounded", mhu);  // mod: neg zero
    
        ("quax340 quantize   0.78 1e-5 ->  0.78000", mhu);
        ("quax341 quantize   0.78 1e-4 ->  0.7800", mhu);
        ("quax342 quantize   0.78 1e-3 ->  0.780", mhu);
        ("quax343 quantize   0.78 1e-2 ->  0.78", mhu);
        ("quax344 quantize   0.78 1e-1 ->  0.8 Inexact Rounded", mhu);
        ("quax345 quantize   0.78  1e0 ->  1 Inexact Rounded", mhu);
        ("quax346 quantize   0.78 1e+1 ->  0E+1 Inexact Rounded", mhu);
        ("quax347 quantize   0.78 1e+2 ->  0E+2 Inexact Rounded", mhu);
    
        ("quax350 quantize  -0.78 1e-5 -> -0.78000", mhu);
        ("quax351 quantize  -0.78 1e-4 -> -0.7800", mhu);
        ("quax352 quantize  -0.78 1e-3 -> -0.780", mhu);
        ("quax353 quantize  -0.78 1e-2 -> -0.78", mhu);
        ("quax354 quantize  -0.78 1e-1 -> -0.8 Inexact Rounded", mhu);
        ("quax355 quantize  -0.78  1e0 -> -1 Inexact Rounded", mhu);
        ("quax356 quantize  -0.78 1e+1 -> 0E+1 Inexact Rounded", mhu);  // mod: neg zero
        ("quax357 quantize  -0.78 1e+2 -> 0E+2 Inexact Rounded", mhu);  // mod: neg zero
    
        ("quax360 quantize   7.8 1e-5 ->  7.80000", mhu);
        ("quax361 quantize   7.8 1e-4 ->  7.8000", mhu);
        ("quax362 quantize   7.8 1e-3 ->  7.800", mhu);
        ("quax363 quantize   7.8 1e-2 ->  7.80", mhu);
        ("quax364 quantize   7.8 1e-1 ->  7.8", mhu);
        ("quax365 quantize   7.8  1e0 ->  8 Inexact Rounded", mhu);
        ("quax366 quantize   7.8 1e+1 ->  1E+1 Inexact Rounded", mhu);
        ("quax367 quantize   7.8 1e+2 ->  0E+2 Inexact Rounded", mhu);
        ("quax368 quantize   7.8 1e+3 ->  0E+3 Inexact Rounded", mhu);
    
        ("quax370 quantize  -7.8 1e-5 -> -7.80000", mhu);
        ("quax371 quantize  -7.8 1e-4 -> -7.8000", mhu);
        ("quax372 quantize  -7.8 1e-3 -> -7.800", mhu);
        ("quax373 quantize  -7.8 1e-2 -> -7.80", mhu);
        ("quax374 quantize  -7.8 1e-1 -> -7.8", mhu);
        ("quax375 quantize  -7.8  1e0 -> -8 Inexact Rounded", mhu);
        ("quax376 quantize  -7.8 1e+1 -> -1E+1 Inexact Rounded", mhu);
        ("quax377 quantize  -7.8 1e+2 -> 0E+2 Inexact Rounded", mhu);  // mod: neg zero
        ("quax378 quantize  -7.8 1e+3 -> 0E+3 Inexact Rounded", mhu);  // mod: neg zero    
    ]

let quantizeSpecSomeIndividuals = 
    [
    
        ("quax380 quantize   352364.506 1e-2 -> 352364.51 Inexact Rounded", mhu);
        ("quax381 quantize   3523645.06 1e-2 -> 3523645.06", mhu);
        ("quax382 quantize   35236450.6 1e-2 -> 35236450.60 Invalid_operation", mhu);   // Mod: NaN
        ("quax383 quantize   352364506  1e-2 -> 352364506.00 Invalid_operation", mhu);   // Mod: NaN
        ("quax384 quantize  -352364.506 1e-2 -> -352364.51 Inexact Rounded", mhu);
        ("quax385 quantize  -3523645.06 1e-2 -> -3523645.06", mhu);
        ("quax386 quantize  -35236450.6 1e-2 -> -35236450.60 Invalid_operation", mhu); // Mod: NaN
        ("quax387 quantize  -352364506  1e-2 -> -352364506.00 Invalid_operation", mhu); // Mod: NaN
    ]

let quantizeSpecExamplesFromEmail = 
    [   
        ("quax391 quantize  12.34567  1e-3 -> 12.346   Inexact Rounded", mhu);
        ("quax392 quantize  123.4567  1e-3 -> 123.457  Inexact Rounded", mhu);
        ("quax393 quantize  1234.567  1e-3 -> 1234.567", mhu);
        ("quax394 quantize  12345.67  1e-3 -> 12345.670 Invalid_operation", mhu); // Mod: NaN
        ("quax395 quantize  123456.7  1e-3 -> 123456.700 Invalid_operation", mhu); // Mod: NaN
        ("quax396 quantize  1234567.  1e-3 -> 1234567.000 Invalid_operation", mhu); // Mod: NaN     
    ]

let quantizeSpecSome9999Examples = 
    [
     // Some operations here were invalid in the original due to overal precision limits that we do not capture.
        ("quax400 quantize   9.999        1e-5  ->  9.99900", mhu);
        ("quax401 quantize   9.999        1e-4  ->  9.9990", mhu);
        ("quax402 quantize   9.999        1e-3  ->  9.999", mhu);
        ("quax403 quantize   9.999        1e-2  -> 10.00     Inexact Rounded", mhu);
        ("quax404 quantize   9.999        1e-1  -> 10.0      Inexact Rounded", mhu);
        ("quax405 quantize   9.999         1e0  -> 10        Inexact Rounded", mhu);
        ("quax406 quantize   9.999         1e1  -> 1E+1      Inexact Rounded", mhu);
        ("quax407 quantize   9.999         1e2  -> 0E+2      Inexact Rounded", mhu);
    
        ("quax410 quantize   0.999        1e-5  ->  0.99900", mhu);
        ("quax411 quantize   0.999        1e-4  ->  0.9990", mhu);
        ("quax412 quantize   0.999        1e-3  ->  0.999", mhu);
        ("quax413 quantize   0.999        1e-2  ->  1.00     Inexact Rounded", mhu);
        ("quax414 quantize   0.999        1e-1  ->  1.0      Inexact Rounded", mhu);
        ("quax415 quantize   0.999         1e0  ->  1        Inexact Rounded", mhu);
        ("quax416 quantize   0.999         1e1  -> 0E+1      Inexact Rounded", mhu);
    
        ("quax420 quantize   0.0999       1e-5  ->  0.09990", mhu);
        ("quax421 quantize   0.0999       1e-4  ->  0.0999", mhu);
        ("quax422 quantize   0.0999       1e-3  ->  0.100    Inexact Rounded", mhu);
        ("quax423 quantize   0.0999       1e-2  ->  0.10     Inexact Rounded", mhu);
        ("quax424 quantize   0.0999       1e-1  ->  0.1      Inexact Rounded", mhu);
        ("quax425 quantize   0.0999        1e0  ->  0        Inexact Rounded", mhu);
        ("quax426 quantize   0.0999        1e1  -> 0E+1      Inexact Rounded", mhu);
    
        ("quax430 quantize   0.00999      1e-5  ->  0.00999", mhu);
        ("quax431 quantize   0.00999      1e-4  ->  0.0100   Inexact Rounded", mhu);
        ("quax432 quantize   0.00999      1e-3  ->  0.010    Inexact Rounded", mhu);
        ("quax433 quantize   0.00999      1e-2  ->  0.01     Inexact Rounded", mhu);
        ("quax434 quantize   0.00999      1e-1  ->  0.0      Inexact Rounded", mhu);
        ("quax435 quantize   0.00999       1e0  ->  0        Inexact Rounded", mhu);
        ("quax436 quantize   0.00999       1e1  -> 0E+1      Inexact Rounded", mhu);
    
        ("quax440 quantize   0.000999     1e-5  ->  0.00100  Inexact Rounded", mhu);
        ("quax441 quantize   0.000999     1e-4  ->  0.0010   Inexact Rounded", mhu);
        ("quax442 quantize   0.000999     1e-3  ->  0.001    Inexact Rounded", mhu);
        ("quax443 quantize   0.000999     1e-2  ->  0.00     Inexact Rounded", mhu);
        ("quax444 quantize   0.000999     1e-1  ->  0.0      Inexact Rounded", mhu);
        ("quax445 quantize   0.000999      1e0  ->  0        Inexact Rounded", mhu);
        ("quax446 quantize   0.000999      1e1  -> 0E+1      Inexact Rounded", mhu);
    
        //precision: 8
        ("quax449 quantize   9.999E-15    1e-23 ->  9.99900000E-15 Invalid_operation", mhu); // mod: NaN
        ("quax450 quantize   9.999E-15    1e-22 ->  9.9990000E-15", mhu);
        ("quax451 quantize   9.999E-15    1e-21 ->  9.999000E-15", mhu);
        ("quax452 quantize   9.999E-15    1e-20 ->  9.99900E-15", mhu);
        ("quax453 quantize   9.999E-15    1e-19 ->  9.9990E-15", mhu);
        ("quax454 quantize   9.999E-15    1e-18 ->  9.999E-15", mhu);
        ("quax455 quantize   9.999E-15    1e-17 ->  1.000E-14 Inexact Rounded", mhu);
        ("quax456 quantize   9.999E-15    1e-16 ->  1.00E-14  Inexact Rounded", mhu);
        ("quax457 quantize   9.999E-15    1e-15 ->  1.0E-14   Inexact Rounded", mhu);
        ("quax458 quantize   9.999E-15    1e-14 ->  1E-14     Inexact Rounded", mhu);
        ("quax459 quantize   9.999E-15    1e-13 ->  0E-13     Inexact Rounded", mhu);
        ("quax460 quantize   9.999E-15    1e-12 ->  0E-12     Inexact Rounded", mhu);
        ("quax461 quantize   9.999E-15    1e-11 ->  0E-11     Inexact Rounded", mhu);
        ("quax462 quantize   9.999E-15    1e-10 ->  0E-10     Inexact Rounded", mhu);
        ("quax463 quantize   9.999E-15     1e-9 ->  0E-9      Inexact Rounded", mhu);
        ("quax464 quantize   9.999E-15     1e-8 ->  0E-8      Inexact Rounded", mhu);
        ("quax465 quantize   9.999E-15     1e-7 ->  0E-7      Inexact Rounded", mhu);
        ("quax466 quantize   9.999E-15     1e-6 ->  0.000000  Inexact Rounded", mhu);
        ("quax467 quantize   9.999E-15     1e-5 ->  0.00000   Inexact Rounded", mhu);
        ("quax468 quantize   9.999E-15     1e-4 ->  0.0000    Inexact Rounded", mhu);
        ("quax469 quantize   9.999E-15     1e-3 ->  0.000     Inexact Rounded", mhu);
        ("quax470 quantize   9.999E-15     1e-2 ->  0.00      Inexact Rounded", mhu);
        ("quax471 quantize   9.999E-15     1e-1 ->  0.0       Inexact Rounded", mhu);
        ("quax472 quantize   9.999E-15      1e0 ->  0         Inexact Rounded", mhu);
        ("quax473 quantize   9.999E-15      1e1 ->  0E+1      Inexact Rounded", mhu);
    
        //precision: 7
        ("quax900 quantize   9.999E-15    1e-22 ->  9.9990000E-15       Invalid_operation", mhu);  // mod: NaN
        ("quax901 quantize   9.999E-15    1e-21 ->  9.999000E-15", mhu);
        ("quax902 quantize   9.999E-15    1e-20 ->  9.99900E-15", mhu);
        ("quax903 quantize   9.999E-15    1e-19 ->  9.9990E-15", mhu);
        ("quax904 quantize   9.999E-15    1e-18 ->  9.999E-15", mhu);
        ("quax905 quantize   9.999E-15    1e-17 ->  1.000E-14 Inexact Rounded", mhu);
        ("quax906 quantize   9.999E-15    1e-16 ->  1.00E-14  Inexact Rounded", mhu);
        ("quax907 quantize   9.999E-15    1e-15 ->  1.0E-14   Inexact Rounded", mhu);
        ("quax908 quantize   9.999E-15    1e-14 ->  1E-14     Inexact Rounded", mhu);
        ("quax909 quantize   9.999E-15    1e-13 ->  0E-13     Inexact Rounded", mhu);
        ("quax910 quantize   9.999E-15    1e-12 ->  0E-12     Inexact Rounded", mhu);
        ("quax911 quantize   9.999E-15    1e-11 ->  0E-11     Inexact Rounded", mhu);
        ("quax912 quantize   9.999E-15    1e-10 ->  0E-10     Inexact Rounded", mhu);
        ("quax913 quantize   9.999E-15     1e-9 ->  0E-9      Inexact Rounded", mhu);
        ("quax914 quantize   9.999E-15     1e-8 ->  0E-8      Inexact Rounded", mhu);
        ("quax915 quantize   9.999E-15     1e-7 ->  0E-7      Inexact Rounded", mhu);
        ("quax916 quantize   9.999E-15     1e-6 ->  0.000000  Inexact Rounded", mhu);
        ("quax917 quantize   9.999E-15     1e-5 ->  0.00000   Inexact Rounded", mhu);
        ("quax918 quantize   9.999E-15     1e-4 ->  0.0000    Inexact Rounded", mhu);
        ("quax919 quantize   9.999E-15     1e-3 ->  0.000     Inexact Rounded", mhu);
        ("quax920 quantize   9.999E-15     1e-2 ->  0.00      Inexact Rounded", mhu);
        ("quax921 quantize   9.999E-15     1e-1 ->  0.0       Inexact Rounded", mhu);
        ("quax922 quantize   9.999E-15      1e0 ->  0         Inexact Rounded", mhu);
        ("quax923 quantize   9.999E-15      1e1 ->  0E+1      Inexact Rounded", mhu);
    
            //             //precision: 6
        ("quax930 quantize   9.999E-15    1e-22 ->  9.9990000E-15       Invalid_operation", mhu);
        ("quax931 quantize   9.999E-15    1e-21 ->  9.999000E-15       Invalid_operation", mhu);
        ("quax932 quantize   9.999E-15    1e-20 ->  9.99900E-15", mhu);
        ("quax933 quantize   9.999E-15    1e-19 ->  9.9990E-15", mhu);
        ("quax934 quantize   9.999E-15    1e-18 ->  9.999E-15", mhu);
        ("quax935 quantize   9.999E-15    1e-17 ->  1.000E-14 Inexact Rounded", mhu);
        ("quax936 quantize   9.999E-15    1e-16 ->  1.00E-14  Inexact Rounded", mhu);
        ("quax937 quantize   9.999E-15    1e-15 ->  1.0E-14   Inexact Rounded", mhu);
        ("quax938 quantize   9.999E-15    1e-14 ->  1E-14     Inexact Rounded", mhu);
        ("quax939 quantize   9.999E-15    1e-13 ->  0E-13     Inexact Rounded", mhu);
        ("quax940 quantize   9.999E-15    1e-12 ->  0E-12     Inexact Rounded", mhu);
        ("quax941 quantize   9.999E-15    1e-11 ->  0E-11     Inexact Rounded", mhu);
        ("quax942 quantize   9.999E-15    1e-10 ->  0E-10     Inexact Rounded", mhu);
        ("quax943 quantize   9.999E-15     1e-9 ->  0E-9      Inexact Rounded", mhu);
        ("quax944 quantize   9.999E-15     1e-8 ->  0E-8      Inexact Rounded", mhu);
        ("quax945 quantize   9.999E-15     1e-7 ->  0E-7      Inexact Rounded", mhu);
        ("quax946 quantize   9.999E-15     1e-6 ->  0.000000  Inexact Rounded", mhu);
        ("quax947 quantize   9.999E-15     1e-5 ->  0.00000   Inexact Rounded", mhu);
        ("quax948 quantize   9.999E-15     1e-4 ->  0.0000    Inexact Rounded", mhu);
        ("quax949 quantize   9.999E-15     1e-3 ->  0.000     Inexact Rounded", mhu);
        ("quax950 quantize   9.999E-15     1e-2 ->  0.00      Inexact Rounded", mhu);
        ("quax951 quantize   9.999E-15     1e-1 ->  0.0       Inexact Rounded", mhu);
        ("quax952 quantize   9.999E-15      1e0 ->  0         Inexact Rounded", mhu);
        ("quax953 quantize   9.999E-15      1e1 ->  0E+1      Inexact Rounded", mhu);
    
        //precision: 3
        ("quax960 quantize   9.999E-15    1e-22 ->  9.9990000E-15       Invalid_operation", mhu); // mod: NaN
        ("quax961 quantize   9.999E-15    1e-21 ->  9.999000E-15       Invalid_operation", mhu); // mod: NaN
        ("quax962 quantize   9.999E-15    1e-20 ->  9.99900E-15       Invalid_operation", mhu); // mod: NaN
        ("quax963 quantize   9.999E-15    1e-19 ->  9.9990E-15       Invalid_operation", mhu); // mod: NaN
        ("quax964 quantize   9.999E-15    1e-18 ->  9.999E-15       Invalid_operation", mhu); // mod: NaN
        ("quax965 quantize   9.999E-15    1e-17 ->  1.000E-14       Invalid_operation", mhu); // mod: NaN
        ("quax966 quantize   9.999E-15    1e-16 ->  1.00E-14  Inexact Rounded", mhu);
        ("quax967 quantize   9.999E-15    1e-15 ->  1.0E-14   Inexact Rounded", mhu);
        ("quax968 quantize   9.999E-15    1e-14 ->  1E-14     Inexact Rounded", mhu);
        ("quax969 quantize   9.999E-15    1e-13 ->  0E-13     Inexact Rounded", mhu);
        ("quax970 quantize   9.999E-15    1e-12 ->  0E-12     Inexact Rounded", mhu);
        ("quax971 quantize   9.999E-15    1e-11 ->  0E-11     Inexact Rounded", mhu);
        ("quax972 quantize   9.999E-15    1e-10 ->  0E-10     Inexact Rounded", mhu);
        ("quax973 quantize   9.999E-15     1e-9 ->  0E-9      Inexact Rounded", mhu);
        ("quax974 quantize   9.999E-15     1e-8 ->  0E-8      Inexact Rounded", mhu);
        ("quax975 quantize   9.999E-15     1e-7 ->  0E-7      Inexact Rounded", mhu);
        ("quax976 quantize   9.999E-15     1e-6 ->  0.000000  Inexact Rounded", mhu);
        ("quax977 quantize   9.999E-15     1e-5 ->  0.00000   Inexact Rounded", mhu);
        ("quax978 quantize   9.999E-15     1e-4 ->  0.0000    Inexact Rounded", mhu);
        ("quax979 quantize   9.999E-15     1e-3 ->  0.000     Inexact Rounded", mhu);
        ("quax980 quantize   9.999E-15     1e-2 ->  0.00      Inexact Rounded", mhu);
        ("quax981 quantize   9.999E-15     1e-1 ->  0.0       Inexact Rounded", mhu);
        ("quax982 quantize   9.999E-15      1e0 ->  0         Inexact Rounded", mhu);
        ("quax983 quantize   9.999E-15      1e1 ->  0E+1      Inexact Rounded", mhu);
    ]

[<Tests>]
let quantizeSpecNegativeList = testList "quantize spec negative" (createTQTests quantizeSpecNegatives)

[<Tests>]
let quantizeSpecGeneralList = testList "quantize spec negative" (createTQTests quantizeSpecGeneral)

[<Tests>]
let quantizeSpecBaseTestsWithNonOneCoeffsList = testList "quantize spec base tests with non-one coeffs" (createTQTests quantizeSpecBaseTestsWithNonOneCoeffs)

[<Tests>]
let quantizeSpecTestPosExponentsList = testList  "quantize spec test pos exponent" (createTQTests quantizeSpecTestPosExponents)

[<Tests>]
let quantizeSpecRoundUpFromBelowList = testList  "quantize spec round up from below" (createTQTests quantizeSpecRoundUpFromBelow)

[<Tests>]
let quantizeSpecSomeIndividualsList = testList "quantize spec some individuals" (createTQTests quantizeSpecSomeIndividuals)

[<Tests>]
let quantizeSpecExamplesFromEmailList = testList "quantize spec examples from email" (createTQTests quantizeSpecExamplesFromEmail)


[<Tests>]
let quantizeSpecSome9999ExamplesList = testList "quantize spec esome 9999 examples" (createTQTests quantizeSpecSome9999Examples)


let absTest argStr c shouldStr =
    let arg = BigDecimal.Parse(argStr)
    let result = BigDecimal.Abs(arg,c)
    let resultStr = result.ToScientificString()
    Expect.equal resultStr shouldStr "Absolute value does not match"

let absTestFromString test c = 
    let argStr, resultStr = getTwoArgs test
    absTest argStr c resultStr

let createSpecAbsTests data =
    data
    |> List.map (fun (test, c) ->
           testCase (sprintf "quantize'%s' with context %s " test (c.ToString()) ) <| fun _ -> 
                absTestFromString test c
           )

let c9 = Context.Create(9u, RoundingMode.HalfUp);
let c7 = Context.Create(7u, RoundingMode.HalfUp);
let c6 = Context.Create(6u, RoundingMode.HalfUp);
let c3 = Context.Create(3u, RoundingMode.HalfUp);


let specAbsTests =
    [
        ("absx001 abs '1'      -> '1'", c9);
        ("absx002 abs '-1'     -> '1'", c9);
        ("absx003 abs '1.00'   -> '1.00'", c9);
        ("absx004 abs '-1.00'  -> '1.00'", c9);
        ("absx005 abs '0'      -> '0'", c9);
        ("absx006 abs '0.00'   -> '0.00'", c9);
        ("absx007 abs '00.0'   -> '0.0'", c9);
        ("absx008 abs '00.00'  -> '0.00'", c9);
        ("absx009 abs '00'     -> '0'", c9);
    
        ("absx010 abs '-2'     -> '2'", c9);
        ("absx011 abs '2'      -> '2'", c9);
        ("absx012 abs '-2.00'  -> '2.00'", c9);
        ("absx013 abs '2.00'   -> '2.00'", c9);
        ("absx014 abs '-0'     -> '0'", c9);
        ("absx015 abs '-0.00'  -> '0.00'", c9);
        ("absx016 abs '-00.0'  -> '0.0'", c9);
        ("absx017 abs '-00.00' -> '0.00'", c9);
        ("absx018 abs '-00'    -> '0'", c9);
    
        ("absx020 abs '-2000000' -> '2000000'", c9);
        ("absx021 abs '2000000'  -> '2000000'", c9);
    
        //  precision: 7
        ("absx022 abs '-2000000' -> '2000000'", c7);
        ("absx023 abs '2000000'  -> '2000000'", c7);
    
        //  precision: 6
        ("absx024 abs '-2000000' -> '2.00000E+6' Rounded", c6);
        ("absx025 abs '2000000'  -> '2.00000E+6' Rounded", c6);
    
    
        //  precision: 3
        ("absx026 abs '-2000000' -> '2.00E+6' Rounded", c3);
        ("absx027 abs '2000000'  -> '2.00E+6' Rounded", c3);
    
        ("absx030 abs '+0.1'            -> '0.1'", c3);
        ("absx031 abs '-0.1'            -> '0.1'", c3);
        ("absx032 abs '+0.01'           -> '0.01'", c3);
        ("absx033 abs '-0.01'           -> '0.01'", c3);
        ("absx034 abs '+0.001'          -> '0.001'", c3);
        ("absx035 abs '-0.001'          -> '0.001'", c3);
        ("absx036 abs '+0.000001'       -> '0.000001'", c3);
        ("absx037 abs '-0.000001'       -> '0.000001'", c3);
        ("absx038 abs '+0.000000000001' -> '1E-12'", c3);
        ("absx039 abs '-0.000000000001' -> '1E-12'", c3);
    
        //-- examples from decArith
        //precision: 9
        ("absx040 abs '2.1'     ->  '2.1'", c9);
        ("absx041 abs '-100'    ->  '100'", c9);
        ("absx042 abs '101.5'   ->  '101.5'", c9);
        ("absx043 abs '-101.5'  ->  '101.5'", c9);
    
        //-- more fixed, potential LHS swaps/overlays if done by subtract 0
        //             //precision: 9
        ("absx060 abs '-56267E-10'  -> '0.0000056267'", c9);
        ("absx061 abs '-56267E-5'   -> '0.56267'", c9);
        ("absx062 abs '-56267E-2'   -> '562.67'", c9);
        ("absx063 abs '-56267E-1'   -> '5626.7'", c9);
        ("absx065 abs '-56267E-0'   -> '56267'", c9);
    ]


[<Tests>]
let specAbsTestList = testList "quantize spec esome 9999 examples" (createSpecAbsTests specAbsTests)



let addTest arg1Str arg2Str c resultStr =
    let arg1 = BigDecimal.Parse(arg1Str)
    let arg2 = BigDecimal.Parse(arg2Str)
    let sum = arg1.Add(arg2,c)
    let sumStr = sum.ToScientificString()
    Expect.equal sumStr resultStr "Result of addition"

let addTestFromStr test c =
    let arg1Str, arg2Str, resultStr = getThreeArgs test
    addTest arg1Str arg2Str c resultStr


let createAddTests data =
    data
    |> List.map (fun (test, c) ->
           testCase (sprintf "'%s' with context %s " test (c.ToString()) ) <| fun _ -> 
                addTestFromStr test c
           )

let c6hu = Context.Create(6u, RoundingMode.HalfUp)
let c15hu = Context.Create(15u, RoundingMode.HalfUp)
let c9he = Context.Create(9u, RoundingMode.HalfEven)
let c9d = Context.Create(9u, RoundingMode.Down)
let c3hu = Context.Create(3u, RoundingMode.HalfUp);
let c3hd = Context.Create(3u, RoundingMode.HalfDown);
let c6hd = Context.Create(6u, RoundingMode.HalfDown);
let c6he = Context.Create(6u, RoundingMode.HalfEven);
let c7hu = Context.Create(7u, RoundingMode.HalfUp);
let c10hd = Context.Create(10u, RoundingMode.HalfDown);
let c10hu = Context.Create(10u, RoundingMode.HalfUp);
let c10he = Context.Create(10u, RoundingMode.HalfEven);


let addTests =
    [
        //-- [first group are 'quick confidence check']
        ("addx001 add 1       1       ->  2", c9hu);
        ("addx002 add 2       3       ->  5", c9hu);
        ("addx003 add '5.75'  '3.3'   ->  9.05", c9hu);
        ("addx004 add '5'     '-3'    ->  2", c9hu);
        ("addx005 add '-5'    '-3'    ->  -8", c9hu);
        ("addx006 add '-7'    '2.5'   ->  -4.5", c9hu);
        ("addx007 add '0.7'   '0.3'   ->  1.0", c9hu);
        ("addx008 add '1.25'  '1.25'  ->  2.50", c9hu);
        ("addx009 add '1.23456789'  '1.00000000' -> '2.23456789'", c9hu);
        ("addx010 add '1.23456789'  '1.00000011' -> '2.23456800'", c9hu);
    
        ("addx011 add '0.4444444444'  '0.5555555555' -> '1.00000000' Inexact Rounded", c9hu);
        ("addx012 add '0.4444444440'  '0.5555555555' -> '1.00000000' Inexact Rounded", c9hu);
        ("addx013 add '0.4444444444'  '0.5555555550' -> '0.999999999' Inexact Rounded", c9hu);
        ("addx014 add '0.44444444449'    '0' -> '0.444444444' Inexact Rounded", c9hu);
        ("addx015 add '0.444444444499'   '0' -> '0.444444444' Inexact Rounded", c9hu);
        ("addx016 add '0.4444444444999'  '0' -> '0.444444444' Inexact Rounded", c9hu);
        ("addx017 add '0.4444444445000'  '0' -> '0.444444445' Inexact Rounded", c9hu);
        ("addx018 add '0.4444444445001'  '0' -> '0.444444445' Inexact Rounded", c9hu);
        ("addx019 add '0.444444444501'   '0' -> '0.444444445' Inexact Rounded", c9hu);
        ("addx020 add '0.44444444451'    '0' -> '0.444444445' Inexact Rounded", c9hu);
    
        ("addx021 add 0 1 -> 1", c9hu);
        ("addx022 add 1 1 -> 2", c9hu);
        ("addx023 add 2 1 -> 3", c9hu);
        ("addx024 add 3 1 -> 4", c9hu);
        ("addx025 add 4 1 -> 5", c9hu);
        ("addx026 add 5 1 -> 6", c9hu);
        ("addx027 add 6 1 -> 7", c9hu);
        ("addx028 add 7 1 -> 8", c9hu);
        ("addx029 add 8 1 -> 9", c9hu);
        ("addx030 add 9 1 -> 10", c9hu);
    
        //-- some carrying effects
    
        ("addx031 add '0.9998'  '0.0000' -> '0.9998'", c9hu);
        ("addx032 add '0.9998'  '0.0001' -> '0.9999'", c9hu);
        ("addx033 add '0.9998'  '0.0002' -> '1.0000'", c9hu);
        ("addx034 add '0.9998'  '0.0003' -> '1.0001'", c9hu);
    
        ("addx035 add '70'  '10000e+9' -> '1.00000000E+13' Inexact Rounded", c9hu);
        ("addx036 add '700'  '10000e+9' -> '1.00000000E+13' Inexact Rounded", c9hu);
        ("addx037 add '7000'  '10000e+9' -> '1.00000000E+13' Inexact Rounded", c9hu);
        ("addx038 add '70000'  '10000e+9' -> '1.00000001E+13' Inexact Rounded", c9hu);
        ("addx039 add '700000'  '10000e+9' -> '1.00000007E+13' Rounded", c9hu);
    
        // -- symmetry:
        ("addx040 add '10000e+9'  '70' -> '1.00000000E+13' Inexact Rounded", c9hu);
        ("addx041 add '10000e+9'  '700' -> '1.00000000E+13' Inexact Rounded", c9hu);
        ("addx042 add '10000e+9'  '7000' -> '1.00000000E+13' Inexact Rounded", c9hu);
        ("addx044 add '10000e+9'  '70000' -> '1.00000001E+13' Inexact Rounded", c9hu);
        ("addx045 add '10000e+9'  '700000' -> '1.00000007E+13' Rounded", c9hu);
    
        // -- same, higher precision
        // precision: 15
        ("addx046 add '10000e+9'  '7' -> '10000000000007'", c15hu);
        ("addx047 add '10000e+9'  '70' -> '10000000000070'", c15hu);
        ("addx048 add '10000e+9'  '700' -> '10000000000700'", c15hu);
        ("addx049 add '10000e+9'  '7000' -> '10000000007000'", c15hu);
        ("addx050 add '10000e+9'  '70000' -> '10000000070000'", c15hu);
        ("addx051 add '10000e+9'  '700000' -> '10000000700000'", c15hu);
        ("addx052 add '10000e+9'  '7000000' -> '10000007000000'", c15hu);
    
        // -- examples from decarith
        ("addx053 add '12' '7.00' -> '19.00'", c9hu);
        ("addx054 add '1.3' '-1.07' -> '0.23'", c9hu);
        ("addx055 add '1.3' '-1.30' -> '0.00'", c9hu);
        ("addx056 add '1.3' '-2.07' -> '-0.77'", c9hu);
        ("addx057 add '1E+2' '1E+4' -> '1.01E+4'", c9hu);
    
        // -- zero preservation
        // precision: 6
        ("addx060 add '10000e+9'  '70000' -> '1.00000E+13' Inexact Rounded", c6hu);
        ("addx061 add 1 '0.0001' -> '1.0001'", c6hu);
        ("addx062 add 1 '0.00001' -> '1.00001'", c6hu);
        ("addx063 add 1 '0.000001' -> '1.00000' Inexact Rounded", c6hu);
        ("addx064 add 1 '0.0000001' -> '1.00000' Inexact Rounded", c6hu);
        ("addx065 add 1 '0.00000001' -> '1.00000' Inexact Rounded", c6hu);
    
        // -- some funny zeros [in case of bad signum]
        ("addx070 add 1  0    -> 1", c6hu);
        ("addx071 add 1 0.    -> 1", c6hu);
        ("addx072 add 1  .0   -> 1.0", c6hu);
        ("addx073 add 1 0.0   -> 1.0", c6hu);
        ("addx074 add 1 0.00  -> 1.00", c6hu);
        ("addx075 add  0  1   -> 1", c6hu);
        ("addx076 add 0.  1   -> 1", c6hu);
        ("addx077 add  .0 1   -> 1.0", c6hu);
        ("addx078 add 0.0 1   -> 1.0", c6hu);
        ("addx079 add 0.00 1  -> 1.00", c6hu);
    
        // precision: 9
    
        // -- some carries
        ("addx080 add 999999998 1  -> 999999999", c9hu);
        ("addx081 add 999999999 1  -> 1.00000000E+9 Rounded", c9hu);
        ("addx082 add  99999999 1  -> 100000000", c9hu);
        ("addx083 add   9999999 1  -> 10000000", c9hu);
        ("addx084 add    999999 1  -> 1000000", c9hu);
        ("addx085 add     99999 1  -> 100000", c9hu);
        ("addx086 add      9999 1  -> 10000", c9hu);
        ("addx087 add       999 1  -> 1000", c9hu);
        ("addx088 add        99 1  -> 100", c9hu);
        ("addx089 add         9 1  -> 10", c9hu);
    
    
        //-- more LHS swaps
        ("addx090 add '-56267E-10'   0 ->  '-0.0000056267'", c9hu);
        ("addx091 add '-56267E-6'    0 ->  '-0.056267'", c9hu);
        ("addx092 add '-56267E-5'    0 ->  '-0.56267'", c9hu);
        ("addx093 add '-56267E-4'    0 ->  '-5.6267'", c9hu);
        ("addx094 add '-56267E-3'    0 ->  '-56.267'", c9hu);
        ("addx095 add '-56267E-2'    0 ->  '-562.67'", c9hu);
        ("addx096 add '-56267E-1'    0 ->  '-5626.7'", c9hu);
        ("addx097 add '-56267E-0'    0 ->  '-56267'", c9hu);
        ("addx098 add '-5E-10'       0 ->  '-5E-10'", c9hu);
        ("addx099 add '-5E-7'        0 ->  '-5E-7'", c9hu);
        ("addx100 add '-5E-6'        0 ->  '-0.000005'", c9hu);
        ("addx101 add '-5E-5'        0 ->  '-0.00005'", c9hu);
        ("addx102 add '-5E-4'        0 ->  '-0.0005'", c9hu);
        ("addx103 add '-5E-1'        0 ->  '-0.5'", c9hu);
        ("addx104 add '-5E0'         0 ->  '-5'", c9hu);
        ("addx105 add '-5E1'         0 ->  '-50'", c9hu);
        ("addx106 add '-5E5'         0 ->  '-500000'", c9hu);
        ("addx107 add '-5E8'         0 ->  '-500000000'", c9hu);
        ("addx108 add '-5E9'         0 ->  '-5.00000000E+9'   Rounded", c9hu);
        ("addx109 add '-5E10'        0 ->  '-5.00000000E+10'  Rounded", c9hu);
        ("addx110 add '-5E11'        0 ->  '-5.00000000E+11'  Rounded", c9hu);
        ("addx111 add '-5E100'       0 ->  '-5.00000000E+100' Rounded", c9hu);
    
        // -- more RHS swaps
        ("addx113 add 0  '-56267E-10' ->  '-0.0000056267'", c9hu);
        ("addx114 add 0  '-56267E-6'  ->  '-0.056267'", c9hu);
        ("addx116 add 0  '-56267E-5'  ->  '-0.56267'", c9hu);
        ("addx117 add 0  '-56267E-4'  ->  '-5.6267'", c9hu);
        ("addx119 add 0  '-56267E-3'  ->  '-56.267'", c9hu);
        ("addx120 add 0  '-56267E-2'  ->  '-562.67'", c9hu);
        ("addx121 add 0  '-56267E-1'  ->  '-5626.7'", c9hu);
        ("addx122 add 0  '-56267E-0'  ->  '-56267'", c9hu);
        ("addx123 add 0  '-5E-10'     ->  '-5E-10'", c9hu);
        ("addx124 add 0  '-5E-7'      ->  '-5E-7'", c9hu);
        ("addx125 add 0  '-5E-6'      ->  '-0.000005'", c9hu);
        ("addx126 add 0  '-5E-5'      ->  '-0.00005'", c9hu);
        ("addx127 add 0  '-5E-4'      ->  '-0.0005'", c9hu);
        ("addx128 add 0  '-5E-1'      ->  '-0.5'", c9hu);
        ("addx129 add 0  '-5E0'       ->  '-5'", c9hu);
        ("addx130 add 0  '-5E1'       ->  '-50'", c9hu);
        ("addx131 add 0  '-5E5'       ->  '-500000'", c9hu);
        ("addx132 add 0  '-5E8'       ->  '-500000000'", c9hu);
        ("addx133 add 0  '-5E9'       ->  '-5.00000000E+9'    Rounded", c9hu);
        ("addx134 add 0  '-5E10'      ->  '-5.00000000E+10'   Rounded", c9hu);
        ("addx135 add 0  '-5E11'      ->  '-5.00000000E+11'   Rounded", c9hu);
        ("addx136 add 0  '-5E100'     ->  '-5.00000000E+100'  Rounded", c9hu);
    
        // -- related
        ("addx137 add  1  '0E-12'      ->  '1.00000000'  Rounded", c9hu);
        ("addx138 add -1  '0E-12'      ->  '-1.00000000' Rounded", c9hu);
        ("addx139 add '0E-12' 1        ->  '1.00000000'  Rounded", c9hu);
        ("addx140 add '0E-12' -1       ->  '-1.00000000' Rounded", c9hu);
        ("addx141 add 1E+4    0.0000   ->  '10000.0000'", c9hu);
        ("addx142 add 1E+4    0.00000  ->  '10000.0000'  Rounded", c9hu);
        ("addx143 add 0.000   1E+5     ->  '100000.000'", c9hu);
        ("addx144 add 0.0000  1E+5     ->  '100000.000'  Rounded", c9hu);
    
        // -- [some of the next group are really constructor tests]
        ("addx146 add '00.0'  0       ->  '0.0'", c9hu);
        ("addx147 add '0.00'  0       ->  '0.00'", c9hu);
        ("addx148 add  0      '0.00'  ->  '0.00'", c9hu);
        ("addx149 add  0      '00.0'  ->  '0.0'", c9hu);
        ("addx150 add '00.0'  '0.00'  ->  '0.00'", c9hu);
        ("addx151 add '0.00'  '00.0'  ->  '0.00'", c9hu);
        ("addx152 add '3'     '.3'    ->  '3.3'", c9hu);
        ("addx153 add '3.'    '.3'    ->  '3.3'", c9hu);
        ("addx154 add '3.0'   '.3'    ->  '3.3'", c9hu);
        ("addx155 add '3.00'  '.3'    ->  '3.30'", c9hu);
        ("addx156 add '3'     '3'     ->  '6'", c9hu);
        ("addx157 add '3'     '+3'    ->  '6'", c9hu);
        ("addx158 add '3'     '-3'    ->  '0'", c9hu);
        ("addx159 add '0.3'   '-0.3'  ->  '0.0'", c9hu);
        ("addx160 add '0.03'  '-0.03' ->  '0.00'", c9hu);
    
        // -- try borderline precision, with carries, etc.
        // precision: 15
        ("addx161 add '1E+12' '-1'    -> '999999999999'", c15hu);
        ("addx162 add '1E+12'  '1.11' -> '1000000000001.11'", c15hu);
        ("addx163 add '1.11'  '1E+12' -> '1000000000001.11'", c15hu);
        ("addx164 add '-1'    '1E+12' -> '999999999999'", c15hu);
        ("addx165 add '7E+12' '-1'    -> '6999999999999'", c15hu);
        ("addx166 add '7E+12'  '1.11' -> '7000000000001.11'", c15hu);
        ("addx167 add '1.11'  '7E+12' -> '7000000000001.11'", c15hu);
        ("addx168 add '-1'    '7E+12' -> '6999999999999'", c15hu);
    
        // --            123456789012345      123456789012345      1 23456789012345
        ("addx170 add '0.444444444444444'  '0.555555555555563' -> '1.00000000000001' Inexact Rounded", c15hu);
        ("addx171 add '0.444444444444444'  '0.555555555555562' -> '1.00000000000001' Inexact Rounded", c15hu);
        ("addx172 add '0.444444444444444'  '0.555555555555561' -> '1.00000000000001' Inexact Rounded", c15hu);
        ("addx173 add '0.444444444444444'  '0.555555555555560' -> '1.00000000000000' Inexact Rounded", c15hu);
        ("addx174 add '0.444444444444444'  '0.555555555555559' -> '1.00000000000000' Inexact Rounded", c15hu);
        ("addx175 add '0.444444444444444'  '0.555555555555558' -> '1.00000000000000' Inexact Rounded", c15hu);
        ("addx176 add '0.444444444444444'  '0.555555555555557' -> '1.00000000000000' Inexact Rounded", c15hu);
        ("addx177 add '0.444444444444444'  '0.555555555555556' -> '1.00000000000000' Rounded", c15hu);
        ("addx178 add '0.444444444444444'  '0.555555555555555' -> '0.999999999999999'", c15hu);
        ("addx179 add '0.444444444444444'  '0.555555555555554' -> '0.999999999999998'", c15hu);
        ("addx180 add '0.444444444444444'  '0.555555555555553' -> '0.999999999999997'", c15hu);
        ("addx181 add '0.444444444444444'  '0.555555555555552' -> '0.999999999999996'", c15hu);
        ("addx182 add '0.444444444444444'  '0.555555555555551' -> '0.999999999999995'", c15hu);
        ("addx183 add '0.444444444444444'  '0.555555555555550' -> '0.999999999999994'", c15hu);
    
        // -- and some more, including residue effects and different roundings
        // precision: 9
        // rounding: half_up
        ("addx200 add '123456789' 0             -> '123456789'", c9hu);
        ("addx201 add '123456789' 0.000000001   -> '123456789' Inexact Rounded", c9hu);
        ("addx202 add '123456789' 0.000001      -> '123456789' Inexact Rounded", c9hu);
        ("addx203 add '123456789' 0.1           -> '123456789' Inexact Rounded", c9hu);
        ("addx204 add '123456789' 0.4           -> '123456789' Inexact Rounded", c9hu);
        ("addx205 add '123456789' 0.49          -> '123456789' Inexact Rounded", c9hu);
        ("addx206 add '123456789' 0.499999      -> '123456789' Inexact Rounded", c9hu);
        ("addx207 add '123456789' 0.499999999   -> '123456789' Inexact Rounded", c9hu);
        ("addx208 add '123456789' 0.5           -> '123456790' Inexact Rounded", c9hu);
        ("addx209 add '123456789' 0.500000001   -> '123456790' Inexact Rounded", c9hu);
        ("addx210 add '123456789' 0.500001      -> '123456790' Inexact Rounded", c9hu);
        ("addx211 add '123456789' 0.51          -> '123456790' Inexact Rounded", c9hu);
        ("addx212 add '123456789' 0.6           -> '123456790' Inexact Rounded", c9hu);
        ("addx213 add '123456789' 0.9           -> '123456790' Inexact Rounded", c9hu);
        ("addx214 add '123456789' 0.99999       -> '123456790' Inexact Rounded", c9hu);
        ("addx215 add '123456789' 0.999999999   -> '123456790' Inexact Rounded", c9hu);
        ("addx216 add '123456789' 1             -> '123456790'", c9hu);
        ("addx217 add '123456789' 1.000000001   -> '123456790' Inexact Rounded", c9hu);
        ("addx218 add '123456789' 1.00001       -> '123456790' Inexact Rounded", c9hu);
        ("addx219 add '123456789' 1.1           -> '123456790' Inexact Rounded", c9hu);
    
        // rounding: half_even
        ("addx220 add '123456789' 0             -> '123456789'", c9he);
        ("addx221 add '123456789' 0.000000001   -> '123456789' Inexact Rounded", c9he);
        ("addx222 add '123456789' 0.000001      -> '123456789' Inexact Rounded", c9he);
        ("addx223 add '123456789' 0.1           -> '123456789' Inexact Rounded", c9he);
        ("addx224 add '123456789' 0.4           -> '123456789' Inexact Rounded", c9he);
        ("addx225 add '123456789' 0.49          -> '123456789' Inexact Rounded", c9he);
        ("addx226 add '123456789' 0.499999      -> '123456789' Inexact Rounded", c9he);
        ("addx227 add '123456789' 0.499999999   -> '123456789' Inexact Rounded", c9he);
        ("addx228 add '123456789' 0.5           -> '123456790' Inexact Rounded", c9he);
        ("addx229 add '123456789' 0.500000001   -> '123456790' Inexact Rounded", c9he);
        ("addx230 add '123456789' 0.500001      -> '123456790' Inexact Rounded", c9he);
        ("addx231 add '123456789' 0.51          -> '123456790' Inexact Rounded", c9he);
        ("addx232 add '123456789' 0.6           -> '123456790' Inexact Rounded", c9he);
        ("addx233 add '123456789' 0.9           -> '123456790' Inexact Rounded", c9he);
        ("addx234 add '123456789' 0.99999       -> '123456790' Inexact Rounded", c9he);
        ("addx235 add '123456789' 0.999999999   -> '123456790' Inexact Rounded", c9he);
        ("addx236 add '123456789' 1             -> '123456790'", c9he);
        ("addx237 add '123456789' 1.00000001    -> '123456790' Inexact Rounded", c9he);
        ("addx238 add '123456789' 1.00001       -> '123456790' Inexact Rounded", c9he);
        ("addx239 add '123456789' 1.1           -> '123456790' Inexact Rounded", c9he);
        
        // -- critical few with even bottom digit...
        ("addx240 add '123456788' 0.499999999   -> '123456788' Inexact Rounded", c9he);
        ("addx241 add '123456788' 0.5           -> '123456788' Inexact Rounded", c9he);
        ("addx242 add '123456788' 0.500000001   -> '123456789' Inexact Rounded", c9he);

    
        // rounding: down
        ("addx250 add '123456789' 0             -> '123456789'", c9d);
        ("addx251 add '123456789' 0.000000001   -> '123456789' Inexact Rounded", c9d);
        ("addx252 add '123456789' 0.000001      -> '123456789' Inexact Rounded", c9d);
        ("addx253 add '123456789' 0.1           -> '123456789' Inexact Rounded", c9d);
        ("addx254 add '123456789' 0.4           -> '123456789' Inexact Rounded", c9d);
        ("addx255 add '123456789' 0.49          -> '123456789' Inexact Rounded", c9d);
        ("addx256 add '123456789' 0.499999      -> '123456789' Inexact Rounded", c9d);
        ("addx257 add '123456789' 0.499999999   -> '123456789' Inexact Rounded", c9d);
        ("addx258 add '123456789' 0.5           -> '123456789' Inexact Rounded", c9d);
        ("addx259 add '123456789' 0.500000001   -> '123456789' Inexact Rounded", c9d);
        ("addx260 add '123456789' 0.500001      -> '123456789' Inexact Rounded", c9d);
        ("addx261 add '123456789' 0.51          -> '123456789' Inexact Rounded", c9d);
        ("addx262 add '123456789' 0.6           -> '123456789' Inexact Rounded", c9d);
        ("addx263 add '123456789' 0.9           -> '123456789' Inexact Rounded", c9d);
        ("addx264 add '123456789' 0.99999       -> '123456789' Inexact Rounded", c9d);
        ("addx265 add '123456789' 0.999999999   -> '123456789' Inexact Rounded", c9d);
        ("addx266 add '123456789' 1             -> '123456790'", c9d);
        ("addx267 add '123456789' 1.00000001    -> '123456790' Inexact Rounded", c9d);
        ("addx268 add '123456789' 1.00001       -> '123456790' Inexact Rounded", c9d);
        ("addx269 add '123456789' 1.1           -> '123456790' Inexact Rounded", c9d);
    
        // -- input preparation tests (operands should not be rounded)
        // precision: 3
        // rounding: half_up
        ("addx270 add '12345678900000'  9999999999999 ->  '2.23E+13' Inexact Rounded", c3hu);
        ("addx271 add  '9999999999999' 12345678900000 ->  '2.23E+13' Inexact Rounded", c3hu);
    
        ("addx272 add '12E+3'  '3444'   ->  '1.54E+4' Inexact Rounded", c3hu);
        ("addx273 add '12E+3'  '3446'   ->  '1.54E+4' Inexact Rounded", c3hu);
        ("addx274 add '12E+3'  '3449.9' ->  '1.54E+4' Inexact Rounded", c3hu);
        ("addx275 add '12E+3'  '3450.0' ->  '1.55E+4' Inexact Rounded", c3hu);
        ("addx276 add '12E+3'  '3450.1' ->  '1.55E+4' Inexact Rounded", c3hu);
        ("addx277 add '12E+3'  '3454'   ->  '1.55E+4' Inexact Rounded", c3hu);
        ("addx278 add '12E+3'  '3456'   ->  '1.55E+4' Inexact Rounded", c3hu);
    
        ("addx281 add '3444'   '12E+3'  ->  '1.54E+4' Inexact Rounded", c3hu);
        ("addx282 add '3446'   '12E+3'  ->  '1.54E+4' Inexact Rounded", c3hu);
        ("addx283 add '3449.9' '12E+3'  ->  '1.54E+4' Inexact Rounded", c3hu);
        ("addx284 add '3450.0' '12E+3'  ->  '1.55E+4' Inexact Rounded", c3hu);
        ("addx285 add '3450.1' '12E+3'  ->  '1.55E+4' Inexact Rounded", c3hu);
        ("addx286 add '3454'   '12E+3'  ->  '1.55E+4' Inexact Rounded", c3hu);
        ("addx287 add '3456'   '12E+3'  ->  '1.55E+4' Inexact Rounded", c3hu);
    
        //rounding: half_down
        ("addx291 add '3444'   '12E+3'  ->  '1.54E+4' Inexact Rounded", c3hd);
        ("addx292 add '3446'   '12E+3'  ->  '1.54E+4' Inexact Rounded", c3hd);
        ("addx293 add '3449.9' '12E+3'  ->  '1.54E+4' Inexact Rounded", c3hd);
        ("addx294 add '3450.0' '12E+3'  ->  '1.54E+4' Inexact Rounded", c3hd);
        ("addx295 add '3450.1' '12E+3'  ->  '1.55E+4' Inexact Rounded", c3hd);
        ("addx296 add '3454'   '12E+3'  ->  '1.55E+4' Inexact Rounded", c3hd);
        ("addx297 add '3456'   '12E+3'  ->  '1.55E+4' Inexact Rounded", c3hd);
    
        // -- 1 in last place tests
        // rounding: half_up
        ("addx301 add  -1   1      ->   0", c3hu);
        ("addx302 add   0   1      ->   1", c3hu);
        ("addx303 add   1   1      ->   2", c3hu);
        ("addx304 add  12   1      ->  13", c3hu);
        ("addx305 add  98   1      ->  99", c3hu);
        ("addx306 add  99   1      -> 100", c3hu);
        ("addx307 add 100   1      -> 101", c3hu);
        ("addx308 add 101   1      -> 102", c3hu);
        ("addx309 add  -1  -1      ->  -2", c3hu);
        ("addx310 add   0  -1      ->  -1", c3hu);
        ("addx311 add   1  -1      ->   0", c3hu);
        ("addx312 add  12  -1      ->  11", c3hu);
        ("addx313 add  98  -1      ->  97", c3hu);
        ("addx314 add  99  -1      ->  98", c3hu);
        ("addx315 add 100  -1      ->  99", c3hu);
        ("addx316 add 101  -1      -> 100", c3hu);
    
        ("addx321 add -0.01  0.01    ->  0.00", c3hu);
        ("addx322 add  0.00  0.01    ->  0.01", c3hu);
        ("addx323 add  0.01  0.01    ->  0.02", c3hu);
        ("addx324 add  0.12  0.01    ->  0.13", c3hu);
        ("addx325 add  0.98  0.01    ->  0.99", c3hu);
        ("addx326 add  0.99  0.01    ->  1.00", c3hu);
        ("addx327 add  1.00  0.01    ->  1.01", c3hu);
        ("addx328 add  1.01  0.01    ->  1.02", c3hu);
        ("addx329 add -0.01 -0.01    -> -0.02", c3hu);
        ("addx330 add  0.00 -0.01    -> -0.01", c3hu);
        ("addx331 add  0.01 -0.01    ->  0.00", c3hu);
        ("addx332 add  0.12 -0.01    ->  0.11", c3hu);
        ("addx333 add  0.98 -0.01    ->  0.97", c3hu);
        ("addx334 add  0.99 -0.01    ->  0.98", c3hu);
        ("addx335 add  1.00 -0.01    ->  0.99", c3hu);
        ("addx336 add  1.01 -0.01    ->  1.00", c3hu);
    
        // -- some more cases where adding 0 affects the coefficient
        //    precision: 9
        ("addx340 add 1E+3    0    ->         1000", c9hu);
        ("addx341 add 1E+8    0    ->    100000000", c9hu);
        ("addx342 add 1E+9    0    ->   1.00000000E+9   Rounded", c9hu);
        ("addx343 add 1E+10   0    ->   1.00000000E+10  Rounded", c9hu);
        // -- which simply follow from these cases ...
        ("addx344 add 1E+3    1    ->         1001", c9hu);
        ("addx345 add 1E+8    1    ->    100000001", c9hu);
        ("addx346 add 1E+9    1    ->   1.00000000E+9   Inexact Rounded", c9hu);
        ("addx347 add 1E+10   1    ->   1.00000000E+10  Inexact Rounded", c9hu);
        ("addx348 add 1E+3    7    ->         1007", c9hu);
        ("addx349 add 1E+8    7    ->    100000007", c9hu);
        ("addx350 add 1E+9    7    ->   1.00000001E+9   Inexact Rounded", c9hu);
        ("addx351 add 1E+10   7    ->   1.00000000E+10  Inexact Rounded", c9hu);
    
        // -- tryzeros cases
        // precision:   7
        // rounding:    half_up
        // maxExponent: 92
        // minexponent: -92    
        ("addx361  add 0E+50 10000E+1  -> 1.0000E+5", c7hu);
        ("addx362  add 10000E+1 0E-50  -> 100000.0  Rounded", c7hu);
        ("addx363  add 10000E+1 10000E-50  -> 100000.0  Rounded Inexact", c7hu);
        ("addx364  add 9.999999E+92 -9.999999E+92 -> 0E+86", c7hu);
    
        // -- a curiosity from JSR 13 testing
        // rounding:    half_down
        // precision:   10
        ("addx370 add 99999999 81512 -> 100081511", c10hd);
        // precision:      6
        ("addx371 add 99999999 81512 -> 1.00082E+8 Rounded Inexact", c6hd);
        // rounding:    half_up
        // precision:   10
        ("addx372 add 99999999 81512 -> 100081511", c10hu);
        // precision:      6
        ("addx373 add 99999999 81512 -> 1.00082E+8 Rounded Inexact", c6hu);
        // rounding:    half_even
        // precision:   10
        ("addx374 add 99999999 81512 -> 100081511", c10he);
        // precision:      6
        ("addx375 add 99999999 81512 -> 1.00082E+8 Rounded Inexact", c6he);
    
        // -- ulp replacement tests
        // precision: 9
        // maxexponent: 999999999
        // minexponent: -999999999
        ("addx400 add   1   77e-7       ->  1.0000077", c9he);
        ("addx401 add   1   77e-8       ->  1.00000077", c9he);
        ("addx402 add   1   77e-9       ->  1.00000008 Inexact Rounded", c9he);
        ("addx403 add   1   77e-10      ->  1.00000001 Inexact Rounded", c9he);
        ("addx404 add   1   77e-11      ->  1.00000000 Inexact Rounded", c9he);
        ("addx405 add   1   77e-12      ->  1.00000000 Inexact Rounded", c9he);
        ("addx406 add   1   77e-999     ->  1.00000000 Inexact Rounded", c9he);
        //("addx407 add   1   77e-9999999 ->  1.00000000 Inexact Rounded", c9he);
    
        ("addx410 add  10   77e-7       ->  10.0000077", c9he);
        ("addx411 add  10   77e-8       ->  10.0000008 Inexact Rounded", c9he);
        ("addx412 add  10   77e-9       ->  10.0000001 Inexact Rounded", c9he);
        ("addx413 add  10   77e-10      ->  10.0000000 Inexact Rounded", c9he);
        ("addx414 add  10   77e-11      ->  10.0000000 Inexact Rounded", c9he);
        ("addx415 add  10   77e-12      ->  10.0000000 Inexact Rounded", c9he);
        ("addx416 add  10   77e-999     ->  10.0000000 Inexact Rounded", c9he);
        //("addx417 add  10   77e-9999999 ->  10.0000000 Inexact Rounded", c9he);
    
        ("addx420 add  77e-7        1   ->  1.0000077", c9he);
        ("addx421 add  77e-8        1   ->  1.00000077", c9he);
        ("addx422 add  77e-9        1   ->  1.00000008 Inexact Rounded", c9he);
        ("addx423 add  77e-10       1   ->  1.00000001 Inexact Rounded", c9he);
        ("addx424 add  77e-11       1   ->  1.00000000 Inexact Rounded", c9he);
        ("addx425 add  77e-12       1   ->  1.00000000 Inexact Rounded", c9he);
        ("addx426 add  77e-999      1   ->  1.00000000 Inexact Rounded", c9he);
        //("addx427 add  77e-9999999  1   ->  1.00000000 Inexact Rounded", c9he);
    
        ("addx430 add  77e-7       10   ->  10.0000077", c9he);
        ("addx431 add  77e-8       10   ->  10.0000008 Inexact Rounded", c9he);
        ("addx432 add  77e-9       10   ->  10.0000001 Inexact Rounded", c9he);
        ("addx433 add  77e-10      10   ->  10.0000000 Inexact Rounded", c9he);
        ("addx434 add  77e-11      10   ->  10.0000000 Inexact Rounded", c9he);
        ("addx435 add  77e-12      10   ->  10.0000000 Inexact Rounded", c9he);
        ("addx436 add  77e-999     10   ->  10.0000000 Inexact Rounded", c9he);
        //("addx437 add  77e-9999999 10   ->  10.0000000 Inexact Rounded", c9he);
    
        // -- negative ulps
        ("addx440 add   1   -77e-7       ->  0.9999923", c9he);
        ("addx441 add   1   -77e-8       ->  0.99999923", c9he);
        ("addx442 add   1   -77e-9       ->  0.999999923", c9he);
        ("addx443 add   1   -77e-10      ->  0.999999992 Inexact Rounded", c9he);
        ("addx444 add   1   -77e-11      ->  0.999999999 Inexact Rounded", c9he);
        ("addx445 add   1   -77e-12      ->  1.00000000 Inexact Rounded", c9he);
        ("addx446 add   1   -77e-999     ->  1.00000000 Inexact Rounded", c9he);
        //("addx447 add   1   -77e-9999999 ->  1.00000000 Inexact Rounded", c9he);
    
        ("addx450 add  10   -77e-7       ->   9.9999923", c9he);
        ("addx451 add  10   -77e-8       ->   9.99999923", c9he);
        ("addx452 add  10   -77e-9       ->   9.99999992 Inexact Rounded", c9he);
        ("addx453 add  10   -77e-10      ->   9.99999999 Inexact Rounded", c9he);
        ("addx454 add  10   -77e-11      ->  10.0000000 Inexact Rounded", c9he);
        ("addx455 add  10   -77e-12      ->  10.0000000 Inexact Rounded", c9he);
        ("addx456 add  10   -77e-999     ->  10.0000000 Inexact Rounded", c9he);
        //("addx457 add  10   -77e-9999999 ->  10.0000000 Inexact Rounded", c9he);
    
        ("addx460 add  -77e-7        1   ->  0.9999923", c9he);
        ("addx461 add  -77e-8        1   ->  0.99999923", c9he);
        ("addx462 add  -77e-9        1   ->  0.999999923", c9he);
        ("addx463 add  -77e-10       1   ->  0.999999992 Inexact Rounded", c9he);
        ("addx464 add  -77e-11       1   ->  0.999999999 Inexact Rounded", c9he);
        ("addx465 add  -77e-12       1   ->  1.00000000 Inexact Rounded", c9he);
        ("addx466 add  -77e-999      1   ->  1.00000000 Inexact Rounded", c9he);
         //("addx467 add  -77e-9999999  1   ->  1.00000000 Inexact Rounded", c9he);
    
        ("addx470 add  -77e-7       10   ->   9.9999923", c9he);
        ("addx471 add  -77e-8       10   ->   9.99999923", c9he);
        ("addx472 add  -77e-9       10   ->   9.99999992 Inexact Rounded", c9he);
        ("addx473 add  -77e-10      10   ->   9.99999999 Inexact Rounded", c9he);
        ("addx474 add  -77e-11      10   ->  10.0000000 Inexact Rounded", c9he);
        ("addx475 add  -77e-12      10   ->  10.0000000 Inexact Rounded", c9he);
        ("addx476 add  -77e-999     10   ->  10.0000000 Inexact Rounded", c9he);
        //("addx477 add  -77e-9999999 10   ->  10.0000000 Inexact Rounded", c9he);
    ]


[<Tests>]
let addTestList = testList "addition examples" (createAddTests addTests)

//     [TestFixture]
//     public class BigDecimalTests
//     {
//         #region test parsing support

//         static void GetThreeArgs(string test, out string arg1, out string arg2, out string result)
//         {
//             string[] atoms = test.Split(Array.Empty<char>(), StringSplitOptions.RemoveEmptyEntries);
//             arg1 = StripSingleQuotes(atoms[2]);
//             arg2 = StripSingleQuotes(atoms[3]);
//             result = StripSingleQuotes(atoms[5]);
//         }



//         #endregion


//         #region Create from X tests

//         [Test]
//         public void CanCreateFromDouble()
//         {


//         }



//         [Test]
//         public void CanCreateFromDecimal()
//         {
//             BigDecimal.Context c9hu = new BigDecimal.Context(9, BigDecimal.RoundingMode.HalfUp);

//             TestDecimal(0M, "0", c9hu);
//             TestDecimal(1M, "1", c9hu);
//             TestDecimal(2M, "2", c9hu);
//             TestDecimal(3M, "3", c9hu);
//             TestDecimal(4M, "4", c9hu);
//             TestDecimal(5M, "5", c9hu);
//             TestDecimal(6M, "6", c9hu);
//             TestDecimal(7M, "7", c9hu);
//             TestDecimal(8M, "8", c9hu);
//             TestDecimal(9M, "9", c9hu);
//             TestDecimal(10M, "10", c9hu);
//             TestDecimal(11M, "11", c9hu);
//             TestDecimal(12M, "12", c9hu);

//             TestDecimal(1M, "1", c9hu);
//             TestDecimal(10M, "10", c9hu);
//             TestDecimal(100M, "100", c9hu);
//             TestDecimal(1000M, "1000", c9hu);
//             TestDecimal(10000M, "10000", c9hu);
//             TestDecimal(100000M, "100000", c9hu);
//             TestDecimal(1000000M, "1000000", c9hu);
//             TestDecimal(10000000M, "10000000", c9hu);
//             TestDecimal(100000000M, "100000000", c9hu);
//             TestDecimal(1000000000M, "1.00000000E+9", c9hu);
//             TestDecimal(10000000000M, "1.00000000E+10", c9hu);
//             TestDecimal(100000000000M, "1.00000000E+11", c9hu);
//             TestDecimal(1000000000000M, "1.00000000E+12", c9hu);
//             TestDecimal(10000000000000M, "1.00000000E+13", c9hu);
//             TestDecimal(100000000000000M, "1.00000000E+14", c9hu);
//             TestDecimal(1000000000000000M, "1.00000000E+15", c9hu);

//             TestDecimal(0.1M, "0.1", c9hu);
//             TestDecimal(0.01M, "0.01", c9hu);
//             TestDecimal(0.001M, "0.001", c9hu);
//             TestDecimal(0.0001M, "0.0001", c9hu);
//             TestDecimal(0.00001M, "0.00001", c9hu);
//             TestDecimal(0.000001M, "0.000001", c9hu);
//             TestDecimal(0.0000001M, "1E-7", c9hu);
//             TestDecimal(0.00000001M, "1E-8", c9hu);
//             TestDecimal(0.000000001M, "1E-9", c9hu);
//             TestDecimal(0.0000000001M, "1E-10", c9hu);
//             TestDecimal(0.00000000001M, "1E-11", c9hu);
//             TestDecimal(0.000000000001M, "1E-12", c9hu);
//             TestDecimal(0.0000000000001M, "1E-13", c9hu);
//             TestDecimal(0.00000000000001M, "1E-14", c9hu);


//             TestDecimal(1.234567891M, "1.23456789", c9hu);
//             TestDecimal(12.34567891M, "12.3456789", c9hu);
//             TestDecimal(123.4567891M, "123.456789", c9hu);
//             TestDecimal(1234.567891M, "1234.56789", c9hu);
//             TestDecimal(12345.67891M, "12345.6789", c9hu);
//             TestDecimal(123456.7891M, "123456.789", c9hu);
//             TestDecimal(1234567.891M, "1234567.89", c9hu);
//             TestDecimal(12345678.91M, "12345678.9", c9hu);
//             TestDecimal(123456789.1M, "123456789", c9hu);
//             TestDecimal(1234567891M, "1.23456789E+9", c9hu);
//             TestDecimal(12345678910M, "1.23456789E+10", c9hu);
//             TestDecimal(123456789100M, "1.23456789E+11", c9hu);
//             TestDecimal(1234567891000M, "1.23456789E+12", c9hu);

//             TestDecimal(1.234567896M, "1.23456790", c9hu);
//             TestDecimal(12.34567896M, "12.3456790", c9hu);
//             TestDecimal(123.4567896M, "123.456790", c9hu);
//             TestDecimal(1234.567896M, "1234.56790", c9hu);
//             TestDecimal(12345.67896M, "12345.6790", c9hu);
//             TestDecimal(123456.7896M, "123456.790", c9hu);
//             TestDecimal(1234567.896M, "1234567.90", c9hu);
//             TestDecimal(12345678.96M, "12345679.0", c9hu);
//             TestDecimal(123456789.6M, "123456790", c9hu);
//             TestDecimal(1234567896M, "1.23456790E+9", c9hu);
//             TestDecimal(12345678960M, "1.23456790E+10", c9hu);
//             TestDecimal(123456789600M, "1.23456790E+11", c9hu);
//             TestDecimal(1234567896000M, "1.23456790E+12", c9hu);

//             TestDecimal(0.1234567891M, "0.123456789", c9hu);
//             TestDecimal(0.01234567891M, "0.0123456789", c9hu);
//             TestDecimal(0.001234567891M, "0.00123456789", c9hu);
//             TestDecimal(0.0001234567891M, "0.000123456789", c9hu);
//             TestDecimal(0.00001234567891M, "0.0000123456789", c9hu);
//             TestDecimal(0.000001234567891M, "0.00000123456789", c9hu);
//             TestDecimal(0.0000001234567891M, "1.23456789E-7", c9hu);
//             TestDecimal(0.00000001234567891M, "1.23456789E-8", c9hu);
//             TestDecimal(0.000000001234567891M, "1.23456789E-9", c9hu);
//             TestDecimal(0.0000000001234567891M, "1.23456789E-10", c9hu);
//             TestDecimal(0.00000000001234567891M, "1.23456789E-11", c9hu);
//             TestDecimal(0.000000000001234567891M, "1.23456789E-12", c9hu);
//             TestDecimal(0.0000000000001234567891M, "1.23456789E-13", c9hu);
//             TestDecimal(0.00000000000001234567891M, "1.23456789E-14", c9hu);

//             TestDecimal(0.1234567896M, "0.123456790", c9hu);
//             TestDecimal(0.01234567896M, "0.0123456790", c9hu);
//             TestDecimal(0.001234567896M, "0.00123456790", c9hu);
//             TestDecimal(0.0001234567896M, "0.000123456790", c9hu);
//             TestDecimal(0.00001234567896M, "0.0000123456790", c9hu);
//             TestDecimal(0.000001234567896M, "0.00000123456790", c9hu);
//             TestDecimal(0.0000001234567896M, "1.23456790E-7", c9hu);
//             TestDecimal(0.00000001234567896M, "1.23456790E-8", c9hu);
//             TestDecimal(0.000000001234567896M, "1.23456790E-9", c9hu);
//             TestDecimal(0.0000000001234567896M, "1.23456790E-10", c9hu);
//             TestDecimal(0.00000000001234567896M, "1.23456790E-11", c9hu);
//             TestDecimal(0.000000000001234567896M, "1.23456790E-12", c9hu);
//             TestDecimal(0.0000000000001234567896M, "1.23456790E-13", c9hu);
//             TestDecimal(0.00000000000001234567896M, "1.23456790E-14", c9hu);


//         }


//         static private void TestDecimal(decimal v, string expectStr, BigDecimal.Context c)
//         {
//             BigDecimal d = BigDecimal.Create(v, c);
//             string gotStr = d.ToScientificString();
//             Expect(gotStr).To.Equal(expectStr);

//             if (v != 0M)
//             {
//                 d = BigDecimal.Create(-v, c);
//                 gotStr = d.ToScientificString();
//                 Expect(gotStr).To.Equal("-" + expectStr);
//             }
//         }

//         #endregion


//         #region Abs tests

//         [Test]
//         public void SpecAbsTests()
//         {



//         }



//         #endregion

//         #region Addition tests


//         [Test]
//         public void SpecAdd()
//         {





//         #endregion

//         #region Subtract tests

//         [Test]
//         public void SpecTestSub()
//         {
//             BigDecimal.Context c9hu = new BigDecimal.Context(9, BigDecimal.RoundingMode.HalfUp);

//             //version: 2.59
//             //
//             //extended:    1
//             //precision:   9
//             //rounding:    half_up
//             //maxExponent: 384
//             //minexponent: -383
//             //
//             //-- [first group are 'quick confidence check']
//             TSub("subx001 subtract  0   0  -> '0'", c9hu);
//             TSub("subx002 subtract  1   1  -> '0'", c9hu);
//             TSub("subx003 subtract  1   2  -> '-1'", c9hu);
//             TSub("subx004 subtract  2   1  -> '1'", c9hu);
//             TSub("subx005 subtract  2   2  -> '0'", c9hu);
//             TSub("subx006 subtract  3   2  -> '1'", c9hu);
//             TSub("subx007 subtract  2   3  -> '-1'", c9hu);

//             TSub("subx011 subtract -0   0  -> '0'", c9hu);  // mod: neg zero
//             TSub("subx012 subtract -1   1  -> '-2'", c9hu);
//             TSub("subx013 subtract -1   2  -> '-3'", c9hu);
//             TSub("subx014 subtract -2   1  -> '-3'", c9hu);
//             TSub("subx015 subtract -2   2  -> '-4'", c9hu);
//             TSub("subx016 subtract -3   2  -> '-5'", c9hu);
//             TSub("subx017 subtract -2   3  -> '-5'", c9hu);

//             TSub("subx021 subtract  0  -0  -> '0'", c9hu);
//             TSub("subx022 subtract  1  -1  -> '2'", c9hu);
//             TSub("subx023 subtract  1  -2  -> '3'", c9hu);
//             TSub("subx024 subtract  2  -1  -> '3'", c9hu);
//             TSub("subx025 subtract  2  -2  -> '4'", c9hu);
//             TSub("subx026 subtract  3  -2  -> '5'", c9hu);
//             TSub("subx027 subtract  2  -3  -> '5'", c9hu);

//             TSub("subx030 subtract  11  1  -> 10", c9hu);
//             TSub("subx031 subtract  10  1  ->  9", c9hu);
//             TSub("subx032 subtract  9   1  ->  8", c9hu);
//             TSub("subx033 subtract  1   1  ->  0", c9hu);
//             TSub("subx034 subtract  0   1  -> -1", c9hu);
//             TSub("subx035 subtract -1   1  -> -2", c9hu);
//             TSub("subx036 subtract -9   1  -> -10", c9hu);
//             TSub("subx037 subtract -10  1  -> -11", c9hu);
//             TSub("subx038 subtract -11  1  -> -12", c9hu);

//             TSub("subx040 subtract '5.75' '3.3'  -> '2.45'", c9hu);
//             TSub("subx041 subtract '5'    '-3'   -> '8'", c9hu);
//             TSub("subx042 subtract '-5'   '-3'   -> '-2'", c9hu);
//             TSub("subx043 subtract '-7'   '2.5'  -> '-9.5'", c9hu);
//             TSub("subx044 subtract '0.7'  '0.3'  -> '0.4'", c9hu);
//             TSub("subx045 subtract '1.3'  '0.3'  -> '1.0'", c9hu);
//             TSub("subx046 subtract '1.25' '1.25' -> '0.00'", c9hu);

//             TSub("subx050 subtract '1.23456789'    '1.00000000' -> '0.23456789'", c9hu);
//             TSub("subx051 subtract '1.23456789'    '1.00000089' -> '0.23456700'", c9hu);
//             TSub("subx052 subtract '0.5555555559'    '0.0000000001' -> '0.555555556' Inexact Rounded", c9hu);
//             TSub("subx053 subtract '0.5555555559'    '0.0000000005' -> '0.555555555' Inexact Rounded", c9hu);
//             TSub("subx054 subtract '0.4444444444'    '0.1111111111' -> '0.333333333' Inexact Rounded", c9hu);
//             TSub("subx055 subtract '1.0000000000'    '0.00000001' -> '0.999999990' Rounded", c9hu);
//             TSub("subx056 subtract '0.4444444444999'    '0' -> '0.444444444' Inexact Rounded", c9hu);
//             TSub("subx057 subtract '0.4444444445000'    '0' -> '0.444444445' Inexact Rounded", c9hu);

//             TSub("subx060 subtract '70'    '10000e+9' -> '-1.00000000E+13' Inexact Rounded", c9hu);
//             TSub("subx061 subtract '700'    '10000e+9' -> '-1.00000000E+13' Inexact Rounded", c9hu);
//             TSub("subx062 subtract '7000'    '10000e+9' -> '-9.99999999E+12' Inexact Rounded", c9hu);
//             TSub("subx063 subtract '70000'    '10000e+9' -> '-9.99999993E+12' Rounded", c9hu);
//             TSub("subx064 subtract '700000'    '10000e+9' -> '-9.99999930E+12' Rounded", c9hu);
//             //  -- symmetry:
//             TSub("subx065 subtract '10000e+9'    '70' -> '1.00000000E+13' Inexact Rounded", c9hu);
//             TSub("subx066 subtract '10000e+9'    '700' -> '1.00000000E+13' Inexact Rounded", c9hu);
//             TSub("subx067 subtract '10000e+9'    '7000' -> '9.99999999E+12' Inexact Rounded", c9hu);
//             TSub("subx068 subtract '10000e+9'    '70000' -> '9.99999993E+12' Rounded", c9hu);
//             TSub("subx069 subtract '10000e+9'    '700000' -> '9.99999930E+12' Rounded", c9hu);

//             //  -- change precision
//             TSub("subx080 subtract '10000e+9'    '70000' -> '9.99999993E+12' Rounded", c9hu);
//             //precision: 6
//             BigDecimal.Context c6hu = new BigDecimal.Context(6, BigDecimal.RoundingMode.HalfUp);
//             TSub("subx081 subtract '10000e+9'    '70000' -> '1.00000E+13' Inexact Rounded", c6hu);
//             //precision: 9

//             //  -- some of the next group are really constructor tests
//             TSub("subx090 subtract '00.0'    '0.0'  -> '0.0'", c9hu);
//             TSub("subx091 subtract '00.0'    '0.00' -> '0.00'", c9hu);
//             TSub("subx092 subtract '0.00'    '00.0' -> '0.00'", c9hu);
//             TSub("subx093 subtract '00.0'    '0.00' -> '0.00'", c9hu);
//             TSub("subx094 subtract '0.00'    '00.0' -> '0.00'", c9hu);
//             TSub("subx095 subtract '3'    '.3'   -> '2.7'", c9hu);
//             TSub("subx096 subtract '3.'   '.3'   -> '2.7'", c9hu);
//             TSub("subx097 subtract '3.0'  '.3'   -> '2.7'", c9hu);
//             TSub("subx098 subtract '3.00' '.3'   -> '2.70'", c9hu);
//             TSub("subx099 subtract '3'    '3'    -> '0'", c9hu);
//             TSub("subx100 subtract '3'    '+3'   -> '0'", c9hu);
//             TSub("subx101 subtract '3'    '-3'   -> '6'", c9hu);
//             TSub("subx102 subtract '3'    '0.3'  -> '2.7'", c9hu);
//             TSub("subx103 subtract '3.'   '0.3'  -> '2.7'", c9hu);
//             TSub("subx104 subtract '3.0'  '0.3'  -> '2.7'", c9hu);
//             TSub("subx105 subtract '3.00' '0.3'  -> '2.70'", c9hu);
//             TSub("subx106 subtract '3'    '3.0'  -> '0.0'", c9hu);
//             TSub("subx107 subtract '3'    '+3.0' -> '0.0'", c9hu);
//             TSub("subx108 subtract '3'    '-3.0' -> '6.0'", c9hu);

//             //-- the above all from add; massaged and extended.  Now some new ones...
//             //-- [particularly important for comparisons]
//             //-- NB: -xE-8 below were non-exponents pre-ANSI X3-274, and -1E-7 or 0E-7
//             //-- with input rounding.
//             TSub("subx120 subtract  '10.23456784'    '10.23456789'  -> '-5E-8'", c9hu);
//             TSub("subx121 subtract  '10.23456785'    '10.23456789'  -> '-4E-8'", c9hu);
//             TSub("subx122 subtract  '10.23456786'    '10.23456789'  -> '-3E-8'", c9hu);
//             TSub("subx123 subtract  '10.23456787'    '10.23456789'  -> '-2E-8'", c9hu);
//             TSub("subx124 subtract  '10.23456788'    '10.23456789'  -> '-1E-8'", c9hu);
//             TSub("subx125 subtract  '10.23456789'    '10.23456789'  -> '0E-8'", c9hu);
//             TSub("subx126 subtract  '10.23456790'    '10.23456789'  -> '1E-8'", c9hu);
//             TSub("subx127 subtract  '10.23456791'    '10.23456789'  -> '2E-8'", c9hu);
//             TSub("subx128 subtract  '10.23456792'    '10.23456789'  -> '3E-8'", c9hu);
//             TSub("subx129 subtract  '10.23456793'    '10.23456789'  -> '4E-8'", c9hu);
//             TSub("subx130 subtract  '10.23456794'    '10.23456789'  -> '5E-8'", c9hu);
//             TSub("subx131 subtract  '10.23456781'    '10.23456786'  -> '-5E-8'", c9hu);
//             TSub("subx132 subtract  '10.23456782'    '10.23456786'  -> '-4E-8'", c9hu);
//             TSub("subx133 subtract  '10.23456783'    '10.23456786'  -> '-3E-8'", c9hu);
//             TSub("subx134 subtract  '10.23456784'    '10.23456786'  -> '-2E-8'", c9hu);
//             TSub("subx135 subtract  '10.23456785'    '10.23456786'  -> '-1E-8'", c9hu);
//             TSub("subx136 subtract  '10.23456786'    '10.23456786'  -> '0E-8'", c9hu);
//             TSub("subx137 subtract  '10.23456787'    '10.23456786'  -> '1E-8'", c9hu);
//             TSub("subx138 subtract  '10.23456788'    '10.23456786'  -> '2E-8'", c9hu);
//             TSub("subx139 subtract  '10.23456789'    '10.23456786'  -> '3E-8'", c9hu);
//             TSub("subx140 subtract  '10.23456790'    '10.23456786'  -> '4E-8'", c9hu);
//             TSub("subx141 subtract  '10.23456791'    '10.23456786'  -> '5E-8'", c9hu);
//             TSub("subx142 subtract  '1'              '0.999999999'  -> '1E-9'", c9hu);
//             TSub("subx143 subtract  '0.999999999'    '1'            -> '-1E-9'", c9hu);
//             TSub("subx144 subtract  '-10.23456780'   '-10.23456786' -> '6E-8'", c9hu);
//             TSub("subx145 subtract  '-10.23456790'   '-10.23456786' -> '-4E-8'", c9hu);
//             TSub("subx146 subtract  '-10.23456791'   '-10.23456786' -> '-5E-8'", c9hu);

//             BigDecimal.Context c3hu = new BigDecimal.Context(3, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c12hu = new BigDecimal.Context(12, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c15hu = new BigDecimal.Context(15, BigDecimal.RoundingMode.HalfUp);

//             //precision: 3
//             TSub("subx150 subtract '12345678900000' '9999999999999'  -> 2.35E+12 Inexact Rounded", c3hu);
//             TSub("subx151 subtract '9999999999999'  '12345678900000' -> -2.35E+12 Inexact Rounded", c3hu);
//             //precision: 6
//             TSub("subx152 subtract '12345678900000' '9999999999999'  -> 2.34568E+12 Inexact Rounded", c6hu);
//             TSub("subx153 subtract '9999999999999'  '12345678900000' -> -2.34568E+12 Inexact Rounded", c6hu);
//             //precision: 9
//             TSub("subx154 subtract '12345678900000' '9999999999999'  -> 2.34567890E+12 Inexact Rounded", c9hu);
//             TSub("subx155 subtract '9999999999999'  '12345678900000' -> -2.34567890E+12 Inexact Rounded", c9hu);
//             //precision: 12
//             TSub("subx156 subtract '12345678900000' '9999999999999'  -> 2.34567890000E+12 Inexact Rounded", c12hu);
//             TSub("subx157 subtract '9999999999999'  '12345678900000' -> -2.34567890000E+12 Inexact Rounded", c12hu);
//             //precision: 15
//             TSub("subx158 subtract '12345678900000' '9999999999999'  -> 2345678900001", c15hu);
//             TSub("subx159 subtract '9999999999999'  '12345678900000' -> -2345678900001", c15hu);
//             //precision: 9

//             //-- additional scaled arithmetic tests [0.97 problem]
//             TSub("subx160 subtract '0'     '.1'      -> '-0.1'", c9hu);
//             TSub("subx161 subtract '00'    '.97983'  -> '-0.97983'", c9hu);
//             TSub("subx162 subtract '0'     '.9'      -> '-0.9'", c9hu);
//             TSub("subx163 subtract '0'     '0.102'   -> '-0.102'", c9hu);
//             TSub("subx164 subtract '0'     '.4'      -> '-0.4'", c9hu);
//             TSub("subx165 subtract '0'     '.307'    -> '-0.307'", c9hu);
//             TSub("subx166 subtract '0'     '.43822'  -> '-0.43822'", c9hu);
//             TSub("subx167 subtract '0'     '.911'    -> '-0.911'", c9hu);
//             TSub("subx168 subtract '.0'    '.02'     -> '-0.02'", c9hu);
//             TSub("subx169 subtract '00'    '.392'    -> '-0.392'", c9hu);
//             TSub("subx170 subtract '0'     '.26'     -> '-0.26'", c9hu);
//             TSub("subx171 subtract '0'     '0.51'    -> '-0.51'", c9hu);
//             TSub("subx172 subtract '0'     '.2234'   -> '-0.2234'", c9hu);
//             TSub("subx173 subtract '0'     '.2'      -> '-0.2'", c9hu);
//             TSub("subx174 subtract '.0'    '.0008'   -> '-0.0008'", c9hu);
//             //-- 0. on left
//             TSub("subx180 subtract '0.0'     '-.1'      -> '0.1'", c9hu);
//             TSub("subx181 subtract '0.00'    '-.97983'  -> '0.97983'", c9hu);
//             TSub("subx182 subtract '0.0'     '-.9'      -> '0.9'", c9hu);
//             TSub("subx183 subtract '0.0'     '-0.102'   -> '0.102'", c9hu);
//             TSub("subx184 subtract '0.0'     '-.4'      -> '0.4'", c9hu);
//             TSub("subx185 subtract '0.0'     '-.307'    -> '0.307'", c9hu);
//             TSub("subx186 subtract '0.0'     '-.43822'  -> '0.43822'", c9hu);
//             TSub("subx187 subtract '0.0'     '-.911'    -> '0.911'", c9hu);
//             TSub("subx188 subtract '0.0'     '-.02'     -> '0.02'", c9hu);
//             TSub("subx189 subtract '0.00'    '-.392'    -> '0.392'", c9hu);
//             TSub("subx190 subtract '0.0'     '-.26'     -> '0.26'", c9hu);
//             TSub("subx191 subtract '0.0'     '-0.51'    -> '0.51'", c9hu);
//             TSub("subx192 subtract '0.0'     '-.2234'   -> '0.2234'", c9hu);
//             TSub("subx193 subtract '0.0'     '-.2'      -> '0.2'", c9hu);
//             TSub("subx194 subtract '0.0'     '-.0008'   -> '0.0008'", c9hu);
//             //-- negatives of same
//             TSub("subx200 subtract '0'     '-.1'      -> '0.1'", c9hu);
//             TSub("subx201 subtract '00'    '-.97983'  -> '0.97983'", c9hu);
//             TSub("subx202 subtract '0'     '-.9'      -> '0.9'", c9hu);
//             TSub("subx203 subtract '0'     '-0.102'   -> '0.102'", c9hu);
//             TSub("subx204 subtract '0'     '-.4'      -> '0.4'", c9hu);
//             TSub("subx205 subtract '0'     '-.307'    -> '0.307'", c9hu);
//             TSub("subx206 subtract '0'     '-.43822'  -> '0.43822'", c9hu);
//             TSub("subx207 subtract '0'     '-.911'    -> '0.911'", c9hu);
//             TSub("subx208 subtract '.0'    '-.02'     -> '0.02'", c9hu);
//             TSub("subx209 subtract '00'    '-.392'    -> '0.392'", c9hu);
//             TSub("subx210 subtract '0'     '-.26'     -> '0.26'", c9hu);
//             TSub("subx211 subtract '0'     '-0.51'    -> '0.51'", c9hu);
//             TSub("subx212 subtract '0'     '-.2234'   -> '0.2234'", c9hu);
//             TSub("subx213 subtract '0'     '-.2'      -> '0.2'", c9hu);
//             TSub("subx214 subtract '.0'    '-.0008'   -> '0.0008'", c9hu);

//             //-- more fixed, LHS swaps [really the same as testcases under add]
//             TSub("subx220 subtract '-56267E-12' 0  -> '-5.6267E-8'", c9hu);
//             TSub("subx221 subtract '-56267E-11' 0  -> '-5.6267E-7'", c9hu);
//             TSub("subx222 subtract '-56267E-10' 0  -> '-0.0000056267'", c9hu);
//             TSub("subx223 subtract '-56267E-9'  0  -> '-0.000056267'", c9hu);
//             TSub("subx224 subtract '-56267E-8'  0  -> '-0.00056267'", c9hu);
//             TSub("subx225 subtract '-56267E-7'  0  -> '-0.0056267'", c9hu);
//             TSub("subx226 subtract '-56267E-6'  0  -> '-0.056267'", c9hu);
//             TSub("subx227 subtract '-56267E-5'  0  -> '-0.56267'", c9hu);
//             TSub("subx228 subtract '-56267E-2'  0  -> '-562.67'", c9hu);
//             TSub("subx229 subtract '-56267E-1'  0  -> '-5626.7'", c9hu);
//             TSub("subx230 subtract '-56267E-0'  0  -> '-56267'", c9hu);
//             //-- symmetry ...
//             TSub("subx240 subtract 0 '-56267E-12'  -> '5.6267E-8'", c9hu);
//             TSub("subx241 subtract 0 '-56267E-11'  -> '5.6267E-7'", c9hu);
//             TSub("subx242 subtract 0 '-56267E-10'  -> '0.0000056267'", c9hu);
//             TSub("subx243 subtract 0 '-56267E-9'   -> '0.000056267'", c9hu);
//             TSub("subx244 subtract 0 '-56267E-8'   -> '0.00056267'", c9hu);
//             TSub("subx245 subtract 0 '-56267E-7'   -> '0.0056267'", c9hu);
//             TSub("subx246 subtract 0 '-56267E-6'   -> '0.056267'", c9hu);
//             TSub("subx247 subtract 0 '-56267E-5'   -> '0.56267'", c9hu);
//             TSub("subx248 subtract 0 '-56267E-2'   -> '562.67'", c9hu);
//             TSub("subx249 subtract 0 '-56267E-1'   -> '5626.7'", c9hu);
//             TSub("subx250 subtract 0 '-56267E-0'   -> '56267'", c9hu);

//             //-- now some more from the 'new' add
//             //precision: 9
//             TSub("subx301 subtract '1.23456789'  '1.00000000' -> '0.23456789'", c9hu);
//             TSub("subx302 subtract '1.23456789'  '1.00000011' -> '0.23456778'", c9hu);

//             TSub("subx311 subtract '0.4444444444'  '0.5555555555' -> '-0.111111111' Inexact Rounded", c9hu);
//             TSub("subx312 subtract '0.4444444440'  '0.5555555555' -> '-0.111111112' Inexact Rounded", c9hu);
//             TSub("subx313 subtract '0.4444444444'  '0.5555555550' -> '-0.111111111' Inexact Rounded", c9hu);
//             TSub("subx314 subtract '0.44444444449'    '0' -> '0.444444444' Inexact Rounded", c9hu);
//             TSub("subx315 subtract '0.444444444499'   '0' -> '0.444444444' Inexact Rounded", c9hu);
//             TSub("subx316 subtract '0.4444444444999'  '0' -> '0.444444444' Inexact Rounded", c9hu);
//             TSub("subx317 subtract '0.4444444445000'  '0' -> '0.444444445' Inexact Rounded", c9hu);
//             TSub("subx318 subtract '0.4444444445001'  '0' -> '0.444444445' Inexact Rounded", c9hu);
//             TSub("subx319 subtract '0.444444444501'   '0' -> '0.444444445' Inexact Rounded", c9hu);
//             TSub("subx320 subtract '0.44444444451'    '0' -> '0.444444445' Inexact Rounded", c9hu);

//             //-- some carrying effects
//             TSub("subx321 subtract '0.9998'  '0.0000' -> '0.9998'", c9hu);
//             TSub("subx322 subtract '0.9998'  '0.0001' -> '0.9997'", c9hu);
//             TSub("subx323 subtract '0.9998'  '0.0002' -> '0.9996'", c9hu);
//             TSub("subx324 subtract '0.9998'  '0.0003' -> '0.9995'", c9hu);
//             TSub("subx325 subtract '0.9998'  '-0.0000' -> '0.9998'", c9hu);
//             TSub("subx326 subtract '0.9998'  '-0.0001' -> '0.9999'", c9hu);
//             TSub("subx327 subtract '0.9998'  '-0.0002' -> '1.0000'", c9hu);
//             TSub("subx328 subtract '0.9998'  '-0.0003' -> '1.0001'", c9hu);

//             TSub("subx330 subtract '70'  '10000e+9' -> '-1.00000000E+13' Inexact Rounded", c9hu);
//             TSub("subx331 subtract '700'  '10000e+9' -> '-1.00000000E+13' Inexact Rounded", c9hu);
//             TSub("subx332 subtract '7000'  '10000e+9' -> '-9.99999999E+12' Inexact Rounded", c9hu);
//             TSub("subx333 subtract '70000'  '10000e+9' -> '-9.99999993E+12' Rounded", c9hu);
//             TSub("subx334 subtract '700000'  '10000e+9' -> '-9.99999930E+12' Rounded", c9hu);
//             TSub("subx335 subtract '7000000'  '10000e+9' -> '-9.99999300E+12' Rounded", c9hu);
//             //-- symmetry:
//             TSub("subx340 subtract '10000e+9'  '70' -> '1.00000000E+13' Inexact Rounded", c9hu);
//             TSub("subx341 subtract '10000e+9'  '700' -> '1.00000000E+13' Inexact Rounded", c9hu);
//             TSub("subx342 subtract '10000e+9'  '7000' -> '9.99999999E+12' Inexact Rounded", c9hu);
//             TSub("subx343 subtract '10000e+9'  '70000' -> '9.99999993E+12' Rounded", c9hu);
//             TSub("subx344 subtract '10000e+9'  '700000' -> '9.99999930E+12' Rounded", c9hu);
//             TSub("subx345 subtract '10000e+9'  '7000000' -> '9.99999300E+12' Rounded", c9hu);

//             //-- same, higher precision
//             //precision: 15
//             TSub("subx346 subtract '10000e+9'  '7'   -> '9999999999993'", c15hu);
//             TSub("subx347 subtract '10000e+9'  '70'   -> '9999999999930'", c15hu);
//             TSub("subx348 subtract '10000e+9'  '700'   -> '9999999999300'", c15hu);
//             TSub("subx349 subtract '10000e+9'  '7000'   -> '9999999993000'", c15hu);
//             TSub("subx350 subtract '10000e+9'  '70000'   -> '9999999930000'", c15hu);
//             TSub("subx351 subtract '10000e+9'  '700000'   -> '9999999300000'", c15hu);
//             TSub("subx352 subtract '7' '10000e+9'   -> '-9999999999993'", c15hu);
//             TSub("subx353 subtract '70' '10000e+9'   -> '-9999999999930'", c15hu);
//             TSub("subx354 subtract '700' '10000e+9'   -> '-9999999999300'", c15hu);
//             TSub("subx355 subtract '7000' '10000e+9'   -> '-9999999993000'", c15hu);
//             TSub("subx356 subtract '70000' '10000e+9'   -> '-9999999930000'", c15hu);
//             TSub("subx357 subtract '700000' '10000e+9'   -> '-9999999300000'", c15hu);

//             //-- zero preservation
//             //precision: 6
//             TSub("subx360 subtract '10000e+9'  '70000' -> '1.00000E+13' Inexact Rounded", c6hu);
//             TSub("subx361 subtract 1 '0.0001' -> '0.9999'", c6hu);
//             TSub("subx362 subtract 1 '0.00001' -> '0.99999'", c6hu);
//             TSub("subx363 subtract 1 '0.000001' -> '0.999999'", c6hu);
//             TSub("subx364 subtract 1 '0.0000001' -> '1.00000' Inexact Rounded", c6hu);
//             TSub("subx365 subtract 1 '0.00000001' -> '1.00000' Inexact Rounded", c6hu);

//             //-- some funny zeros [in case of bad signum]
//             TSub("subx370 subtract 1  0  -> 1", c6hu);
//             TSub("subx371 subtract 1 0.  -> 1", c6hu);
//             TSub("subx372 subtract 1  .0 -> 1.0", c6hu);
//             TSub("subx373 subtract 1 0.0 -> 1.0", c6hu);
//             TSub("subx374 subtract  0  1 -> -1", c6hu);
//             TSub("subx375 subtract 0.  1 -> -1", c6hu);
//             TSub("subx376 subtract  .0 1 -> -1.0", c6hu);
//             TSub("subx377 subtract 0.0 1 -> -1.0", c6hu);

//             //precision: 9

//             //-- leading 0 digit before round
//             TSub("subx910 subtract -103519362 -51897955.3 -> -51621406.7", c9hu);
//             TSub("subx911 subtract 159579.444 89827.5229 -> 69751.9211", c9hu);

//             TSub("subx920 subtract 333.123456 33.1234566 -> 299.999999 Inexact Rounded", c9hu);
//             TSub("subx921 subtract 333.123456 33.1234565 -> 300.000000 Inexact Rounded", c9hu);
//             TSub("subx922 subtract 133.123456 33.1234565 ->  99.9999995", c9hu);
//             TSub("subx923 subtract 133.123456 33.1234564 ->  99.9999996", c9hu);
//             TSub("subx924 subtract 133.123456 33.1234540 -> 100.000002 Rounded", c9hu);
//             TSub("subx925 subtract 133.123456 43.1234560 ->  90.0000000", c9hu);
//             TSub("subx926 subtract 133.123456 43.1234561 ->  89.9999999", c9hu);
//             TSub("subx927 subtract 133.123456 43.1234566 ->  89.9999994", c9hu);
//             TSub("subx928 subtract 101.123456 91.1234566 ->   9.9999994", c9hu);
//             TSub("subx929 subtract 101.123456 99.1234566 ->   1.9999994", c9hu);

//             //-- more of the same; probe for cluster boundary problems

//             BigDecimal.Context c1hu = new BigDecimal.Context(1, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c2hu = new BigDecimal.Context(2, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c4hu = new BigDecimal.Context(4, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c5hu = new BigDecimal.Context(5, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c7hu = new BigDecimal.Context(7, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c8hu = new BigDecimal.Context(8, BigDecimal.RoundingMode.HalfUp);

//             //precision: 1
//             TSub("subx930 subtract  11 2           -> 9", c1hu);
//             //precision: 2
//             TSub("subx932 subtract 101 2           -> 99", c2hu);
//             //precision: 3
//             TSub("subx934 subtract 101 2.1         -> 98.9", c3hu);
//             TSub("subx935 subtract 101 92.01       ->  8.99", c3hu);
//             //precision: 4
//             TSub("subx936 subtract 101 2.01        -> 98.99", c4hu);
//             TSub("subx937 subtract 101 92.01       ->  8.99", c4hu);
//             TSub("subx938 subtract 101 92.006      ->  8.994", c4hu);
//             //precision: 5
//             TSub("subx939 subtract 101 2.001       -> 98.999", c5hu);
//             TSub("subx940 subtract 101 92.001      ->  8.999", c5hu);
//             TSub("subx941 subtract 101 92.0006     ->  8.9994", c5hu);
//             //precision: 6
//             TSub("subx942 subtract 101 2.0001      -> 98.9999", c6hu);
//             TSub("subx943 subtract 101 92.0001     ->  8.9999", c6hu);
//             TSub("subx944 subtract 101 92.00006    ->  8.99994", c6hu);
//             //precision: 7
//             TSub("subx945 subtract 101 2.00001     -> 98.99999", c7hu);
//             TSub("subx946 subtract 101 92.00001    ->  8.99999", c7hu);
//             TSub("subx947 subtract 101 92.000006   ->  8.999994", c7hu);
//             //precision: 8
//             TSub("subx948 subtract 101 2.000001    -> 98.999999", c8hu);
//             TSub("subx949 subtract 101 92.000001   ->  8.999999", c8hu);
//             TSub("subx950 subtract 101 92.0000006  ->  8.9999994", c8hu);
//             //precision: 9
//             TSub("subx951 subtract 101 2.0000001   -> 98.9999999", c9hu);
//             TSub("subx952 subtract 101 92.0000001  ->  8.9999999", c9hu);
//             TSub("subx953 subtract 101 92.00000006 ->  8.99999994", c9hu);

//             //precision: 9

//             //-- more LHS swaps [were fixed]
//             TSub("subx390 subtract '-56267E-10'   0 ->  '-0.0000056267'", c9hu);
//             TSub("subx391 subtract '-56267E-6'    0 ->  '-0.056267'", c9hu);
//             TSub("subx392 subtract '-56267E-5'    0 ->  '-0.56267'", c9hu);
//             TSub("subx393 subtract '-56267E-4'    0 ->  '-5.6267'", c9hu);
//             TSub("subx394 subtract '-56267E-3'    0 ->  '-56.267'", c9hu);
//             TSub("subx395 subtract '-56267E-2'    0 ->  '-562.67'", c9hu);
//             TSub("subx396 subtract '-56267E-1'    0 ->  '-5626.7'", c9hu);
//             TSub("subx397 subtract '-56267E-0'    0 ->  '-56267'", c9hu);
//             TSub("subx398 subtract '-5E-10'       0 ->  '-5E-10'", c9hu);
//             TSub("subx399 subtract '-5E-7'        0 ->  '-5E-7'", c9hu);
//             TSub("subx400 subtract '-5E-6'        0 ->  '-0.000005'", c9hu);
//             TSub("subx401 subtract '-5E-5'        0 ->  '-0.00005'", c9hu);
//             TSub("subx402 subtract '-5E-4'        0 ->  '-0.0005'", c9hu);
//             TSub("subx403 subtract '-5E-1'        0 ->  '-0.5'", c9hu);
//             TSub("subx404 subtract '-5E0'         0 ->  '-5'", c9hu);
//             TSub("subx405 subtract '-5E1'         0 ->  '-50'", c9hu);
//             TSub("subx406 subtract '-5E5'         0 ->  '-500000'", c9hu);
//             TSub("subx407 subtract '-5E8'         0 ->  '-500000000'", c9hu);
//             TSub("subx408 subtract '-5E9'         0 ->  '-5.00000000E+9'   Rounded", c9hu);
//             TSub("subx409 subtract '-5E10'        0 ->  '-5.00000000E+10'  Rounded", c9hu);
//             TSub("subx410 subtract '-5E11'        0 ->  '-5.00000000E+11'  Rounded", c9hu);
//             TSub("subx411 subtract '-5E100'       0 ->  '-5.00000000E+100' Rounded", c9hu);

//             //-- more RHS swaps [were fixed]
//             TSub("subx420 subtract 0  '-56267E-10' ->  '0.0000056267'", c9hu);
//             TSub("subx421 subtract 0  '-56267E-6'  ->  '0.056267'", c9hu);
//             TSub("subx422 subtract 0  '-56267E-5'  ->  '0.56267'", c9hu);
//             TSub("subx423 subtract 0  '-56267E-4'  ->  '5.6267'", c9hu);
//             TSub("subx424 subtract 0  '-56267E-3'  ->  '56.267'", c9hu);
//             TSub("subx425 subtract 0  '-56267E-2'  ->  '562.67'", c9hu);
//             TSub("subx426 subtract 0  '-56267E-1'  ->  '5626.7'", c9hu);
//             TSub("subx427 subtract 0  '-56267E-0'  ->  '56267'", c9hu);
//             TSub("subx428 subtract 0  '-5E-10'     ->  '5E-10'", c9hu);
//             TSub("subx429 subtract 0  '-5E-7'      ->  '5E-7'", c9hu);
//             TSub("subx430 subtract 0  '-5E-6'      ->  '0.000005'", c9hu);
//             TSub("subx431 subtract 0  '-5E-5'      ->  '0.00005'", c9hu);
//             TSub("subx432 subtract 0  '-5E-4'      ->  '0.0005'", c9hu);
//             TSub("subx433 subtract 0  '-5E-1'      ->  '0.5'", c9hu);
//             TSub("subx434 subtract 0  '-5E0'       ->  '5'", c9hu);
//             TSub("subx435 subtract 0  '-5E1'       ->  '50'", c9hu);
//             TSub("subx436 subtract 0  '-5E5'       ->  '500000'", c9hu);
//             TSub("subx437 subtract 0  '-5E8'       ->  '500000000'", c9hu);
//             TSub("subx438 subtract 0  '-5E9'       ->  '5.00000000E+9'    Rounded", c9hu);
//             TSub("subx439 subtract 0  '-5E10'      ->  '5.00000000E+10'   Rounded", c9hu);
//             TSub("subx440 subtract 0  '-5E11'      ->  '5.00000000E+11'   Rounded", c9hu);
//             TSub("subx441 subtract 0  '-5E100'     ->  '5.00000000E+100'  Rounded", c9hu);


//             //-- try borderline precision, with carries, etc.
//             //precision: 15
//             TSub("subx461 subtract '1E+12' '1'       -> '999999999999'", c15hu);
//             TSub("subx462 subtract '1E+12' '-1.11'   -> '1000000000001.11'", c15hu);
//             TSub("subx463 subtract '1.11'  '-1E+12'  -> '1000000000001.11'", c15hu);
//             TSub("subx464 subtract '-1'    '-1E+12'  -> '999999999999'", c15hu);
//             TSub("subx465 subtract '7E+12' '1'       -> '6999999999999'", c15hu);
//             TSub("subx466 subtract '7E+12' '-1.11'   -> '7000000000001.11'", c15hu);
//             TSub("subx467 subtract '1.11'  '-7E+12'  -> '7000000000001.11'", c15hu);
//             TSub("subx468 subtract '-1'    '-7E+12'  -> '6999999999999'", c15hu);

//             //--                 123456789012345       123456789012345      1 23456789012345
//             TSub("subx470 subtract '0.444444444444444'  '-0.555555555555563' -> '1.00000000000001' Inexact Rounded", c15hu);
//             TSub("subx471 subtract '0.444444444444444'  '-0.555555555555562' -> '1.00000000000001' Inexact Rounded", c15hu);
//             TSub("subx472 subtract '0.444444444444444'  '-0.555555555555561' -> '1.00000000000001' Inexact Rounded", c15hu);
//             TSub("subx473 subtract '0.444444444444444'  '-0.555555555555560' -> '1.00000000000000' Inexact Rounded", c15hu);
//             TSub("subx474 subtract '0.444444444444444'  '-0.555555555555559' -> '1.00000000000000' Inexact Rounded", c15hu);
//             TSub("subx475 subtract '0.444444444444444'  '-0.555555555555558' -> '1.00000000000000' Inexact Rounded", c15hu);
//             TSub("subx476 subtract '0.444444444444444'  '-0.555555555555557' -> '1.00000000000000' Inexact Rounded", c15hu);
//             TSub("subx477 subtract '0.444444444444444'  '-0.555555555555556' -> '1.00000000000000' Rounded", c15hu);
//             TSub("subx478 subtract '0.444444444444444'  '-0.555555555555555' -> '0.999999999999999'", c15hu);
//             TSub("subx479 subtract '0.444444444444444'  '-0.555555555555554' -> '0.999999999999998'", c15hu);
//             TSub("subx480 subtract '0.444444444444444'  '-0.555555555555553' -> '0.999999999999997'", c15hu);
//             TSub("subx481 subtract '0.444444444444444'  '-0.555555555555552' -> '0.999999999999996'", c15hu);
//             TSub("subx482 subtract '0.444444444444444'  '-0.555555555555551' -> '0.999999999999995'", c15hu);
//             TSub("subx483 subtract '0.444444444444444'  '-0.555555555555550' -> '0.999999999999994'", c15hu);


//             //-- and some more, including residue effects and different roundings
//             //precision: 9
//             //rounding: half_up
//             TSub("subx500 subtract '123456789' 0             -> '123456789'", c9hu);
//             TSub("subx501 subtract '123456789' 0.000000001   -> '123456789' Inexact Rounded", c9hu);
//             TSub("subx502 subtract '123456789' 0.000001      -> '123456789' Inexact Rounded", c9hu);
//             TSub("subx503 subtract '123456789' 0.1           -> '123456789' Inexact Rounded", c9hu);
//             TSub("subx504 subtract '123456789' 0.4           -> '123456789' Inexact Rounded", c9hu);
//             TSub("subx505 subtract '123456789' 0.49          -> '123456789' Inexact Rounded", c9hu);
//             TSub("subx506 subtract '123456789' 0.499999      -> '123456789' Inexact Rounded", c9hu);
//             TSub("subx507 subtract '123456789' 0.499999999   -> '123456789' Inexact Rounded", c9hu);
//             TSub("subx508 subtract '123456789' 0.5           -> '123456789' Inexact Rounded", c9hu);
//             TSub("subx509 subtract '123456789' 0.500000001   -> '123456788' Inexact Rounded", c9hu);
//             TSub("subx510 subtract '123456789' 0.500001      -> '123456788' Inexact Rounded", c9hu);
//             TSub("subx511 subtract '123456789' 0.51          -> '123456788' Inexact Rounded", c9hu);
//             TSub("subx512 subtract '123456789' 0.6           -> '123456788' Inexact Rounded", c9hu);
//             TSub("subx513 subtract '123456789' 0.9           -> '123456788' Inexact Rounded", c9hu);
//             TSub("subx514 subtract '123456789' 0.99999       -> '123456788' Inexact Rounded", c9hu);
//             TSub("subx515 subtract '123456789' 0.999999999   -> '123456788' Inexact Rounded", c9hu);
//             TSub("subx516 subtract '123456789' 1             -> '123456788'", c9hu);
//             TSub("subx517 subtract '123456789' 1.000000001   -> '123456788' Inexact Rounded", c9hu);
//             TSub("subx518 subtract '123456789' 1.00001       -> '123456788' Inexact Rounded", c9hu);
//             TSub("subx519 subtract '123456789' 1.1           -> '123456788' Inexact Rounded", c9hu);

//             //rounding: half_even
//             BigDecimal.Context c9he = new BigDecimal.Context(9, BigDecimal.RoundingMode.HalfEven);
//             TSub("subx520 subtract '123456789' 0             -> '123456789'", c9he);
//             TSub("subx521 subtract '123456789' 0.000000001   -> '123456789' Inexact Rounded", c9he);
//             TSub("subx522 subtract '123456789' 0.000001      -> '123456789' Inexact Rounded", c9he);
//             TSub("subx523 subtract '123456789' 0.1           -> '123456789' Inexact Rounded", c9he);
//             TSub("subx524 subtract '123456789' 0.4           -> '123456789' Inexact Rounded", c9he);
//             TSub("subx525 subtract '123456789' 0.49          -> '123456789' Inexact Rounded", c9he);
//             TSub("subx526 subtract '123456789' 0.499999      -> '123456789' Inexact Rounded", c9he);
//             TSub("subx527 subtract '123456789' 0.499999999   -> '123456789' Inexact Rounded", c9he);
//             TSub("subx528 subtract '123456789' 0.5           -> '123456788' Inexact Rounded", c9he);
//             TSub("subx529 subtract '123456789' 0.500000001   -> '123456788' Inexact Rounded", c9he);
//             TSub("subx530 subtract '123456789' 0.500001      -> '123456788' Inexact Rounded", c9he);
//             TSub("subx531 subtract '123456789' 0.51          -> '123456788' Inexact Rounded", c9he);
//             TSub("subx532 subtract '123456789' 0.6           -> '123456788' Inexact Rounded", c9he);
//             TSub("subx533 subtract '123456789' 0.9           -> '123456788' Inexact Rounded", c9he);
//             TSub("subx534 subtract '123456789' 0.99999       -> '123456788' Inexact Rounded", c9he);
//             TSub("subx535 subtract '123456789' 0.999999999   -> '123456788' Inexact Rounded", c9he);
//             TSub("subx536 subtract '123456789' 1             -> '123456788'", c9he);
//             TSub("subx537 subtract '123456789' 1.00000001    -> '123456788' Inexact Rounded", c9he);
//             TSub("subx538 subtract '123456789' 1.00001       -> '123456788' Inexact Rounded", c9he);
//             TSub("subx539 subtract '123456789' 1.1           -> '123456788' Inexact Rounded", c9he);
//             //-- critical few with even bottom digit...
//             TSub("subx540 subtract '123456788' 0.499999999   -> '123456788' Inexact Rounded", c9he);
//             TSub("subx541 subtract '123456788' 0.5           -> '123456788' Inexact Rounded", c9he);
//             TSub("subx542 subtract '123456788' 0.500000001   -> '123456787' Inexact Rounded", c9he);

//             //rounding: down
//             BigDecimal.Context c9d = new BigDecimal.Context(9, BigDecimal.RoundingMode.Down);

//             TSub("subx550 subtract '123456789' 0             -> '123456789'", c9d);
//             TSub("subx551 subtract '123456789' 0.000000001   -> '123456788' Inexact Rounded", c9d);
//             TSub("subx552 subtract '123456789' 0.000001      -> '123456788' Inexact Rounded", c9d);
//             TSub("subx553 subtract '123456789' 0.1           -> '123456788' Inexact Rounded", c9d);
//             TSub("subx554 subtract '123456789' 0.4           -> '123456788' Inexact Rounded", c9d);
//             TSub("subx555 subtract '123456789' 0.49          -> '123456788' Inexact Rounded", c9d);
//             TSub("subx556 subtract '123456789' 0.499999      -> '123456788' Inexact Rounded", c9d);
//             TSub("subx557 subtract '123456789' 0.499999999   -> '123456788' Inexact Rounded", c9d);
//             TSub("subx558 subtract '123456789' 0.5           -> '123456788' Inexact Rounded", c9d);
//             TSub("subx559 subtract '123456789' 0.500000001   -> '123456788' Inexact Rounded", c9d);
//             TSub("subx560 subtract '123456789' 0.500001      -> '123456788' Inexact Rounded", c9d);
//             TSub("subx561 subtract '123456789' 0.51          -> '123456788' Inexact Rounded", c9d);
//             TSub("subx562 subtract '123456789' 0.6           -> '123456788' Inexact Rounded", c9d);
//             TSub("subx563 subtract '123456789' 0.9           -> '123456788' Inexact Rounded", c9d);
//             TSub("subx564 subtract '123456789' 0.99999       -> '123456788' Inexact Rounded", c9d);
//             TSub("subx565 subtract '123456789' 0.999999999   -> '123456788' Inexact Rounded", c9d);
//             TSub("subx566 subtract '123456789' 1             -> '123456788'", c9d);
//             TSub("subx567 subtract '123456789' 1.00000001    -> '123456787' Inexact Rounded", c9d);
//             TSub("subx568 subtract '123456789' 1.00001       -> '123456787' Inexact Rounded", c9d);
//             TSub("subx569 subtract '123456789' 1.1           -> '123456787' Inexact Rounded", c9d);

//             //-- symmetry...
//             //rounding: half_up
//             TSub("subx600 subtract 0             '123456789' -> '-123456789'", c9hu);
//             TSub("subx601 subtract 0.000000001   '123456789' -> '-123456789' Inexact Rounded", c9hu);
//             TSub("subx602 subtract 0.000001      '123456789' -> '-123456789' Inexact Rounded", c9hu);
//             TSub("subx603 subtract 0.1           '123456789' -> '-123456789' Inexact Rounded", c9hu);
//             TSub("subx604 subtract 0.4           '123456789' -> '-123456789' Inexact Rounded", c9hu);
//             TSub("subx605 subtract 0.49          '123456789' -> '-123456789' Inexact Rounded", c9hu);
//             TSub("subx606 subtract 0.499999      '123456789' -> '-123456789' Inexact Rounded", c9hu);
//             TSub("subx607 subtract 0.499999999   '123456789' -> '-123456789' Inexact Rounded", c9hu);
//             TSub("subx608 subtract 0.5           '123456789' -> '-123456789' Inexact Rounded", c9hu);
//             TSub("subx609 subtract 0.500000001   '123456789' -> '-123456788' Inexact Rounded", c9hu);
//             TSub("subx610 subtract 0.500001      '123456789' -> '-123456788' Inexact Rounded", c9hu);
//             TSub("subx611 subtract 0.51          '123456789' -> '-123456788' Inexact Rounded", c9hu);
//             TSub("subx612 subtract 0.6           '123456789' -> '-123456788' Inexact Rounded", c9hu);
//             TSub("subx613 subtract 0.9           '123456789' -> '-123456788' Inexact Rounded", c9hu);
//             TSub("subx614 subtract 0.99999       '123456789' -> '-123456788' Inexact Rounded", c9hu);
//             TSub("subx615 subtract 0.999999999   '123456789' -> '-123456788' Inexact Rounded", c9hu);
//             TSub("subx616 subtract 1             '123456789' -> '-123456788'", c9hu);
//             TSub("subx617 subtract 1.000000001   '123456789' -> '-123456788' Inexact Rounded", c9hu);
//             TSub("subx618 subtract 1.00001       '123456789' -> '-123456788' Inexact Rounded", c9hu);
//             TSub("subx619 subtract 1.1           '123456789' -> '-123456788' Inexact Rounded", c9hu);

//             //rounding: half_even
//             TSub("subx620 subtract 0             '123456789' -> '-123456789'", c9he);
//             TSub("subx621 subtract 0.000000001   '123456789' -> '-123456789' Inexact Rounded", c9he);
//             TSub("subx622 subtract 0.000001      '123456789' -> '-123456789' Inexact Rounded", c9he);
//             TSub("subx623 subtract 0.1           '123456789' -> '-123456789' Inexact Rounded", c9he);
//             TSub("subx624 subtract 0.4           '123456789' -> '-123456789' Inexact Rounded", c9he);
//             TSub("subx625 subtract 0.49          '123456789' -> '-123456789' Inexact Rounded", c9he);
//             TSub("subx626 subtract 0.499999      '123456789' -> '-123456789' Inexact Rounded", c9he);
//             TSub("subx627 subtract 0.499999999   '123456789' -> '-123456789' Inexact Rounded", c9he);
//             TSub("subx628 subtract 0.5           '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx629 subtract 0.500000001   '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx630 subtract 0.500001      '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx631 subtract 0.51          '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx632 subtract 0.6           '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx633 subtract 0.9           '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx634 subtract 0.99999       '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx635 subtract 0.999999999   '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx636 subtract 1             '123456789' -> '-123456788'", c9he);
//             TSub("subx637 subtract 1.00000001    '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx638 subtract 1.00001       '123456789' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx639 subtract 1.1           '123456789' -> '-123456788' Inexact Rounded", c9he);
//             //-- critical few with even bottom digit...
//             TSub("subx640 subtract 0.499999999   '123456788' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx641 subtract 0.5           '123456788' -> '-123456788' Inexact Rounded", c9he);
//             TSub("subx642 subtract 0.500000001   '123456788' -> '-123456787' Inexact Rounded", c9he);

//             //rounding: down
//             TSub("subx650 subtract 0             '123456789' -> '-123456789'", c9d);
//             TSub("subx651 subtract 0.000000001   '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx652 subtract 0.000001      '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx653 subtract 0.1           '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx654 subtract 0.4           '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx655 subtract 0.49          '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx656 subtract 0.499999      '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx657 subtract 0.499999999   '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx658 subtract 0.5           '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx659 subtract 0.500000001   '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx660 subtract 0.500001      '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx661 subtract 0.51          '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx662 subtract 0.6           '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx663 subtract 0.9           '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx664 subtract 0.99999       '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx665 subtract 0.999999999   '123456789' -> '-123456788' Inexact Rounded", c9d);
//             TSub("subx666 subtract 1             '123456789' -> '-123456788'", c9d);
//             TSub("subx667 subtract 1.00000001    '123456789' -> '-123456787' Inexact Rounded", c9d);
//             TSub("subx668 subtract 1.00001       '123456789' -> '-123456787' Inexact Rounded", c9d);
//             TSub("subx669 subtract 1.1           '123456789' -> '-123456787' Inexact Rounded", c9d);


//             //-- lots of leading zeros in intermediate result, and showing effects of
//             //-- input rounding would have affected the following
//             //precision: 9
//             //rounding: half_up
//             TSub("subx670 subtract '123456789' '123456788.1' -> 0.9", c9hu);
//             TSub("subx671 subtract '123456789' '123456788.9' -> 0.1", c9hu);
//             TSub("subx672 subtract '123456789' '123456789.1' -> -0.1", c9hu);
//             TSub("subx673 subtract '123456789' '123456789.5' -> -0.5", c9hu);
//             TSub("subx674 subtract '123456789' '123456789.9' -> -0.9", c9hu);

//             //rounding: half_even
//             TSub("subx680 subtract '123456789' '123456788.1' -> 0.9", c9he);
//             TSub("subx681 subtract '123456789' '123456788.9' -> 0.1", c9he);
//             TSub("subx682 subtract '123456789' '123456789.1' -> -0.1", c9he);
//             TSub("subx683 subtract '123456789' '123456789.5' -> -0.5", c9he);
//             TSub("subx684 subtract '123456789' '123456789.9' -> -0.9", c9he);

//             TSub("subx685 subtract '123456788' '123456787.1' -> 0.9", c9he);
//             TSub("subx686 subtract '123456788' '123456787.9' -> 0.1", c9he);
//             TSub("subx687 subtract '123456788' '123456788.1' -> -0.1", c9he);
//             TSub("subx688 subtract '123456788' '123456788.5' -> -0.5", c9he);
//             TSub("subx689 subtract '123456788' '123456788.9' -> -0.9", c9he);

//             //rounding: down
//             TSub("subx690 subtract '123456789' '123456788.1' -> 0.9", c9d);
//             TSub("subx691 subtract '123456789' '123456788.9' -> 0.1", c9d);
//             TSub("subx692 subtract '123456789' '123456789.1' -> -0.1", c9d);
//             TSub("subx693 subtract '123456789' '123456789.5' -> -0.5", c9d);
//             TSub("subx694 subtract '123456789' '123456789.9' -> -0.9", c9d);

//             //-- input preparation tests
//             //rounding: half_up
//             //precision: 3

//             TSub("subx700 subtract '12345678900000'  -9999999999999 ->  '2.23E+13' Inexact Rounded", c3hu);
//             TSub("subx701 subtract  '9999999999999' -12345678900000 ->  '2.23E+13' Inexact Rounded", c3hu);
//             TSub("subx702 subtract '12E+3'  '-3456' ->  '1.55E+4' Inexact Rounded", c3hu);
//             TSub("subx703 subtract '12E+3'  '-3446' ->  '1.54E+4' Inexact Rounded", c3hu);
//             TSub("subx704 subtract '12E+3'  '-3454' ->  '1.55E+4' Inexact Rounded", c3hu);
//             TSub("subx705 subtract '12E+3'  '-3444' ->  '1.54E+4' Inexact Rounded", c3hu);

//             TSub("subx706 subtract '3456'  '-12E+3' ->  '1.55E+4' Inexact Rounded", c3hu);
//             TSub("subx707 subtract '3446'  '-12E+3' ->  '1.54E+4' Inexact Rounded", c3hu);
//             TSub("subx708 subtract '3454'  '-12E+3' ->  '1.55E+4' Inexact Rounded", c3hu);
//             TSub("subx709 subtract '3444'  '-12E+3' ->  '1.54E+4' Inexact Rounded", c3hu);

//             //-- overflow and underflow tests [subnormals now possible]
//             //maxexponent: 999999999
//             //minexponent: -999999999
//             //precision: 9
//             //rounding: down
//             //TSub("subx710 subtract 1E+999999999    -9E+999999999   -> 9.99999999E+999999999 Overflow Inexact Rounded", c9d);
//             //TSub("subx711 subtract 9E+999999999    -1E+999999999   -> 9.99999999E+999999999 Overflow Inexact Rounded", c9d);
//             //rounding: half_up
//             //TSub("subx712 subtract 1E+999999999    -9E+999999999   -> Infinity Overflow Inexact Rounded", c9hu);
//             //TSub("subx713 subtract 9E+999999999    -1E+999999999   -> Infinity Overflow Inexact Rounded", c9hu);
//             //TSub("subx714 subtract -1.1E-999999999 -1E-999999999   -> -1E-1000000000 Subnormal", c9hu);
//             //TSub("subx715 subtract 1E-999999999    +1.1e-999999999 -> -1E-1000000000 Subnormal", c9hu);
//             //TSub("subx716 subtract -1E+999999999   +9E+999999999   -> -Infinity Overflow Inexact Rounded", c9hu);
//             //TSub("subx717 subtract -9E+999999999   +1E+999999999   -> -Infinity Overflow Inexact Rounded", c9hu);
//             //TSub("subx718 subtract +1.1E-999999999 +1E-999999999   -> 1E-1000000000 Subnormal", c9hu);
//             //TSub("subx719 subtract -1E-999999999   -1.1e-999999999 -> 1E-1000000000 Subnormal", c9hu);

//             //precision: 3
//             //TSub("subx720 subtract 1  9.999E+999999999   -> -Infinity Inexact Overflow Rounded", c3hu);
//             //TSub("subx721 subtract 1 -9.999E+999999999   ->  Infinity Inexact Overflow Rounded", c3hu);
//             //TSub("subx722 subtract    9.999E+999999999 1 ->  Infinity Inexact Overflow Rounded", c3hu);
//             //TSub("subx723 subtract   -9.999E+999999999 1 -> -Infinity Inexact Overflow Rounded", c3hu);
//             //TSub("subx724 subtract 1  9.999E+999999999   -> -Infinity Inexact Overflow Rounded", c3hu);
//             //TSub("subx725 subtract 1 -9.999E+999999999   ->  Infinity Inexact Overflow Rounded", c3hu);
//             //TSub("subx726 subtract    9.999E+999999999 1 ->  Infinity Inexact Overflow Rounded", c3hu);
//             //TSub("subx727 subtract   -9.999E+999999999 1 -> -Infinity Inexact Overflow Rounded", c3hu);

//             //-- [more below]

//             //-- long operand checks
//             //maxexponent: 999
//             //minexponent: -999
//             //precision: 9
//             TSub("sub731 subtract 12345678000 0 ->  1.23456780E+10 Rounded", c9hu);
//             TSub("sub732 subtract 0 12345678000 -> -1.23456780E+10 Rounded", c9hu);
//             TSub("sub733 subtract 1234567800  0 ->  1.23456780E+9 Rounded", c9hu);
//             TSub("sub734 subtract 0 1234567800  -> -1.23456780E+9 Rounded", c9hu);
//             TSub("sub735 subtract 1234567890  0 ->  1.23456789E+9 Rounded", c9hu);
//             TSub("sub736 subtract 0 1234567890  -> -1.23456789E+9 Rounded", c9hu);
//             TSub("sub737 subtract 1234567891  0 ->  1.23456789E+9 Inexact Rounded", c9hu);
//             TSub("sub738 subtract 0 1234567891  -> -1.23456789E+9 Inexact Rounded", c9hu);
//             TSub("sub739 subtract 12345678901 0 ->  1.23456789E+10 Inexact Rounded", c9hu);
//             TSub("sub740 subtract 0 12345678901 -> -1.23456789E+10 Inexact Rounded", c9hu);
//             TSub("sub741 subtract 1234567896  0 ->  1.23456790E+9 Inexact Rounded", c9hu);
//             TSub("sub742 subtract 0 1234567896  -> -1.23456790E+9 Inexact Rounded", c9hu);

//             //precision: 15
//             TSub("sub751 subtract 12345678000 0 ->  12345678000", c15hu);
//             TSub("sub752 subtract 0 12345678000 -> -12345678000", c15hu);
//             TSub("sub753 subtract 1234567800  0 ->  1234567800", c15hu);
//             TSub("sub754 subtract 0 1234567800  -> -1234567800", c15hu);
//             TSub("sub755 subtract 1234567890  0 ->  1234567890", c15hu);
//             TSub("sub756 subtract 0 1234567890  -> -1234567890", c15hu);
//             TSub("sub757 subtract 1234567891  0 ->  1234567891", c15hu);
//             TSub("sub758 subtract 0 1234567891  -> -1234567891", c15hu);
//             TSub("sub759 subtract 12345678901 0 ->  12345678901", c15hu);
//             TSub("sub760 subtract 0 12345678901 -> -12345678901", c15hu);
//             TSub("sub761 subtract 1234567896  0 ->  1234567896", c15hu);
//             TSub("sub762 subtract 0 1234567896  -> -1234567896", c15hu);

//             //-- Specials
//             //TSub("subx780 subtract -Inf   Inf   -> -Infinity
//             //TSub("subx781 subtract -Inf   1000  -> -Infinity
//             //TSub("subx782 subtract -Inf   1     -> -Infinity
//             //TSub("subx783 subtract -Inf  -0     -> -Infinity
//             //TSub("subx784 subtract -Inf  -1     -> -Infinity
//             //TSub("subx785 subtract -Inf  -1000  -> -Infinity
//             //TSub("subx787 subtract -1000  Inf   -> -Infinity
//             //TSub("subx788 subtract -Inf   Inf   -> -Infinity
//             //TSub("subx789 subtract -1     Inf   -> -Infinity
//             //TSub("subx790 subtract  0     Inf   -> -Infinity
//             //TSub("subx791 subtract  1     Inf   -> -Infinity
//             //TSub("subx792 subtract  1000  Inf   -> -Infinity

//             //TSub("subx800 subtract  Inf   Inf   ->  NaN  Invalid_operation
//             ///TSub("subx801 subtract  Inf   1000  ->  Infinity
//             //TSub("subx802 subtract  Inf   1     ->  Infinity
//             //TSub("subx803 subtract  Inf   0     ->  Infinity
//             //TSub("subx804 subtract  Inf  -0     ->  Infinity
//             //TSub("subx805 subtract  Inf  -1     ->  Infinity
//             //TSub("subx806 subtract  Inf  -1000  ->  Infinity
//             //TSub("subx807 subtract  Inf  -Inf   ->  Infinity
//             //TSub("subx808 subtract -1000 -Inf   ->  Infinity
//             //TSub("subx809 subtract -Inf  -Inf   ->  NaN  Invalid_operation
//             //TSub("subx810 subtract -1    -Inf   ->  Infinity
//             //TSub("subx811 subtract -0    -Inf   ->  Infinity
//             //TSub("subx812 subtract  0    -Inf   ->  Infinity
//             //TSub("subx813 subtract  1    -Inf   ->  Infinity
//             //TSub("subx814 subtract  1000 -Inf   ->  Infinity
//             //TSub("subx815 subtract  Inf  -Inf   ->  Infinity

//             //TSub("subx821 subtract  NaN   Inf   ->  NaN
//             //TSub("subx822 subtract -NaN   1000  -> -NaN
//             //TSub("subx823 subtract  NaN   1     ->  NaN
//             //TSub("subx824 subtract  NaN   0     ->  NaN
//             //TSub("subx825 subtract  NaN  -0     ->  NaN
//             //TSub("subx826 subtract  NaN  -1     ->  NaN
//             //TSub("subx827 subtract  NaN  -1000  ->  NaN
//             //TSub("subx828 subtract  NaN  -Inf   ->  NaN
//             //TSub("subx829 subtract -NaN   NaN   -> -NaN
//             //TSub("subx830 subtract -Inf   NaN   ->  NaN
//             //TSub("subx831 subtract -1000  NaN   ->  NaN
//             //TSub("subx832 subtract -1     NaN   ->  NaN
//             //TSub("subx833 subtract -0     NaN   ->  NaN
//             //TSub("subx834 subtract  0     NaN   ->  NaN
//             //TSub("subx835 subtract  1     NaN   ->  NaN
//             //TSub("subx836 subtract  1000 -NaN   -> -NaN
//             //TSub("subx837 subtract  Inf   NaN   ->  NaN

//             //TSub("subx841 subtract  sNaN  Inf   ->  NaN  Invalid_operation
//             //TSub("subx842 subtract -sNaN  1000  -> -NaN  Invalid_operation
//             //TSub("subx843 subtract  sNaN  1     ->  NaN  Invalid_operation
//             //TSub("subx844 subtract  sNaN  0     ->  NaN  Invalid_operation
//             //TSub("subx845 subtract  sNaN -0     ->  NaN  Invalid_operation
//             //TSub("subx846 subtract  sNaN -1     ->  NaN  Invalid_operation
//             //TSub("subx847 subtract  sNaN -1000  ->  NaN  Invalid_operation
//             //TSub("subx848 subtract  sNaN  NaN   ->  NaN  Invalid_operation
//             //TSub("subx849 subtract  sNaN sNaN   ->  NaN  Invalid_operation
//             //TSub("subx850 subtract  NaN  sNaN   ->  NaN  Invalid_operation
//             //TSub("subx851 subtract -Inf -sNaN   -> -NaN  Invalid_operation
//             //TSub("subx852 subtract -1000 sNaN   ->  NaN  Invalid_operation
//             //TSub("subx853 subtract -1    sNaN   ->  NaN  Invalid_operation
//             //TSub("subx854 subtract -0    sNaN   ->  NaN  Invalid_operation
//             //TSub("subx855 subtract  0    sNaN   ->  NaN  Invalid_operation
//             //TSub("subx856 subtract  1    sNaN   ->  NaN  Invalid_operation
//             //TSub("subx857 subtract  1000 sNaN   ->  NaN  Invalid_operation
//             //TSub("subx858 subtract  Inf  sNaN   ->  NaN  Invalid_operation
//             //TSub("subx859 subtract  NaN  sNaN   ->  NaN  Invalid_operation

//             //-- propagating NaNs
//             //TSub("subx861 subtract  NaN01   -Inf     ->  NaN1
//             //TSub("subx862 subtract -NaN02   -1000    -> -NaN2
//             //TSub("subx863 subtract  NaN03    1000    ->  NaN3
//             //TSub("subx864 subtract  NaN04    Inf     ->  NaN4
//             //TSub("subx865 subtract  NaN05    NaN61   ->  NaN5
//             //TSub("subx866 subtract -Inf     -NaN71   -> -NaN71
//             //TSub("subx867 subtract -1000     NaN81   ->  NaN81
//             //TSub("subx868 subtract  1000     NaN91   ->  NaN91
//             //TSub("subx869 subtract  Inf      NaN101  ->  NaN101
//             //TSub("subx871 subtract  sNaN011  -Inf    ->  NaN11  Invalid_operation
//             //TSub("subx872 subtract  sNaN012  -1000   ->  NaN12  Invalid_operation
//             //TSub("subx873 subtract -sNaN013   1000   -> -NaN13  Invalid_operation
//             //TSub("subx874 subtract  sNaN014   NaN171 ->  NaN14  Invalid_operation
//             //TSub("subx875 subtract  sNaN015  sNaN181 ->  NaN15  Invalid_operation
//             //TSub("subx876 subtract  NaN016   sNaN191 ->  NaN191 Invalid_operation
//             //TSub("subx877 subtract -Inf      sNaN201 ->  NaN201 Invalid_operation
//             //TSub("subx878 subtract -1000     sNaN211 ->  NaN211 Invalid_operation
//             //TSub("subx879 subtract  1000    -sNaN221 -> -NaN221 Invalid_operation
//             //TSub("subx880 subtract  Inf      sNaN231 ->  NaN231 Invalid_operation
//             //TSub("subx881 subtract  NaN025   sNaN241 ->  NaN241 Invalid_operation

//             //-- edge case spills
//             TSub("subx901 subtract  2.E-3  1.002  -> -1.000", c15hu);
//             TSub("subx902 subtract  2.0E-3  1.002  -> -1.0000", c15hu);
//             TSub("subx903 subtract  2.00E-3  1.0020  -> -1.00000", c15hu);
//             TSub("subx904 subtract  2.000E-3  1.00200  -> -1.000000", c15hu);
//             TSub("subx905 subtract  2.0000E-3  1.002000  -> -1.0000000", c15hu);
//             TSub("subx906 subtract  2.00000E-3  1.0020000  -> -1.00000000", c15hu);
//             TSub("subx907 subtract  2.000000E-3  1.00200000  -> -1.000000000", c15hu);
//             TSub("subx908 subtract  2.0000000E-3  1.002000000  -> -1.0000000000", c15hu);

//             //-- subnormals and underflows
//             //precision: 3
//             //maxexponent: 999
//             //minexponent: -999
//             TSub("subx1010 subtract  0  1.00E-999       ->  -1.00E-999", c3hu);
//             TSub("subx1011 subtract  0  0.1E-999        ->  -1E-1000   Subnormal", c3hu);
//             TSub("subx1012 subtract  0  0.10E-999       ->  -1.0E-1000 Subnormal", c3hu);
//             //?TSub("subx1013 subtract  0  0.100E-999      ->  -1.0E-1000 Subnormal Rounded", c3hu);
//             TSub("subx1014 subtract  0  0.01E-999       ->  -1E-1001   Subnormal", c3hu);
//             //-- next is rounded to Emin
//             //?TSub("subx1015 subtract  0  0.999E-999      ->  -1.00E-999 Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1016 subtract  0  0.099E-999      ->  -1.0E-1000 Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1017 subtract  0  0.009E-999      ->  -1E-1001   Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1018 subtract  0  0.001E-999      ->  -0E-1001   Inexact Rounded Subnormal Underflow Clamped", c3hu);
//             //?TSub("subx1019 subtract  0  0.0009E-999     ->  -0E-1001   Inexact Rounded Subnormal Underflow Clamped", c3hu);
//             //?TSub("subx1020 subtract  0  0.0001E-999     ->  -0E-1001   Inexact Rounded Subnormal Underflow Clamped", c3hu);

//             //?TSub("subx1030 subtract  0 -1.00E-999       ->   1.00E-999", c3hu);
//             //?TSub("subx1031 subtract  0 -0.1E-999        ->   1E-1000   Subnormal", c3hu);
//             //?TSub("subx1032 subtract  0 -0.10E-999       ->   1.0E-1000 Subnormal", c3hu);
//             //?TSub("subx1033 subtract  0 -0.100E-999      ->   1.0E-1000 Subnormal Rounded", c3hu);
//             //?TSub("subx1034 subtract  0 -0.01E-999       ->   1E-1001   Subnormal", c3hu);
//             //-- next is rounded to Emin
//             //?TSub("subx1035 subtract  0 -0.999E-999      ->   1.00E-999 Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1036 subtract  0 -0.099E-999      ->   1.0E-1000 Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1037 subtract  0 -0.009E-999      ->   1E-1001   Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1038 subtract  0 -0.001E-999      ->   0E-1001   Inexact Rounded Subnormal Underflow Clamped", c3hu);
//             //?TSub("subx1039 subtract  0 -0.0009E-999     ->   0E-1001   Inexact Rounded Subnormal Underflow Clamped", c3hu);
//             //?TSub("subx1040 subtract  0 -0.0001E-999     ->   0E-1001   Inexact Rounded Subnormal Underflow Clamped", c3hu);

//             //-- some non-zero subnormal subtracts
//             //-- TSub("subx1056 is a tricky case
//             //rounding: half_up
//             //?TSub("subx1050 subtract  1.00E-999   0.1E-999  ->   9.0E-1000  Subnormal", c3hu);
//             //?TSub("subx1051 subtract  0.1E-999    0.1E-999  ->   0E-1000", c3hu);
//             //?TSub("subx1052 subtract  0.10E-999   0.1E-999  ->   0E-1001", c3hu);
//             //?TSub("subx1053 subtract  0.100E-999  0.1E-999  ->   0E-1001    Clamped", c3hu);
//             //?TSub("subx1054 subtract  0.01E-999   0.1E-999  ->   -9E-1001   Subnormal", c3hu);
//             //?TSub("subx1055 subtract  0.999E-999  0.1E-999  ->   9.0E-1000  Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1056 subtract  0.099E-999  0.1E-999  ->   -0E-1001   Inexact Rounded Subnormal Underflow Clamped", c3hu);
//             //?TSub("subx1057 subtract  0.009E-999  0.1E-999  ->   -9E-1001   Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1058 subtract  0.001E-999  0.1E-999  ->   -1.0E-1000 Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1059 subtract  0.0009E-999 0.1E-999  ->   -1.0E-1000 Inexact Rounded Subnormal Underflow", c3hu);
//             //?TSub("subx1060 subtract  0.0001E-999 0.1E-999  ->   -1.0E-1000 Inexact Rounded Subnormal Underflow", c3hu);


//             //-- check for double-rounded subnormals
//             //precision:   5
//             //maxexponent: 79
//             //minexponent: -79
//             //?TSub("subx1101 subtract  0 1.52444E-80 -> -1.524E-80 Inexact Rounded Subnormal Underflow", c5hu);
//             //?TSub("subx1102 subtract  0 1.52445E-80 -> -1.524E-80 Inexact Rounded Subnormal Underflow", c5hu);
//             //?TSub("subx1103 subtract  0 1.52446E-80 -> -1.524E-80 Inexact Rounded Subnormal Underflow", c5hu);
//             //?TSub("subx1104 subtract  1.52444E-80 0 ->  1.524E-80 Inexact Rounded Subnormal Underflow", c5hu);
//             //?TSub("subx1105 subtract  1.52445E-80 0 ->  1.524E-80 Inexact Rounded Subnormal Underflow", c5hu);
//             //?TSub("subx1106 subtract  1.52446E-80 0 ->  1.524E-80 Inexact Rounded Subnormal Underflow", c5hu);

//             //?TSub("subx1111 subtract  1.2345678E-80  1.2345671E-80 ->  0E-83 Inexact Rounded Subnormal Underflow Clamped", c5hu);
//             //?TSub("subx1112 subtract  1.2345678E-80  1.2345618E-80 ->  0E-83 Inexact Rounded Subnormal Underflow Clamped", c5hu);
//             //?TSub("subx1113 subtract  1.2345678E-80  1.2345178E-80 ->  0E-83 Inexact Rounded Subnormal Underflow Clamped", c5hu);
//             //?TSub("subx1114 subtract  1.2345678E-80  1.2341678E-80 ->  0E-83 Inexact Rounded Subnormal Underflow Clamped", c5hu);
//             //?TSub("subx1115 subtract  1.2345678E-80  1.2315678E-80 ->  3E-83         Rounded Subnormal", c5hu);
//             //?TSub("subx1116 subtract  1.2345678E-80  1.2145678E-80 ->  2.0E-82       Rounded Subnormal", c5hu);
//             //?TSub("subx1117 subtract  1.2345678E-80  1.1345678E-80 ->  1.00E-81      Rounded Subnormal", c5hu);
//             //?TSub("subx1118 subtract  1.2345678E-80  0.2345678E-80 ->  1.000E-80     Rounded Subnormal", c5hu);

//             //precision:   34
//             //rounding:    half_up
//             //maxExponent: 6144
//             //minExponent: -6143
//             //-- Examples from SQL proposal (Krishna Kulkarni)
//             BigDecimal.Context c34hu = new BigDecimal.Context(34, BigDecimal.RoundingMode.HalfUp);

//             TSub("subx1125  subtract 130E-2  120E-2 -> 0.10", c34hu);
//             TSub("subx1126  subtract 130E-2  12E-1  -> 0.10", c34hu);
//             TSub("subx1127  subtract 130E-2  1E0    -> 0.30", c34hu);
//             TSub("subx1128  subtract 1E2     1E4    -> -9.9E+3", c34hu);

//             //-- Null tests
//             //subx9990 subtract 10  # -> NaN Invalid_operation
//             //subx9991 subtract  # 10 -> NaN Invalid_operation

//         }


//         static void TSub(string test, BigDecimal.Context c)
//         {
//             GetThreeArgs(test, out string arg1Str, out string arg2Str, out string resultStr);
//             TestSubtraction(arg1Str, arg2Str, c, resultStr);
//         }

//         static void TestSubtraction(string arg1Str, string arg2Str, BigDecimal.Context c, string resultStr)
//         {
//             BigDecimal arg1 = BigDecimal.Parse(arg1Str);
//             BigDecimal arg2 = BigDecimal.Parse(arg2Str);
//             BigDecimal val = arg1.Subtract(arg2, c);
//             string valStr = val.ToScientificString();
//             Expect(valStr).To.Equal(resultStr);
//         }

//         #endregion

//         #region Multiply

//         [Test]
//         public void SpecTestMultiply()
//         {

//             //version: 2.59

//             //extended:    1
//             //precision:   9
//             //rounding:    half_up
//             //maxExponent: 384
//             //minexponent: -383

//             BigDecimal.Context c6hu = new BigDecimal.Context(6, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c7hu = new BigDecimal.Context(7, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c8hu = new BigDecimal.Context(8, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c9hu = new BigDecimal.Context(9, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c15hu = new BigDecimal.Context(15, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c30hu = new BigDecimal.Context(30, BigDecimal.RoundingMode.HalfUp);
//             BigDecimal.Context c33hu = new BigDecimal.Context(33, BigDecimal.RoundingMode.HalfUp);

//             //-- sanity checks (as base, above)
//             TMul("mulx000 multiply 2      2 -> 4", c9hu);
//             TMul("mulx001 multiply 2      3 -> 6", c9hu);
//             TMul("mulx002 multiply 5      1 -> 5", c9hu);
//             TMul("mulx003 multiply 5      2 -> 10", c9hu);
//             TMul("mulx004 multiply 1.20   2 -> 2.40", c9hu);
//             TMul("mulx005 multiply 1.20   0 -> 0.00", c9hu);
//             TMul("mulx006 multiply 1.20  -2 -> -2.40", c9hu);
//             TMul("mulx007 multiply -1.20  2 -> -2.40", c9hu);
//             TMul("mulx008 multiply -1.20  0 -> 0.00", c9hu);  // mod: neg 0
//             TMul("mulx009 multiply -1.20 -2 -> 2.40", c9hu);
//             TMul("mulx010 multiply 5.09 7.1 -> 36.139", c9hu);
//             TMul("mulx011 multiply 2.5    4 -> 10.0", c9hu);
//             TMul("mulx012 multiply 2.50   4 -> 10.00", c9hu);
//             TMul("mulx013 multiply 1.23456789 1.00000000 -> 1.23456789 Rounded", c9hu);
//             TMul("mulx014 multiply 9.999999999 9.999999999 -> 100.000000 Inexact Rounded", c9hu);
//             TMul("mulx015 multiply 2.50   4 -> 10.00", c9hu);
//             //precision: 6
//             TMul("mulx016 multiply 2.50   4 -> 10.00", c6hu);
//             TMul("mulx017 multiply  9.999999999  9.999999999 ->  100.000 Inexact Rounded", c6hu);
//             TMul("mulx018 multiply  9.999999999 -9.999999999 -> -100.000 Inexact Rounded", c6hu);
//             TMul("mulx019 multiply -9.999999999  9.999999999 -> -100.000 Inexact Rounded", c6hu);
//             TMul("mulx020 multiply -9.999999999 -9.999999999 ->  100.000 Inexact Rounded", c6hu);

//             //-- 1999.12.21: next one is a edge case if intermediate longs are used
//             //precision: 15
//             TMul("mulx059 multiply 999999999999 9765625 -> 9.76562499999023E+18 Inexact Rounded", c15hu);
//             //precision: 30
//             TMul("mulx160 multiply 999999999999 9765625 -> 9765624999990234375", c30hu);
//             //precision: 9
//             //-----

//             //-- zeros, etc.
//             TMul("mulx021 multiply  0      0     ->  0", c9hu);
//             TMul("mulx022 multiply  0     -0     -> 0", c9hu);  // mod: neg 0
//             TMul("mulx023 multiply -0      0     -> 0", c9hu); // mod: neg 0
//             TMul("mulx024 multiply -0     -0     ->  0", c9hu);
//             TMul("mulx025 multiply -0.0   -0.0   ->  0.00", c9hu);
//             TMul("mulx026 multiply -0.0   -0.0   ->  0.00", c9hu);
//             TMul("mulx027 multiply -0.0   -0.0   ->  0.00", c9hu);
//             TMul("mulx028 multiply -0.0   -0.0   ->  0.00", c9hu);
//             TMul("mulx030 multiply  5.00   1E-3  ->  0.00500", c9hu);
//             TMul("mulx031 multiply  00.00  0.000 ->  0.00000", c9hu);
//             TMul("mulx032 multiply  00.00  0E-3  ->  0.00000     -- rhs is 0", c9hu);
//             TMul("mulx033 multiply  0E-3   00.00 ->  0.00000     -- lhs is 0", c9hu);
//             TMul("mulx034 multiply -5.00   1E-3  -> -0.00500", c9hu);
//             TMul("mulx035 multiply -00.00  0.000 -> 0.00000", c9hu); // mod: neg 0
//             TMul("mulx036 multiply -00.00  0E-3  -> 0.00000     -- rhs is 0", c9hu); // mod: neg 0
//             TMul("mulx037 multiply -0E-3   00.00 -> 0.00000     -- lhs is 0", c9hu); // mod: neg 0
//             TMul("mulx038 multiply  5.00  -1E-3  -> -0.00500", c9hu);
//             TMul("mulx039 multiply  00.00 -0.000 -> 0.00000", c9hu); // mod: neg 0
//             TMul("mulx040 multiply  00.00 -0E-3  -> 0.00000     -- rhs is 0", c9hu); // mod: neg 0
//             TMul("mulx041 multiply  0E-3  -00.00 -> 0.00000     -- lhs is 0", c9hu); // mod: neg 0
//             TMul("mulx042 multiply -5.00  -1E-3  ->  0.00500", c9hu);
//             TMul("mulx043 multiply -00.00 -0.000 ->  0.00000", c9hu);
//             TMul("mulx044 multiply -00.00 -0E-3  ->  0.00000     -- rhs is 0", c9hu);
//             TMul("mulx045 multiply -0E-3  -00.00 ->  0.00000     -- lhs is 0", c9hu);

//             //-- examples from decarith
//             TMul("mulx050 multiply 1.20 3        -> 3.60", c9hu);
//             TMul("mulx051 multiply 7    3        -> 21", c9hu);
//             TMul("mulx052 multiply 0.9  0.8      -> 0.72", c9hu);
//             TMul("mulx053 multiply 0.9  -0       -> 0.0", c9hu); // mod: neg 0
//             TMul("mulx054 multiply 654321 654321 -> 4.28135971E+11  Inexact Rounded", c9hu);

//             TMul("mulx060 multiply 123.45 1e7  ->  1.2345E+9", c9hu);
//             TMul("mulx061 multiply 123.45 1e8  ->  1.2345E+10", c9hu);
//             TMul("mulx062 multiply 123.45 1e+9 ->  1.2345E+11", c9hu);
//             TMul("mulx063 multiply 123.45 1e10 ->  1.2345E+12", c9hu);
//             TMul("mulx064 multiply 123.45 1e11 ->  1.2345E+13", c9hu);
//             TMul("mulx065 multiply 123.45 1e12 ->  1.2345E+14", c9hu);
//             TMul("mulx066 multiply 123.45 1e13 ->  1.2345E+15", c9hu);


//             //-- test some intermediate lengths
//             //precision: 9
//             TMul("mulx080 multiply 0.1 123456789          -> 12345678.9", c9hu);
//             TMul("mulx081 multiply 0.1 1234567891         -> 123456789 Inexact Rounded", c9hu);
//             TMul("mulx082 multiply 0.1 12345678912        -> 1.23456789E+9 Inexact Rounded", c9hu);
//             TMul("mulx083 multiply 0.1 12345678912345     -> 1.23456789E+12 Inexact Rounded", c9hu);
//             TMul("mulx084 multiply 0.1 123456789          -> 12345678.9", c9hu);
//             //precision: 8
//             TMul("mulx085 multiply 0.1 12345678912        -> 1.2345679E+9 Inexact Rounded", c8hu);
//             TMul("mulx086 multiply 0.1 12345678912345     -> 1.2345679E+12 Inexact Rounded", c8hu);
//             //precision: 7
//             TMul("mulx087 multiply 0.1 12345678912        -> 1.234568E+9 Inexact Rounded", c7hu);
//             TMul("mulx088 multiply 0.1 12345678912345     -> 1.234568E+12 Inexact Rounded", c7hu);

//             //precision: 9
//             TMul("mulx090 multiply 123456789          0.1 -> 12345678.9", c9hu);
//             TMul("mulx091 multiply 1234567891         0.1 -> 123456789 Inexact Rounded", c9hu);
//             TMul("mulx092 multiply 12345678912        0.1 -> 1.23456789E+9 Inexact Rounded", c9hu);
//             TMul("mulx093 multiply 12345678912345     0.1 -> 1.23456789E+12 Inexact Rounded", c9hu);
//             TMul("mulx094 multiply 123456789          0.1 -> 12345678.9", c9hu);
//             //precision: 8
//             TMul("mulx095 multiply 12345678912        0.1 -> 1.2345679E+9 Inexact Rounded", c8hu);
//             TMul("mulx096 multiply 12345678912345     0.1 -> 1.2345679E+12 Inexact Rounded", c8hu);
//             //precision: 7
//             TMul("mulx097 multiply 12345678912        0.1 -> 1.234568E+9 Inexact Rounded", c7hu);
//             TMul("mulx098 multiply 12345678912345     0.1 -> 1.234568E+12 Inexact Rounded", c7hu);

//             //-- test some more edge cases and carries
//             //maxexponent: 9999
//             //minexponent: -9999
//             //precision: 33
//             TMul("mulx101 multiply 9 9   -> 81", c33hu);
//             TMul("mulx102 multiply 9 90   -> 810", c33hu);
//             TMul("mulx103 multiply 9 900   -> 8100", c33hu);
//             TMul("mulx104 multiply 9 9000   -> 81000", c33hu);
//             TMul("mulx105 multiply 9 90000   -> 810000", c33hu);
//             TMul("mulx106 multiply 9 900000   -> 8100000", c33hu);
//             TMul("mulx107 multiply 9 9000000   -> 81000000", c33hu);
//             TMul("mulx108 multiply 9 90000000   -> 810000000", c33hu);
//             TMul("mulx109 multiply 9 900000000   -> 8100000000", c33hu);
//             TMul("mulx110 multiply 9 9000000000   -> 81000000000", c33hu);
//             TMul("mulx111 multiply 9 90000000000   -> 810000000000", c33hu);
//             TMul("mulx112 multiply 9 900000000000   -> 8100000000000", c33hu);
//             TMul("mulx113 multiply 9 9000000000000   -> 81000000000000", c33hu);
//             TMul("mulx114 multiply 9 90000000000000   -> 810000000000000", c33hu);
//             TMul("mulx115 multiply 9 900000000000000   -> 8100000000000000", c33hu);
//             TMul("mulx116 multiply 9 9000000000000000   -> 81000000000000000", c33hu);
//             TMul("mulx117 multiply 9 90000000000000000   -> 810000000000000000", c33hu);
//             TMul("mulx118 multiply 9 900000000000000000   -> 8100000000000000000", c33hu);
//             TMul("mulx119 multiply 9 9000000000000000000   -> 81000000000000000000", c33hu);
//             TMul("mulx120 multiply 9 90000000000000000000   -> 810000000000000000000", c33hu);
//             TMul("mulx121 multiply 9 900000000000000000000   -> 8100000000000000000000", c33hu);
//             TMul("mulx122 multiply 9 9000000000000000000000   -> 81000000000000000000000", c33hu);
//             TMul("mulx123 multiply 9 90000000000000000000000   -> 810000000000000000000000", c33hu);
//             //-- test some more edge cases without carries
//             TMul("mulx131 multiply 3 3   -> 9", c33hu);
//             TMul("mulx132 multiply 3 30   -> 90", c33hu);
//             TMul("mulx133 multiply 3 300   -> 900", c33hu);
//             TMul("mulx134 multiply 3 3000   -> 9000", c33hu);
//             TMul("mulx135 multiply 3 30000   -> 90000", c33hu);
//             TMul("mulx136 multiply 3 300000   -> 900000", c33hu);
//             TMul("mulx137 multiply 3 3000000   -> 9000000", c33hu);
//             TMul("mulx138 multiply 3 30000000   -> 90000000", c33hu);
//             TMul("mulx139 multiply 3 300000000   -> 900000000", c33hu);
//             TMul("mulx140 multiply 3 3000000000   -> 9000000000", c33hu);
//             TMul("mulx141 multiply 3 30000000000   -> 90000000000", c33hu);
//             TMul("mulx142 multiply 3 300000000000   -> 900000000000", c33hu);
//             TMul("mulx143 multiply 3 3000000000000   -> 9000000000000", c33hu);
//             TMul("mulx144 multiply 3 30000000000000   -> 90000000000000", c33hu);
//             TMul("mulx145 multiply 3 300000000000000   -> 900000000000000", c33hu);
//             TMul("mulx146 multiply 3 3000000000000000   -> 9000000000000000", c33hu);
//             TMul("mulx147 multiply 3 30000000000000000   -> 90000000000000000", c33hu);
//             TMul("mulx148 multiply 3 300000000000000000   -> 900000000000000000", c33hu);
//             TMul("mulx149 multiply 3 3000000000000000000   -> 9000000000000000000", c33hu);
//             TMul("mulx150 multiply 3 30000000000000000000   -> 90000000000000000000", c33hu);
//             TMul("mulx151 multiply 3 300000000000000000000   -> 900000000000000000000", c33hu);
//             TMul("mulx152 multiply 3 3000000000000000000000   -> 9000000000000000000000", c33hu);
//             TMul("mulx153 multiply 3 30000000000000000000000   -> 90000000000000000000000", c33hu);

//             //maxexponent: 999999999
//             //minexponent: -999999999
//             //precision: 9
//             //-- test some cases that are close to exponent overflow/underflow
//             TMul("mulx170 multiply 1 9e999999999    -> 9E+999999999", c9hu);
//             TMul("mulx171 multiply 1 9.9e999999999  -> 9.9E+999999999", c9hu);
//             TMul("mulx172 multiply 1 9.99e999999999 -> 9.99E+999999999", c9hu);
//             TMul("mulx173 multiply 9e999999999    1 -> 9E+999999999", c9hu);
//             TMul("mulx174 multiply 9.9e999999999  1 -> 9.9E+999999999", c9hu);
//             TMul("mulx176 multiply 9.99e999999999 1 -> 9.99E+999999999", c9hu);
//             TMul("mulx177 multiply 1 9.99999999e999999999 -> 9.99999999E+999999999", c9hu);
//             TMul("mulx178 multiply 9.99999999e999999999 1 -> 9.99999999E+999999999", c9hu);

//             TMul("mulx180 multiply 0.1 9e-999999998   -> 9E-999999999", c9hu);
//             TMul("mulx181 multiply 0.1 99e-999999998  -> 9.9E-999999998", c9hu);
//             TMul("mulx182 multiply 0.1 999e-999999998 -> 9.99E-999999997", c9hu);

//             TMul("mulx183 multiply 0.1 9e-999999998     -> 9E-999999999", c9hu);
//             TMul("mulx184 multiply 0.1 99e-999999998    -> 9.9E-999999998", c9hu);
//             TMul("mulx185 multiply 0.1 999e-999999998   -> 9.99E-999999997", c9hu);
//             TMul("mulx186 multiply 0.1 999e-999999997   -> 9.99E-999999996", c9hu);
//             TMul("mulx187 multiply 0.1 9999e-999999997  -> 9.999E-999999995", c9hu);
//             TMul("mulx188 multiply 0.1 99999e-999999997 -> 9.9999E-999999994", c9hu);

//             TMul("mulx190 multiply 1 9e-999999998   -> 9E-999999998", c9hu);
//             TMul("mulx191 multiply 1 99e-999999998  -> 9.9E-999999997", c9hu);
//             TMul("mulx192 multiply 1 999e-999999998 -> 9.99E-999999996", c9hu);
//             TMul("mulx193 multiply 9e-999999998   1 -> 9E-999999998", c9hu);
//             TMul("mulx194 multiply 99e-999999998  1 -> 9.9E-999999997", c9hu);
//             TMul("mulx195 multiply 999e-999999998 1 -> 9.99E-999999996", c9hu);

//             TMul("mulx196 multiply 1e-599999999 1e-400000000 -> 1E-999999999", c9hu);
//             TMul("mulx197 multiply 1e-600000000 1e-399999999 -> 1E-999999999", c9hu);
//             TMul("mulx198 multiply 1.2e-599999999 1.2e-400000000 -> 1.44E-999999999", c9hu);
//             TMul("mulx199 multiply 1.2e-600000000 1.2e-399999999 -> 1.44E-999999999", c9hu);

//             TMul("mulx201 multiply 1e599999999 1e400000000 -> 1E+999999999", c9hu);
//             TMul("mulx202 multiply 1e600000000 1e399999999 -> 1E+999999999", c9hu);
//             TMul("mulx203 multiply 1.2e599999999 1.2e400000000 -> 1.44E+999999999", c9hu);
//             TMul("mulx204 multiply 1.2e600000000 1.2e399999999 -> 1.44E+999999999", c9hu);

//             //-- long operand triangle
//             //precision: 33
//             TMul("mulx246 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.290801193369671916511992830 Inexact Rounded", new BigDecimal.Context(33, BigDecimal.RoundingMode.HalfUp));
//             //precision: 32
//             TMul("mulx247 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.29080119336967191651199283  Inexact Rounded", new BigDecimal.Context(32, BigDecimal.RoundingMode.HalfUp));
//             //precision: 31
//             TMul("mulx248 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.2908011933696719165119928   Inexact Rounded", new BigDecimal.Context(31, BigDecimal.RoundingMode.HalfUp));
//             //precision: 30
//             TMul("mulx249 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.290801193369671916511993    Inexact Rounded", new BigDecimal.Context(30, BigDecimal.RoundingMode.HalfUp));
//             //precision: 29
//             TMul("mulx250 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.29080119336967191651199     Inexact Rounded", new BigDecimal.Context(29, BigDecimal.RoundingMode.HalfUp));
//             //precision: 28
//             TMul("mulx251 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.2908011933696719165120      Inexact Rounded", new BigDecimal.Context(28, BigDecimal.RoundingMode.HalfUp));
//             //precision: 27
//             TMul("mulx252 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.290801193369671916512       Inexact Rounded", new BigDecimal.Context(27, BigDecimal.RoundingMode.HalfUp));
//             //precision: 26
//             TMul("mulx253 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.29080119336967191651        Inexact Rounded", new BigDecimal.Context(26, BigDecimal.RoundingMode.HalfUp));
//             //precision: 25
//             TMul("mulx254 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.2908011933696719165         Inexact Rounded", new BigDecimal.Context(25, BigDecimal.RoundingMode.HalfUp));
//             //precision: 24
//             TMul("mulx255 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.290801193369671917          Inexact Rounded", new BigDecimal.Context(24, BigDecimal.RoundingMode.HalfUp));
//             //precision: 23
//             TMul("mulx256 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.29080119336967192           Inexact Rounded", new BigDecimal.Context(23, BigDecimal.RoundingMode.HalfUp));
//             //precision: 22
//             TMul("mulx257 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.2908011933696719            Inexact Rounded", new BigDecimal.Context(22, BigDecimal.RoundingMode.HalfUp));
//             //precision: 21
//             TMul("mulx258 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.290801193369672             Inexact Rounded", new BigDecimal.Context(21, BigDecimal.RoundingMode.HalfUp));
//             //precision: 20
//             TMul("mulx259 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.29080119336967              Inexact Rounded", new BigDecimal.Context(20, BigDecimal.RoundingMode.HalfUp));
//             //precision: 19
//             TMul("mulx260 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.2908011933697               Inexact Rounded", new BigDecimal.Context(19, BigDecimal.RoundingMode.HalfUp));
//             //precision: 18
//             TMul("mulx261 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.290801193370                Inexact Rounded", new BigDecimal.Context(18, BigDecimal.RoundingMode.HalfUp));
//             //precision: 17
//             TMul("mulx262 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.29080119337                 Inexact Rounded", new BigDecimal.Context(17, BigDecimal.RoundingMode.HalfUp));
//             //precision: 16
//             TMul("mulx263 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.2908011934                  Inexact Rounded", new BigDecimal.Context(16, BigDecimal.RoundingMode.HalfUp));
//             //precision: 15
//             TMul("mulx264 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.290801193                   Inexact Rounded", new BigDecimal.Context(15, BigDecimal.RoundingMode.HalfUp));
//             //precision: 14
//             TMul("mulx265 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.29080119                    Inexact Rounded", new BigDecimal.Context(14, BigDecimal.RoundingMode.HalfUp));
//             //precision: 13
//             TMul("mulx266 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.2908012                     Inexact Rounded", new BigDecimal.Context(13, BigDecimal.RoundingMode.HalfUp));
//             //precision: 12
//             TMul("mulx267 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.290801                      Inexact Rounded", new BigDecimal.Context(12, BigDecimal.RoundingMode.HalfUp));
//             //precision: 11
//             TMul("mulx268 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.29080                       Inexact Rounded", new BigDecimal.Context(11, BigDecimal.RoundingMode.HalfUp));
//             //precision: 10
//             TMul("mulx269 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.2908                        Inexact Rounded", new BigDecimal.Context(10, BigDecimal.RoundingMode.HalfUp));
//             //precision:  9
//             TMul("mulx270 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.291                         Inexact Rounded", new BigDecimal.Context(9, BigDecimal.RoundingMode.HalfUp));
//             //precision:  8
//             TMul("mulx271 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.29                          Inexact Rounded", new BigDecimal.Context(8, BigDecimal.RoundingMode.HalfUp));
//             //precision:  7
//             TMul("mulx272 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433.3                           Inexact Rounded", new BigDecimal.Context(7, BigDecimal.RoundingMode.HalfUp));
//             //precision:  6
//             TMul("mulx273 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 145433                            Inexact Rounded", new BigDecimal.Context(6, BigDecimal.RoundingMode.HalfUp));
//             //precision:  5
//             TMul("mulx274 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 1.4543E+5                         Inexact Rounded", new BigDecimal.Context(5, BigDecimal.RoundingMode.HalfUp));
//             //precision:  4
//             TMul("mulx275 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 1.454E+5                         Inexact Rounded", new BigDecimal.Context(4, BigDecimal.RoundingMode.HalfUp));
//             //precision:  3
//             TMul("mulx276 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 1.45E+5                         Inexact Rounded", new BigDecimal.Context(3, BigDecimal.RoundingMode.HalfUp));
//             //precision:  2
//             TMul("mulx277 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 1.5E+5                         Inexact Rounded", new BigDecimal.Context(2, BigDecimal.RoundingMode.HalfUp));
//             //precision:  1
//             TMul("mulx278 multiply 30269.587755640502150977251770554 4.8046009735990873395936309640543 -> 1E+5                          Inexact Rounded", new BigDecimal.Context(1, BigDecimal.RoundingMode.HalfUp));

//             //-- test some edge cases with exact rounding
//             //maxexponent: 9999
//             //minexponent: -9999
//             //precision: 9
//             TMul("mulx301 multiply 9 9   -> 81", c9hu);
//             TMul("mulx302 multiply 9 90   -> 810", c9hu);
//             TMul("mulx303 multiply 9 900   -> 8100", c9hu);
//             TMul("mulx304 multiply 9 9000   -> 81000", c9hu);
//             TMul("mulx305 multiply 9 90000   -> 810000", c9hu);
//             TMul("mulx306 multiply 9 900000   -> 8100000", c9hu);
//             TMul("mulx307 multiply 9 9000000   -> 81000000", c9hu);
//             TMul("mulx308 multiply 9 90000000   -> 810000000", c9hu);
//             TMul("mulx309 multiply 9 900000000   -> 8.10000000E+9   Rounded", c9hu);
//             TMul("mulx310 multiply 9 9000000000   -> 8.10000000E+10  Rounded", c9hu);
//             TMul("mulx311 multiply 9 90000000000   -> 8.10000000E+11  Rounded", c9hu);
//             TMul("mulx312 multiply 9 900000000000   -> 8.10000000E+12  Rounded", c9hu);
//             TMul("mulx313 multiply 9 9000000000000   -> 8.10000000E+13  Rounded", c9hu);
//             TMul("mulx314 multiply 9 90000000000000   -> 8.10000000E+14  Rounded", c9hu);
//             TMul("mulx315 multiply 9 900000000000000   -> 8.10000000E+15  Rounded", c9hu);
//             TMul("mulx316 multiply 9 9000000000000000   -> 8.10000000E+16  Rounded", c9hu);
//             TMul("mulx317 multiply 9 90000000000000000   -> 8.10000000E+17  Rounded", c9hu);
//             TMul("mulx318 multiply 9 900000000000000000   -> 8.10000000E+18  Rounded", c9hu);
//             TMul("mulx319 multiply 9 9000000000000000000   -> 8.10000000E+19  Rounded", c9hu);
//             TMul("mulx320 multiply 9 90000000000000000000   -> 8.10000000E+20  Rounded", c9hu);
//             TMul("mulx321 multiply 9 900000000000000000000   -> 8.10000000E+21  Rounded", c9hu);
//             TMul("mulx322 multiply 9 9000000000000000000000   -> 8.10000000E+22  Rounded", c9hu);
//             TMul("mulx323 multiply 9 90000000000000000000000   -> 8.10000000E+23  Rounded", c9hu);

//             //-- fastpath breakers
//             //precision:   29
//             TMul("mulx330 multiply 1.491824697641270317824852952837224 1.105170918075647624811707826490246514675628614562883537345747603 -> 1.6487212707001281468486507878 Inexact Rounded", new BigDecimal.Context(29, BigDecimal.RoundingMode.HalfUp));
//             //precision:   55
//             TMul("mulx331 multiply 0.8958341352965282506768545828765117803873717284891040428 0.8958341352965282506768545828765117803873717284891040428 -> 0.8025187979624784829842553829934069955890983696752228299 Inexact Rounded", new BigDecimal.Context(55, BigDecimal.RoundingMode.HalfUp));


//             //-- tryzeros cases
//             //precision:   7
//             //rounding:    half_up
//             //maxExponent: 92
//             //minexponent: -92
//             //TMul("mulx504  multiply  0E-60 1000E-60  -> 0E-98 Clamped", new BigDecimal.Context(7, BigDecimal.RoundingMode.HalfUp));
//             //TMul("mulx505  multiply  100E+60 0E+60   -> 0E+92 Clamped", new BigDecimal.Context(7, BigDecimal.RoundingMode.HalfUp));

//             //-- mixed with zeros
//             //maxexponent: 999999999
//             //minexponent: -999999999
//             //precision: 9
//             TMul("mulx541 multiply  0    -1     -> 0", c9hu);  // mod: neg zero
//             TMul("mulx542 multiply -0    -1     ->  0", c9hu);
//             TMul("mulx543 multiply  0     1     ->  0", c9hu);
//             TMul("mulx544 multiply -0     1     -> 0", c9hu);  // mod: neg zero
//             TMul("mulx545 multiply -1     0     -> 0", c9hu);  // mod: neg zero
//             TMul("mulx546 multiply -1    -0     ->  0", c9hu);
//             TMul("mulx547 multiply  1     0     ->  0", c9hu);
//             TMul("mulx548 multiply  1    -0     -> 0", c9hu);  // mod: neg zero

//             TMul("mulx551 multiply  0.0  -1     -> 0.0", c9hu);  // mod: neg zero
//             TMul("mulx552 multiply -0.0  -1     ->  0.0", c9hu);
//             TMul("mulx553 multiply  0.0   1     ->  0.0", c9hu);
//             TMul("mulx554 multiply -0.0   1     -> 0.0", c9hu);  // mod: neg zero
//             TMul("mulx555 multiply -1.0   0     -> 0.0", c9hu);  // mod: neg zero
//             TMul("mulx556 multiply -1.0  -0     ->  0.0", c9hu);
//             TMul("mulx557 multiply  1.0   0     ->  0.0", c9hu);
//             TMul("mulx558 multiply  1.0  -0     -> 0.0", c9hu);  // mod: neg zero

//             TMul("mulx561 multiply  0    -1.0   -> 0.0", c9hu);  // mod: neg zero
//             TMul("mulx562 multiply -0    -1.0   ->  0.0", c9hu);
//             TMul("mulx563 multiply  0     1.0   ->  0.0", c9hu);
//             TMul("mulx564 multiply -0     1.0   -> 0.0", c9hu);  // mod: neg zero
//             TMul("mulx565 multiply -1     0.0   -> 0.0", c9hu);  // mod: neg zero
//             TMul("mulx566 multiply -1    -0.0   ->  0.0", c9hu);
//             TMul("mulx567 multiply  1     0.0   ->  0.0", c9hu);
//             TMul("mulx568 multiply  1    -0.0   -> 0.0", c9hu);  // mod: neg zero

//             TMul("mulx571 multiply  0.0  -1.0   -> 0.00", c9hu);  // mod: neg zero
//             TMul("mulx572 multiply -0.0  -1.0   ->  0.00", c9hu);
//             TMul("mulx573 multiply  0.0   1.0   ->  0.00", c9hu);
//             TMul("mulx574 multiply -0.0   1.0   -> 0.00", c9hu);  // mod: neg zero
//             TMul("mulx575 multiply -1.0   0.0   -> 0.00", c9hu);  // mod: neg zero
//             TMul("mulx576 multiply -1.0  -0.0   ->  0.00", c9hu);
//             TMul("mulx577 multiply  1.0   0.0   ->  0.00", c9hu);
//             TMul("mulx578 multiply  1.0  -0.0   -> 0.00", c9hu);  // mod: neg zero


//             //-- Specials



//             //-- test subnormals rounding
//             //precision:   5
//             //maxExponent: 999
//             //minexponent: -999
//             //rounding:    half_even


//             //...

//         }

//         static void TMul(string test, BigDecimal.Context c)
//         {
//             GetThreeArgs(test, out string arg1Str, out string arg2Str, out string resultStr);
//             TestMultiply(arg1Str, arg2Str, c, resultStr);
//         }

//         static void TestMultiply(string arg1Str, string arg2Str, BigDecimal.Context c, string resultStr)
//         {
//             BigDecimal arg1 = BigDecimal.Parse(arg1Str);
//             BigDecimal arg2 = BigDecimal.Parse(arg2Str);
//             BigDecimal val = arg1.Multiply(arg2, c);
//             string valStr = val.ToScientificString();
//             Expect(valStr).To.Equal(resultStr);
//         }

//         #endregion

//         #region Divide tests

//         [Test]
//         public void TestSpecDivide()
//         {

//             BigDecimal.Context c9hu = new BigDecimal.Context(9, BigDecimal.RoundingMode.HalfUp);
//             //version: 2.59

//             //extended:    1
//             //precision:   9
//             //rounding:    half_up
//             //maxExponent: 384
//             //minexponent: -383

//             //-- sanity checks
//             TDiv("divx001 divide  1     1    ->  1", c9hu);
//             TDiv("divx002 divide  2     1    ->  2", c9hu);
//             TDiv("divx003 divide  1     2    ->  0.5", c9hu);
//             TDiv("divx004 divide  2     2    ->  1", c9hu);
//             TDiv("divx005 divide  0     1    ->  0", c9hu);
//             TDiv("divx006 divide  0     2    ->  0", c9hu);
//             TDiv("divx007 divide  1     3    ->  0.333333333 Inexact Rounded", c9hu);
//             TDiv("divx008 divide  2     3    ->  0.666666667 Inexact Rounded", c9hu);
//             TDiv("divx009 divide  3     3    ->  1", c9hu);

//             TDiv("divx010 divide  2.4   1    ->  2.4", c9hu);
//             TDiv("divx011 divide  2.4   -1   ->  -2.4", c9hu);
//             TDiv("divx012 divide  -2.4  1    ->  -2.4", c9hu);
//             TDiv("divx013 divide  -2.4  -1   ->  2.4", c9hu);
//             TDiv("divx014 divide  2.40  1    ->  2.40", c9hu);
//             TDiv("divx015 divide  2.400 1    ->  2.400", c9hu);
//             TDiv("divx016 divide  2.4   2    ->  1.2", c9hu);
//             TDiv("divx017 divide  2.400 2    ->  1.200", c9hu);
//             TDiv("divx018 divide  2.    2    ->  1", c9hu);
//             TDiv("divx019 divide  20    20   ->  1", c9hu);

//             TDiv("divx020 divide  187   187    ->  1", c9hu);
//             TDiv("divx021 divide  5     2      ->  2.5", c9hu);
//             TDiv("divx022 divide  50    20     ->  2.5", c9hu);
//             TDiv("divx023 divide  500   200    ->  2.5", c9hu);
//             TDiv("divx024 divide  50.0  20.0   ->  2.5", c9hu);
//             TDiv("divx025 divide  5.00  2.00   ->  2.5", c9hu);
//             TDiv("divx026 divide  5     2.0    ->  2.5", c9hu);
//             TDiv("divx027 divide  5     2.000  ->  2.5", c9hu);
//             TDiv("divx028 divide  5     0.20   ->  25", c9hu);
//             TDiv("divx029 divide  5     0.200  ->  25", c9hu);
//             TDiv("divx030 divide  10    1      ->  10", c9hu);
//             TDiv("divx031 divide  100   1      ->  100", c9hu);
//             TDiv("divx032 divide  1000  1      ->  1000", c9hu);
//             TDiv("divx033 divide  1000  100    ->  10", c9hu);

//             TDiv("divx035 divide  1     2      ->  0.5", c9hu);
//             TDiv("divx036 divide  1     4      ->  0.25", c9hu);
//             TDiv("divx037 divide  1     8      ->  0.125", c9hu);
//             TDiv("divx038 divide  1     16     ->  0.0625", c9hu);
//             TDiv("divx039 divide  1     32     ->  0.03125", c9hu);
//             TDiv("divx040 divide  1     64     ->  0.015625", c9hu);
//             TDiv("divx041 divide  1    -2      ->  -0.5", c9hu);
//             TDiv("divx042 divide  1    -4      ->  -0.25", c9hu);
//             TDiv("divx043 divide  1    -8      ->  -0.125", c9hu);
//             TDiv("divx044 divide  1    -16     ->  -0.0625", c9hu);
//             TDiv("divx045 divide  1    -32     ->  -0.03125", c9hu);
//             TDiv("divx046 divide  1    -64     ->  -0.015625", c9hu);
//             TDiv("divx047 divide -1     2      ->  -0.5", c9hu);
//             TDiv("divx048 divide -1     4      ->  -0.25", c9hu);
//             TDiv("divx049 divide -1     8      ->  -0.125", c9hu);
//             TDiv("divx050 divide -1     16     ->  -0.0625", c9hu);
//             TDiv("divx051 divide -1     32     ->  -0.03125", c9hu);
//             TDiv("divx052 divide -1     64     ->  -0.015625", c9hu);
//             TDiv("divx053 divide -1    -2      ->  0.5", c9hu);
//             TDiv("divx054 divide -1    -4      ->  0.25", c9hu);
//             TDiv("divx055 divide -1    -8      ->  0.125", c9hu);
//             TDiv("divx056 divide -1    -16     ->  0.0625", c9hu);
//             TDiv("divx057 divide -1    -32     ->  0.03125", c9hu);
//             TDiv("divx058 divide -1    -64     ->  0.015625", c9hu);

//             TDiv("divx070 divide  999999999        1    ->  999999999", c9hu);
//             TDiv("divx071 divide  999999999.4      1    ->  999999999 Inexact Rounded", c9hu);
//             TDiv("divx072 divide  999999999.5      1    ->  1.00000000E+9 Inexact Rounded", c9hu);
//             TDiv("divx073 divide  999999999.9      1    ->  1.00000000E+9 Inexact Rounded", c9hu);
//             TDiv("divx074 divide  999999999.999    1    ->  1.00000000E+9 Inexact Rounded", c9hu);
//             //precision: 6
//             BigDecimal.Context c6hu = new BigDecimal.Context(6, BigDecimal.RoundingMode.HalfUp);
//             TDiv("divx080 divide  999999999     1  ->  1.00000E+9 Inexact Rounded", c6hu);
//             TDiv("divx081 divide  99999999      1  ->  1.00000E+8 Inexact Rounded", c6hu);
//             TDiv("divx082 divide  9999999       1  ->  1.00000E+7 Inexact Rounded", c6hu);
//             TDiv("divx083 divide  999999        1  ->  999999", c6hu);
//             TDiv("divx084 divide  99999         1  ->  99999", c6hu);
//             TDiv("divx085 divide  9999          1  ->  9999", c6hu);
//             TDiv("divx086 divide  999           1  ->  999", c6hu);
//             TDiv("divx087 divide  99            1  ->  99", c6hu);
//             TDiv("divx088 divide  9             1  ->  9", c6hu);

//             //precision: 9
//             TDiv("divx090 divide  0.            1    ->  0", c9hu);
//             TDiv("divx091 divide  .0            1    ->  0.0", c9hu);
//             TDiv("divx092 divide  0.00          1    ->  0.00", c9hu);
//             TDiv("divx093 divide  0.00E+9       1    ->  0E+7", c9hu);
//             TDiv("divx094 divide  0.0000E-50    1    ->  0E-54", c9hu);

//             TDiv("divx095 divide  1            1E-8  ->  1E+8", c9hu);
//             TDiv("divx096 divide  1            1E-9  ->  1E+9", c9hu);
//             TDiv("divx097 divide  1            1E-10 ->  1E+10", c9hu);
//             TDiv("divx098 divide  1            1E-11 ->  1E+11", c9hu);
//             TDiv("divx099 divide  1            1E-12 ->  1E+12", c9hu);

//             TDiv("divx100 divide  1  1   -> 1", c9hu);
//             TDiv("divx101 divide  1  2   -> 0.5", c9hu);
//             TDiv("divx102 divide  1  3   -> 0.333333333 Inexact Rounded", c9hu);
//             TDiv("divx103 divide  1  4   -> 0.25", c9hu);
//             TDiv("divx104 divide  1  5   -> 0.2", c9hu);
//             TDiv("divx105 divide  1  6   -> 0.166666667 Inexact Rounded", c9hu);
//             TDiv("divx106 divide  1  7   -> 0.142857143 Inexact Rounded", c9hu);
//             TDiv("divx107 divide  1  8   -> 0.125", c9hu);
//             TDiv("divx108 divide  1  9   -> 0.111111111 Inexact Rounded", c9hu);
//             TDiv("divx109 divide  1  10  -> 0.1", c9hu);
//             TDiv("divx110 divide  1  1   -> 1", c9hu);
//             TDiv("divx111 divide  2  1   -> 2", c9hu);
//             TDiv("divx112 divide  3  1   -> 3", c9hu);
//             TDiv("divx113 divide  4  1   -> 4", c9hu);
//             TDiv("divx114 divide  5  1   -> 5", c9hu);
//             TDiv("divx115 divide  6  1   -> 6", c9hu);
//             TDiv("divx116 divide  7  1   -> 7", c9hu);
//             TDiv("divx117 divide  8  1   -> 8", c9hu);
//             TDiv("divx118 divide  9  1   -> 9", c9hu);
//             TDiv("divx119 divide  10 1   -> 10", c9hu);

//             TDiv("divx120 divide  3E+1 0.001  -> 3E+4", c9hu);
//             TDiv("divx121 divide  2.200 2     -> 1.100", c9hu);

//             TDiv("divx130 divide  12345  4.999  ->  2469.49390 Inexact Rounded", c9hu);
//             TDiv("divx131 divide  12345  4.99   ->  2473.94790 Inexact Rounded", c9hu);
//             TDiv("divx132 divide  12345  4.9    ->  2519.38776 Inexact Rounded", c9hu);
//             TDiv("divx133 divide  12345  5      ->  2469", c9hu);
//             TDiv("divx134 divide  12345  5.1    ->  2420.58824 Inexact Rounded", c9hu);
//             TDiv("divx135 divide  12345  5.01   ->  2464.07186 Inexact Rounded", c9hu);
//             TDiv("divx136 divide  12345  5.001  ->  2468.50630 Inexact Rounded", c9hu);

//             //precision:   9
//             //maxexponent: 999999999
//             //minexponent: -999999999

//             //-- test possibly imprecise results
//             TDiv("divx220 divide 391   597 ->  0.654941374 Inexact Rounded", c9hu);
//             TDiv("divx221 divide 391  -597 -> -0.654941374 Inexact Rounded", c9hu);
//             TDiv("divx222 divide -391  597 -> -0.654941374 Inexact Rounded", c9hu);
//             TDiv("divx223 divide -391 -597 ->  0.654941374 Inexact Rounded", c9hu);

//             //-- test some cases that are close to exponent overflow
//             //maxexponent: 999999999
//             //minexponent: -999999999
//             TDiv("divx270 divide 1 1e999999999    -> 1E-999999999", c9hu);
//             TDiv("divx271 divide 1 0.9e999999999  -> 1.11111111E-999999999 Inexact Rounded", c9hu);
//             TDiv("divx272 divide 1 0.99e999999999 -> 1.01010101E-999999999 Inexact Rounded", c9hu);
//             TDiv("divx273 divide 1 0.999999999e999999999 -> 1.00000000E-999999999 Inexact Rounded", c9hu);
//             TDiv("divx274 divide 9e999999999    1 -> 9E+999999999", c9hu);
//             TDiv("divx275 divide 9.9e999999999  1 -> 9.9E+999999999", c9hu);
//             TDiv("divx276 divide 9.99e999999999 1 -> 9.99E+999999999", c9hu);
//             TDiv("divx277 divide 9.99999999e999999999 1 -> 9.99999999E+999999999", c9hu);

//             TDiv("divx280 divide 0.1 9e-999999999   -> 1.11111111E+999999997 Inexact Rounded", c9hu);
//             TDiv("divx281 divide 0.1 99e-999999999  -> 1.01010101E+999999996 Inexact Rounded", c9hu);
//             TDiv("divx282 divide 0.1 999e-999999999 -> 1.00100100E+999999995 Inexact Rounded", c9hu);

//             TDiv("divx283 divide 0.1 9e-999999998     -> 1.11111111E+999999996 Inexact Rounded", c9hu);
//             TDiv("divx284 divide 0.1 99e-999999998    -> 1.01010101E+999999995 Inexact Rounded", c9hu);
//             TDiv("divx285 divide 0.1 999e-999999998   -> 1.00100100E+999999994 Inexact Rounded", c9hu);
//             TDiv("divx286 divide 0.1 999e-999999997   -> 1.00100100E+999999993 Inexact Rounded", c9hu);
//             TDiv("divx287 divide 0.1 9999e-999999997  -> 1.00010001E+999999992 Inexact Rounded", c9hu);
//             TDiv("divx288 divide 0.1 99999e-999999997 -> 1.00001000E+999999991 Inexact Rounded", c9hu);

//             //-- Divide into 0 tests

//             TDiv("divx301 divide    0    7     -> 0", c9hu);
//             TDiv("divx302 divide    0    7E-5  -> 0E+5", c9hu);
//             TDiv("divx303 divide    0    7E-1  -> 0E+1", c9hu);
//             TDiv("divx304 divide    0    7E+1  -> 0.0", c9hu);
//             TDiv("divx305 divide    0    7E+5  -> 0.00000", c9hu);
//             TDiv("divx306 divide    0    7E+6  -> 0.000000", c9hu);
//             TDiv("divx307 divide    0    7E+7  -> 0E-7", c9hu);
//             TDiv("divx308 divide    0   70E-5  -> 0E+5", c9hu);
//             TDiv("divx309 divide    0   70E-1  -> 0E+1", c9hu);
//             TDiv("divx310 divide    0   70E+0  -> 0", c9hu);
//             TDiv("divx311 divide    0   70E+1  -> 0.0", c9hu);
//             TDiv("divx312 divide    0   70E+5  -> 0.00000", c9hu);
//             TDiv("divx313 divide    0   70E+6  -> 0.000000", c9hu);
//             TDiv("divx314 divide    0   70E+7  -> 0E-7", c9hu);
//             TDiv("divx315 divide    0  700E-5  -> 0E+5", c9hu);
//             TDiv("divx316 divide    0  700E-1  -> 0E+1", c9hu);
//             TDiv("divx317 divide    0  700E+0  -> 0", c9hu);
//             TDiv("divx318 divide    0  700E+1  -> 0.0", c9hu);
//             TDiv("divx319 divide    0  700E+5  -> 0.00000", c9hu);
//             TDiv("divx320 divide    0  700E+6  -> 0.000000", c9hu);
//             TDiv("divx321 divide    0  700E+7  -> 0E-7", c9hu);
//             TDiv("divx322 divide    0  700E+77 -> 0E-77", c9hu);

//             TDiv("divx331 divide 0E-3    7E-5  -> 0E+2", c9hu);
//             TDiv("divx332 divide 0E-3    7E-1  -> 0.00", c9hu);
//             TDiv("divx333 divide 0E-3    7E+1  -> 0.0000", c9hu);
//             TDiv("divx334 divide 0E-3    7E+5  -> 0E-8", c9hu);
//             TDiv("divx335 divide 0E-1    7E-5  -> 0E+4", c9hu);
//             TDiv("divx336 divide 0E-1    7E-1  -> 0", c9hu);
//             TDiv("divx337 divide 0E-1    7E+1  -> 0.00", c9hu);
//             TDiv("divx338 divide 0E-1    7E+5  -> 0.000000", c9hu);
//             TDiv("divx339 divide 0E+1    7E-5  -> 0E+6", c9hu);
//             TDiv("divx340 divide 0E+1    7E-1  -> 0E+2", c9hu);
//             TDiv("divx341 divide 0E+1    7E+1  -> 0", c9hu);
//             TDiv("divx342 divide 0E+1    7E+5  -> 0.0000", c9hu);
//             TDiv("divx343 divide 0E+3    7E-5  -> 0E+8", c9hu);
//             TDiv("divx344 divide 0E+3    7E-1  -> 0E+4", c9hu);
//             TDiv("divx345 divide 0E+3    7E+1  -> 0E+2", c9hu);
//             TDiv("divx346 divide 0E+3    7E+5  -> 0.00", c9hu);


//             //maxexponent: 92
//             //minexponent: -92
//             //precision:    7
//             BigDecimal.Context c7hu = new BigDecimal.Context(7, BigDecimal.RoundingMode.HalfUp);

//             TDiv("divx351 divide 0E-92   7E-1  -> 0E-91", c7hu);
//             TDiv("divx352 divide 0E-92   7E+1  -> 0E-93", c7hu);
//             TDiv("divx353 divide 0E-92   7E+5  -> 0E-97", c7hu);
//             TDiv("divx354 divide 0E-92   7E+6  -> 0E-98", c7hu);
//             //TDiv("divx355 divide 0E-92   7E+7  -> 0E-98 Clamped", c7hu);
//             TDiv("divx356 divide 0E-92 777E-1  -> 0E-91", c7hu);
//             TDiv("divx357 divide 0E-92 777E+1  -> 0E-93", c7hu);
//             TDiv("divx358 divide 0E-92 777E+3  -> 0E-95", c7hu);
//             TDiv("divx359 divide 0E-92 777E+4  -> 0E-96", c7hu);
//             TDiv("divx360 divide 0E-92 777E+5  -> 0E-97", c7hu);
//             TDiv("divx361 divide 0E-92 777E+6  -> 0E-98", c7hu);
//             //TDiv("divx362 divide 0E-92 777E+7  -> 0E-98 Clamped", c7hu);
//             //TDiv("divx363 divide 0E-92   7E+92 -> 0E-98 Clamped", c7hu);

//             TDiv("divx371 divide 0E-92 700E-1  -> 0E-91", c7hu);
//             TDiv("divx372 divide 0E-92 700E+1  -> 0E-93", c7hu);
//             TDiv("divx373 divide 0E-92 700E+3  -> 0E-95", c7hu);
//             TDiv("divx374 divide 0E-92 700E+4  -> 0E-96", c7hu);
//             TDiv("divx375 divide 0E-92 700E+5  -> 0E-97", c7hu);
//             TDiv("divx376 divide 0E-92 700E+6  -> 0E-98", c7hu);
//             //TDiv("divx377 divide 0E-92 700E+7  -> 0E-98 Clamped", c7hu);

//             TDiv("divx381 divide 0E+92   7E+1  -> 0E+91", c7hu);
//             TDiv("divx382 divide 0E+92   7E+0  -> 0E+92", c7hu);
//             //TDiv("divx383 divide 0E+92   7E-1  -> 0E+92 Clamped", c7hu);
//             TDiv("divx384 divide 0E+90 777E+1  -> 0E+89", c7hu);
//             TDiv("divx385 divide 0E+90 777E-1  -> 0E+91", c7hu);
//             TDiv("divx386 divide 0E+90 777E-2  -> 0E+92", c7hu);
//             //TDiv("divx387 divide 0E+90 777E-3  -> 0E+92 Clamped", c7hu);
//             //TDiv("divx388 divide 0E+90 777E-4  -> 0E+92 Clamped", c7hu);

//             TDiv("divx391 divide 0E+90 700E+1  -> 0E+89", c7hu);
//             TDiv("divx392 divide 0E+90 700E-1  -> 0E+91", c7hu);
//             TDiv("divx393 divide 0E+90 700E-2  -> 0E+92", c7hu);
//             //TDiv("divx394 divide 0E+90 700E-3  -> 0E+92 Clamped", c7hu);
//             //TDiv("divx395 divide 0E+90 700E-4  -> 0E+92 Clamped", c7hu);

//             //-- input rounding checks
//             //maxexponent: 999
//             //minexponent: -999
//             //precision: 9
//             TDiv("divx401 divide 12345678000 1 -> 1.23456780E+10 Rounded", c9hu);
//             TDiv("divx402 divide 1 12345678000 -> 8.10000066E-11 Inexact Rounded", c9hu);
//             TDiv("divx403 divide 1234567800  1 -> 1.23456780E+9  Rounded", c9hu);
//             TDiv("divx404 divide 1 1234567800  -> 8.10000066E-10 Inexact Rounded", c9hu);
//             TDiv("divx405 divide 1234567890  1 -> 1.23456789E+9  Rounded", c9hu);
//             TDiv("divx406 divide 1 1234567890  -> 8.10000007E-10 Inexact Rounded", c9hu);
//             TDiv("divx407 divide 1234567891  1 -> 1.23456789E+9  Inexact Rounded", c9hu);
//             TDiv("divx408 divide 1 1234567891  -> 8.10000007E-10 Inexact Rounded", c9hu);
//             TDiv("divx409 divide 12345678901 1 -> 1.23456789E+10 Inexact Rounded", c9hu);
//             TDiv("divx410 divide 1 12345678901 -> 8.10000007E-11 Inexact Rounded", c9hu);
//             TDiv("divx411 divide 1234567896  1 -> 1.23456790E+9  Inexact Rounded", c9hu);
//             TDiv("divx412 divide 1 1234567896  -> 8.10000003E-10 Inexact Rounded", c9hu);
//             TDiv("divx413 divide 1 1234567897  -> 8.10000003E-10 Inexact Rounded", c9hu);
//             TDiv("divx414 divide 1 1234567898  -> 8.10000002E-10 Inexact Rounded", c9hu);
//             TDiv("divx415 divide 1 1234567899  -> 8.10000001E-10 Inexact Rounded", c9hu);
//             TDiv("divx416 divide 1 1234567900  -> 8.10000001E-10 Inexact Rounded", c9hu);
//             TDiv("divx417 divide 1 1234567901  -> 8.10000000E-10 Inexact Rounded", c9hu);
//             TDiv("divx418 divide 1 1234567902  -> 8.09999999E-10 Inexact Rounded", c9hu);
//             //-- some longies
//             TDiv("divx421 divide 1234567896.000000000000  1 -> 1.23456790E+9  Inexact Rounded", c9hu);
//             TDiv("divx422 divide 1 1234567896.000000000000  -> 8.10000003E-10 Inexact Rounded", c9hu);
//             TDiv("divx423 divide 1234567896.000000000001  1 -> 1.23456790E+9  Inexact Rounded", c9hu);
//             TDiv("divx424 divide 1 1234567896.000000000001  -> 8.10000003E-10 Inexact Rounded", c9hu);
//             TDiv("divx425 divide 1234567896.000000000000000000000000000000000000000009  1 -> 1.23456790E+9  Inexact Rounded", c9hu);
//             TDiv("divx426 divide 1 1234567896.000000000000000000000000000000000000000009  -> 8.10000003E-10 Inexact Rounded", c9hu);
//             TDiv("divx427 divide 1234567897.900010000000000000000000000000000000000009  1 -> 1.23456790E+9  Inexact Rounded", c9hu);
//             TDiv("divx428 divide 1 1234567897.900010000000000000000000000000000000000009  -> 8.10000002E-10 Inexact Rounded", c9hu);

//             //precision: 15
//             //-- still checking...
//             BigDecimal.Context c15hu = new BigDecimal.Context(15, BigDecimal.RoundingMode.HalfUp);
//             TDiv("divx441 divide 12345678000 1 -> 12345678000", c15hu);
//             TDiv("divx442 divide 1 12345678000 -> 8.10000066420005E-11 Inexact Rounded", c15hu);
//             TDiv("divx443 divide 1234567800  1 -> 1234567800", c15hu);
//             TDiv("divx444 divide 1 1234567800  -> 8.10000066420005E-10 Inexact Rounded", c15hu);
//             TDiv("divx445 divide 1234567890  1 -> 1234567890", c15hu);
//             TDiv("divx446 divide 1 1234567890  -> 8.10000007371000E-10 Inexact Rounded", c15hu);
//             TDiv("divx447 divide 1234567891  1 -> 1234567891", c15hu);
//             TDiv("divx448 divide 1 1234567891  -> 8.10000006714900E-10 Inexact Rounded", c15hu);
//             TDiv("divx449 divide 12345678901 1 -> 12345678901", c15hu);
//             TDiv("divx450 divide 1 12345678901 -> 8.10000007305390E-11 Inexact Rounded", c15hu);
//             TDiv("divx451 divide 1234567896  1 -> 1234567896", c15hu);
//             TDiv("divx452 divide 1 1234567896  -> 8.10000003434400E-10 Inexact Rounded", c15hu);

//             //-- high-lows
//             TDiv("divx453 divide 1e+1   1    ->   1E+1", c15hu);
//             TDiv("divx454 divide 1e+1   1.0  ->   1E+1", c15hu);
//             TDiv("divx455 divide 1e+1   1.00 ->   1E+1", c15hu);
//             TDiv("divx456 divide 1e+2   2    ->   5E+1", c15hu);
//             TDiv("divx457 divide 1e+2   2.0  ->   5E+1", c15hu);
//             TDiv("divx458 divide 1e+2   2.00 ->   5E+1", c15hu);

//             //-- some from IEEE discussions
//             TDiv("divx460 divide 3e0      2e0     -> 1.5", c15hu);
//             TDiv("divx461 divide 30e-1    2e0     -> 1.5", c15hu);
//             TDiv("divx462 divide 300e-2   2e0     -> 1.50", c15hu);
//             TDiv("divx464 divide 3000e-3  2e0     -> 1.500", c15hu);
//             TDiv("divx465 divide 3e0      20e-1   -> 1.5", c15hu);
//             TDiv("divx466 divide 30e-1    20e-1   -> 1.5", c15hu);
//             TDiv("divx467 divide 300e-2   20e-1   -> 1.5", c15hu);
//             TDiv("divx468 divide 3000e-3  20e-1   -> 1.50", c15hu);
//             TDiv("divx469 divide 3e0      200e-2  -> 1.5", c15hu);
//             TDiv("divx470 divide 30e-1    200e-2  -> 1.5", c15hu);
//             TDiv("divx471 divide 300e-2   200e-2  -> 1.5", c15hu);
//             TDiv("divx472 divide 3000e-3  200e-2  -> 1.5", c15hu);
//             TDiv("divx473 divide 3e0      2000e-3 -> 1.5", c15hu);
//             TDiv("divx474 divide 30e-1    2000e-3 -> 1.5", c15hu);
//             TDiv("divx475 divide 300e-2   2000e-3 -> 1.5", c15hu);
//             TDiv("divx476 divide 3000e-3  2000e-3 -> 1.5", c15hu);

//             //-- some reciprocals
//             TDiv("divx480 divide 1        1.0E+33 -> 1E-33", c15hu);
//             TDiv("divx481 divide 1        10E+33  -> 1E-34", c15hu);
//             TDiv("divx482 divide 1        1.0E-33 -> 1E+33", c15hu);
//             TDiv("divx483 divide 1        10E-33  -> 1E+32", c15hu);

//             //-- RMS discussion table
//             //maxexponent:  96
//             //minexponent: -95
//             //precision:     7

//             TDiv("divx484 divide 0e5     1e3 ->   0E+2", c7hu);
//             TDiv("divx485 divide 0e5     2e3 ->   0E+2", c7hu);
//             TDiv("divx486 divide 0e5    10e2 ->   0E+3", c7hu);
//             TDiv("divx487 divide 0e5    20e2 ->   0E+3", c7hu);
//             TDiv("divx488 divide 0e5   100e1 ->   0E+4", c7hu);
//             TDiv("divx489 divide 0e5   200e1 ->   0E+4", c7hu);

//             TDiv("divx491 divide 1e5     1e3 ->   1E+2", c7hu);
//             TDiv("divx492 divide 1e5     2e3 ->   5E+1", c7hu);
//             TDiv("divx493 divide 1e5    10e2 ->   1E+2", c7hu);
//             TDiv("divx494 divide 1e5    20e2 ->   5E+1", c7hu);
//             TDiv("divx495 divide 1e5   100e1 ->   1E+2", c7hu);
//             TDiv("divx496 divide 1e5   200e1 ->   5E+1", c7hu);


//             //-- tryzeros cases
//             //precision:   7
//             //rounding:    half_up
//             //maxExponent: 92
//             //minexponent: -92
//             //TDiv("divx497  divide  0E+86 1000E-13  -> 0E+92 Clamped", c7hu);
//             //TDiv("divx498  divide  0E-98 1000E+13  -> 0E-98 Clamped", c7hu);

//             //precision:   9
//             //rounding:    half_up
//             //maxExponent: 999
//             //minexponent: -999

//             //-- focus on trailing zeros issues
//             BigDecimal.Context c8hu = new BigDecimal.Context(8, BigDecimal.RoundingMode.HalfUp);

//             //precision:   9
//             TDiv("divx500 divide  1      9.9    ->  0.101010101  Inexact Rounded", c9hu);
//             //precision:   8
//             TDiv("divx501 divide  1      9.9    ->  0.10101010   Inexact Rounded", c8hu);
//             //precision:   7
//             TDiv("divx502 divide  1      9.9    ->  0.1010101    Inexact Rounded", c7hu);
//             //precision:   6
//             TDiv("divx503 divide  1      9.9    ->  0.101010     Inexact Rounded", c6hu);
//             //precision:   9

//             TDiv("divx511 divide 1         2    -> 0.5", c9hu);
//             TDiv("divx512 divide 1.0       2    -> 0.5", c9hu);
//             TDiv("divx513 divide 1.00      2    -> 0.50", c9hu);
//             TDiv("divx514 divide 1.000     2    -> 0.500", c9hu);
//             TDiv("divx515 divide 1.0000    2    -> 0.5000", c9hu);
//             TDiv("divx516 divide 1.00000   2    -> 0.50000", c9hu);
//             TDiv("divx517 divide 1.000000  2    -> 0.500000", c9hu);
//             TDiv("divx518 divide 1.0000000 2    -> 0.5000000", c9hu);
//             TDiv("divx519 divide 1.00      2.00 -> 0.5", c9hu);

//             TDiv("divx521 divide 2    1         -> 2", c9hu);
//             TDiv("divx522 divide 2    1.0       -> 2", c9hu);
//             TDiv("divx523 divide 2    1.00      -> 2", c9hu);
//             TDiv("divx524 divide 2    1.000     -> 2", c9hu);
//             TDiv("divx525 divide 2    1.0000    -> 2", c9hu);
//             TDiv("divx526 divide 2    1.00000   -> 2", c9hu);
//             TDiv("divx527 divide 2    1.000000  -> 2", c9hu);
//             TDiv("divx528 divide 2    1.0000000 -> 2", c9hu);
//             TDiv("divx529 divide 2.00 1.00      -> 2", c9hu);

//             TDiv("divx530 divide  2.40   2      ->  1.20", c9hu);
//             TDiv("divx531 divide  2.40   4      ->  0.60", c9hu);
//             TDiv("divx532 divide  2.40  10      ->  0.24", c9hu);
//             TDiv("divx533 divide  2.40   2.0    ->  1.2", c9hu);
//             TDiv("divx534 divide  2.40   4.0    ->  0.6", c9hu);
//             TDiv("divx535 divide  2.40  10.0    ->  0.24", c9hu);
//             TDiv("divx536 divide  2.40   2.00   ->  1.2", c9hu);
//             TDiv("divx537 divide  2.40   4.00   ->  0.6", c9hu);
//             TDiv("divx538 divide  2.40  10.00   ->  0.24", c9hu);
//             TDiv("divx539 divide  0.9    0.1    ->  9", c9hu);
//             TDiv("divx540 divide  0.9    0.01   ->  9E+1", c9hu);
//             TDiv("divx541 divide  0.9    0.001  ->  9E+2", c9hu);
//             TDiv("divx542 divide  5      2      ->  2.5", c9hu);
//             TDiv("divx543 divide  5      2.0    ->  2.5", c9hu);
//             TDiv("divx544 divide  5      2.00   ->  2.5", c9hu);
//             TDiv("divx545 divide  5      20     ->  0.25", c9hu);
//             TDiv("divx546 divide  5      20.0   ->  0.25", c9hu);
//             TDiv("divx547 divide  2.400  2      ->  1.200", c9hu);
//             TDiv("divx548 divide  2.400  2.0    ->  1.20", c9hu);
//             TDiv("divx549 divide  2.400  2.400  ->  1", c9hu);

//             TDiv("divx550 divide  240    1      ->  240", c9hu);
//             TDiv("divx551 divide  240    10     ->  24", c9hu);
//             TDiv("divx552 divide  240    100    ->  2.4", c9hu);
//             TDiv("divx553 divide  240    1000   ->  0.24", c9hu);
//             TDiv("divx554 divide  2400   1      ->  2400", c9hu);
//             TDiv("divx555 divide  2400   10     ->  240", c9hu);
//             TDiv("divx556 divide  2400   100    ->  24", c9hu);
//             TDiv("divx557 divide  2400   1000   ->  2.4", c9hu);

//             //-- +ve exponent
//             //precision: 5
//             BigDecimal.Context c5hu = new BigDecimal.Context(5, BigDecimal.RoundingMode.HalfUp);
//             TDiv("divx570 divide  2.4E+6     2  ->  1.2E+6", c5hu);
//             TDiv("divx571 divide  2.40E+6    2  ->  1.20E+6", c5hu);
//             TDiv("divx572 divide  2.400E+6   2  ->  1.200E+6", c5hu);
//             TDiv("divx573 divide  2.4000E+6  2  ->  1.2000E+6", c5hu);
//             TDiv("divx574 divide  24E+5      2  ->  1.2E+6", c5hu);
//             TDiv("divx575 divide  240E+4     2  ->  1.20E+6", c5hu);
//             TDiv("divx576 divide  2400E+3    2  ->  1.200E+6", c5hu);
//             TDiv("divx577 divide  24000E+2   2  ->  1.2000E+6", c5hu);
//             //precision: 6
//             TDiv("divx580 divide  2.4E+6     2  ->  1.2E+6", c6hu);
//             TDiv("divx581 divide  2.40E+6    2  ->  1.20E+6", c6hu);
//             TDiv("divx582 divide  2.400E+6   2  ->  1.200E+6", c6hu);
//             TDiv("divx583 divide  2.4000E+6  2  ->  1.2000E+6", c6hu);
//             TDiv("divx584 divide  24E+5      2  ->  1.2E+6", c6hu);
//             TDiv("divx585 divide  240E+4     2  ->  1.20E+6", c6hu);
//             TDiv("divx586 divide  2400E+3    2  ->  1.200E+6", c6hu);
//             TDiv("divx587 divide  24000E+2   2  ->  1.2000E+6", c6hu);
//             //precision: 7
//             TDiv("divx590 divide  2.4E+6     2  ->  1.2E+6", c7hu);
//             TDiv("divx591 divide  2.40E+6    2  ->  1.20E+6", c7hu);
//             TDiv("divx592 divide  2.400E+6   2  ->  1.200E+6", c7hu);
//             TDiv("divx593 divide  2.4000E+6  2  ->  1.2000E+6", c7hu);
//             TDiv("divx594 divide  24E+5      2  ->  1.2E+6", c7hu);
//             TDiv("divx595 divide  240E+4     2  ->  1.20E+6", c7hu);
//             TDiv("divx596 divide  2400E+3    2  ->  1.200E+6", c7hu);
//             TDiv("divx597 divide  24000E+2   2  ->  1.2000E+6", c7hu);
//             //precision:   9
//             TDiv("divx600 divide  2.4E+9     2  ->  1.2E+9", c9hu);
//             TDiv("divx601 divide  2.40E+9    2  ->  1.20E+9", c9hu);
//             TDiv("divx602 divide  2.400E+9   2  ->  1.200E+9", c9hu);
//             TDiv("divx603 divide  2.4000E+9  2  ->  1.2000E+9", c9hu);
//             TDiv("divx604 divide  24E+8      2  ->  1.2E+9", c9hu);
//             TDiv("divx605 divide  240E+7     2  ->  1.20E+9", c9hu);
//             TDiv("divx606 divide  2400E+6    2  ->  1.200E+9", c9hu);
//             TDiv("divx607 divide  24000E+5   2  ->  1.2000E+9", c9hu);


//             //-- long operand triangle
//             //precision: 33
//             TDiv("divx610 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.8131097703792 Inexact Rounded", new BigDecimal.Context(33, BigDecimal.RoundingMode.HalfUp));
//             //precision: 32
//             TDiv("divx611 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.813109770379  Inexact Rounded", new BigDecimal.Context(32, BigDecimal.RoundingMode.HalfUp));
//             //precision: 31
//             TDiv("divx612 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.81310977038   Inexact Rounded", new BigDecimal.Context(31, BigDecimal.RoundingMode.HalfUp));
//             //precision: 30
//             TDiv("divx613 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.8131097704    Inexact Rounded", new BigDecimal.Context(30, BigDecimal.RoundingMode.HalfUp));
//             //precision: 29
//             TDiv("divx614 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.813109770     Inexact Rounded", new BigDecimal.Context(29, BigDecimal.RoundingMode.HalfUp));
//             //precision: 28
//             TDiv("divx615 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.81310977      Inexact Rounded", new BigDecimal.Context(28, BigDecimal.RoundingMode.HalfUp));
//             //precision: 27
//             TDiv("divx616 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.8131098       Inexact Rounded", new BigDecimal.Context(27, BigDecimal.RoundingMode.HalfUp));
//             //precision: 26
//             TDiv("divx617 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.813110        Inexact Rounded", new BigDecimal.Context(26, BigDecimal.RoundingMode.HalfUp));
//             //precision: 25
//             TDiv("divx618 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.81311         Inexact Rounded", new BigDecimal.Context(25, BigDecimal.RoundingMode.HalfUp));
//             //precision: 24
//             TDiv("divx619 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.8131          Inexact Rounded", new BigDecimal.Context(24, BigDecimal.RoundingMode.HalfUp));
//             //precision: 23
//             TDiv("divx620 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.813           Inexact Rounded", new BigDecimal.Context(23, BigDecimal.RoundingMode.HalfUp));
//             //precision: 22
//             TDiv("divx621 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.81            Inexact Rounded", new BigDecimal.Context(22, BigDecimal.RoundingMode.HalfUp));
//             //precision: 21
//             TDiv("divx622 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817797.8             Inexact Rounded", new BigDecimal.Context(21, BigDecimal.RoundingMode.HalfUp));
//             //precision: 20
//             TDiv("divx623 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -41011408883796817798               Inexact Rounded", new BigDecimal.Context(20, BigDecimal.RoundingMode.HalfUp));
//             //precision: 19
//             TDiv("divx624 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.101140888379681780E+19         Inexact Rounded", new BigDecimal.Context(19, BigDecimal.RoundingMode.HalfUp));
//             //precision: 18
//             TDiv("divx625 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.10114088837968178E+19         Inexact Rounded", new BigDecimal.Context(18, BigDecimal.RoundingMode.HalfUp));
//             //precision: 17
//             TDiv("divx626 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.1011408883796818E+19         Inexact Rounded", new BigDecimal.Context(17, BigDecimal.RoundingMode.HalfUp));
//             //precision: 16
//             TDiv("divx627 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.101140888379682E+19         Inexact Rounded", new BigDecimal.Context(16, BigDecimal.RoundingMode.HalfUp));
//             //precision: 15
//             TDiv("divx628 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.10114088837968E+19         Inexact Rounded", new BigDecimal.Context(15, BigDecimal.RoundingMode.HalfUp));
//             //precision: 14
//             TDiv("divx629 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.1011408883797E+19         Inexact Rounded", new BigDecimal.Context(14, BigDecimal.RoundingMode.HalfUp));
//             //precision: 13
//             TDiv("divx630 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.101140888380E+19         Inexact Rounded", new BigDecimal.Context(13, BigDecimal.RoundingMode.HalfUp));
//             //precision: 12
//             TDiv("divx631 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.10114088838E+19         Inexact Rounded", new BigDecimal.Context(12, BigDecimal.RoundingMode.HalfUp));
//             //precision: 11
//             TDiv("divx632 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.1011408884E+19         Inexact Rounded", new BigDecimal.Context(11, BigDecimal.RoundingMode.HalfUp));
//             //precision: 10
//             TDiv("divx633 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.101140888E+19         Inexact Rounded", new BigDecimal.Context(10, BigDecimal.RoundingMode.HalfUp));
//             //precision:  9
//             TDiv("divx634 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.10114089E+19         Inexact Rounded", new BigDecimal.Context(9, BigDecimal.RoundingMode.HalfUp));
//             //precision:  8
//             TDiv("divx635 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.1011409E+19         Inexact Rounded", new BigDecimal.Context(8, BigDecimal.RoundingMode.HalfUp));
//             //precision:  7
//             TDiv("divx636 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.101141E+19         Inexact Rounded", new BigDecimal.Context(7, BigDecimal.RoundingMode.HalfUp));
//             //precision:  6
//             TDiv("divx637 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.10114E+19         Inexact Rounded", new BigDecimal.Context(6, BigDecimal.RoundingMode.HalfUp));
//             //precision:  5
//             TDiv("divx638 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.1011E+19         Inexact Rounded", new BigDecimal.Context(5, BigDecimal.RoundingMode.HalfUp));
//             //precision:  4
//             TDiv("divx639 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.101E+19         Inexact Rounded", new BigDecimal.Context(4, BigDecimal.RoundingMode.HalfUp));
//             //precision:  3
//             TDiv("divx640 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.10E+19         Inexact Rounded", new BigDecimal.Context(3, BigDecimal.RoundingMode.HalfUp));
//             //precision:  2
//             TDiv("divx641 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4.1E+19         Inexact Rounded", new BigDecimal.Context(2, BigDecimal.RoundingMode.HalfUp));
//             //precision:  1
//             TDiv("divx642 divide -3374988581607586061255542201048 82293895124.90045271504836568681 -> -4E+19         Inexact Rounded", new BigDecimal.Context(1, BigDecimal.RoundingMode.HalfUp));


//             //-- more zeros, etc.
//             //precision:   16
//             //rounding:    half_up
//             //maxExponent: 384
//             //minExponent: -383
//             BigDecimal.Context c16hu = new BigDecimal.Context(16, BigDecimal.RoundingMode.HalfUp);

//             TDiv("divx731 divide 5.00 1E-3    -> 5.00E+3", c16hu);

//             TDiv("divx741 divide  0    -1     -> 0", c16hu); // mod: neg zero
//             TDiv("divx742 divide -0    -1     ->  0", c16hu);
//             TDiv("divx743 divide  0     1     ->  0", c16hu);
//             TDiv("divx744 divide -0     1     -> 0", c16hu); // mod: neg zero

//             TDiv("divx751 divide  0.0  -1     -> 0.0", c16hu); // mod: neg zero
//             TDiv("divx752 divide -0.0  -1     ->  0.0", c16hu);
//             TDiv("divx753 divide  0.0   1     ->  0.0", c16hu);
//             TDiv("divx754 divide -0.0   1     -> 0.0", c16hu); // mod: neg zero

//             TDiv("divx761 divide  0    -1.0   -> 0E+1", c16hu); // mod: neg zero
//             TDiv("divx762 divide -0    -1.0   ->  0E+1", c16hu);
//             TDiv("divx763 divide  0     1.0   ->  0E+1", c16hu);
//             TDiv("divx764 divide -0     1.0   -> 0E+1", c16hu); // mod: neg zero

//             TDiv("divx771 divide  0.0  -1.0   -> 0", c16hu); // mod: neg zero
//             TDiv("divx772 divide -0.0  -1.0   ->  0", c16hu);
//             TDiv("divx773 divide  0.0   1.0   ->  0", c16hu);
//             TDiv("divx774 divide -0.0   1.0   -> 0", c16hu); // mod: neg zero

//             //precision:   34
//             //rounding:    half_up
//             //maxExponent: 6144
//             //minExponent: -6143

//             //-- Examples from SQL proposal (Krishna Kulkarni)
//             //precision: 7
//             TDiv("divx1021  divide 1E0          1E0 -> 1", c7hu);
//             TDiv("divx1022  divide 1E0          2E0 -> 0.5", c7hu);
//             TDiv("divx1023  divide 1E0          3E0 -> 0.3333333 Inexact Rounded", c7hu);
//             TDiv("divx1024  divide 100E-2   1000E-3 -> 1", c7hu);
//             TDiv("divx1025  divide 24E-1        2E0 -> 1.2", c7hu);
//             TDiv("divx1026  divide 2400E-3      2E0 -> 1.200", c7hu);
//             TDiv("divx1027  divide 5E0          2E0 -> 2.5", c7hu);
//             TDiv("divx1028  divide 5E0        20E-1 -> 2.5", c7hu);
//             TDiv("divx1029  divide 5E0      2000E-3 -> 2.5", c7hu);
//             TDiv("divx1030  divide 5E0         2E-1 -> 25", c7hu);
//             TDiv("divx1031  divide 5E0        20E-2 -> 25", c7hu);
//             TDiv("divx1032  divide 480E-2       3E0 -> 1.60", c7hu);
//             TDiv("divx1033  divide 47E-1        2E0 -> 2.35", c7hu);

//             //-- ECMAScript bad examples
//             BigDecimal.Context c7hd = new BigDecimal.Context(7, BigDecimal.RoundingMode.HalfDown);
//             BigDecimal.Context c7he = new BigDecimal.Context(7, BigDecimal.RoundingMode.HalfEven);

//             //rounding:    half_down
//             //precision: 7
//             TDiv("divx1050  divide 5 9  -> 0.5555556 Inexact Rounded", c7hd);
//             //rounding:    half_even
//             TDiv("divx1051  divide 5 11 -> 0.4545455 Inexact Rounded", c7he);
//         }

//         [Test]
//         public void TestBadDivides()
//         {
//             BigDecimal.Context c16hu = new BigDecimal.Context(16, BigDecimal.RoundingMode.HalfUp);

//             TDivEx("divx732 divide 00.00 0.000  -> NaN Division_undefined", c16hu);
//             TDivEx("divx733 divide 00.00 0E-3   -> NaN Division_undefined", c16hu);
//             TDivEx("divx734 divide  0    -0     -> NaN Division_undefined", c16hu);
//             TDivEx("divx735 divide -0     0     -> NaN Division_undefined", c16hu);
//             TDivEx("divx736 divide -0    -0     -> NaN Division_undefined", c16hu);
//             TDivEx("divx745 divide -1     0     -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx746 divide -1    -0     ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx747 divide  1     0     ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx748 divide  1    -0     -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx755 divide -1.0   0     -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx756 divide -1.0  -0     ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx757 divide  1.0   0     ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx758 divide  1.0  -0     -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx765 divide -1     0.0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx766 divide -1    -0.0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx767 divide  1     0.0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx768 divide  1    -0.0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx775 divide -1.0   0.0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx776 divide -1.0  -0.0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx777 divide  1.0   0.0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx778 divide  1.0  -0.0   -> -Infinity Division_by_zero ", c16hu);

//             //-- Various flavours of divide by 0
//             TDivEx("divx901 divide    0       0   ->  NaN Division_undefined", c16hu);
//             TDivEx("divx902 divide    0.0E5   0   ->  NaN Division_undefined", c16hu);
//             TDivEx("divx903 divide    0.000   0   ->  NaN Division_undefined", c16hu);
//             TDivEx("divx904 divide    0.0001  0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx905 divide    0.01    0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx906 divide    0.1     0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx907 divide    1       0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx908 divide    1       0.0 ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx909 divide   10       0.0 ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx910 divide   1E+100   0.0 ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx911 divide   1E+1000  0   ->  Infinity Division_by_zero", c16hu);

//             TDivEx("divx921 divide   -0.0001  0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx922 divide   -0.01    0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx923 divide   -0.1     0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx924 divide   -1       0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx925 divide   -1       0.0 -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx926 divide  -10       0.0 -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx927 divide  -1E+100   0.0 -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx928 divide  -1E+1000  0   -> -Infinity Division_by_zero", c16hu);

//             TDivEx("divx931 divide    0.0001 -0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx932 divide    0.01   -0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx933 divide    0.1    -0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx934 divide    1      -0   -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx935 divide    1      -0.0 -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx936 divide   10      -0.0 -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx937 divide   1E+100  -0.0 -> -Infinity Division_by_zero", c16hu);
//             TDivEx("divx938 divide   1E+1000 -0   -> -Infinity Division_by_zero", c16hu);

//             TDivEx("divx941 divide   -0.0001 -0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx942 divide   -0.01   -0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx943 divide   -0.1    -0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx944 divide   -1      -0   ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx945 divide   -1      -0.0 ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx946 divide  -10      -0.0 ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx947 divide  -1E+100  -0.0 ->  Infinity Division_by_zero", c16hu);
//             TDivEx("divx948 divide  -1E+1000 -0   ->  Infinity Division_by_zero", c16hu);
//         }

//         [Test]
//         public void DivideTestNoContext()
//         {
//             BigDecimal.Context c0u = new BigDecimal.Context(0, BigDecimal.RoundingMode.Unnecessary);

//             //-- sanity checks
//             TDiv("divx001 divide  1     1    ->  1", c0u);
//             TDiv("divx002 divide  2     1    ->  2", c0u);
//             TDiv("divx003 divide  1     2    ->  0.5", c0u);
//             TDiv("divx004 divide  2     2    ->  1", c0u);
//             TDiv("divx005 divide  0     1    ->  0", c0u);
//             TDiv("divx006 divide  0     2    ->  0", c0u);
//             TDivEx("divx007 divide  1     3    ->  0.333333333 Inexact Rounded", c0u);
//             TDivEx("divx008 divide  2     3    ->  0.666666667 Inexact Rounded", c0u);
//             TDiv("divx009 divide  3     3    ->  1", c0u);

//             TDiv("divx010 divide  2.4   1    ->  2.4", c0u);
//             TDiv("divx011 divide  2.4   -1   ->  -2.4", c0u);
//         }

//         static void TDivEx(string test, BigDecimal.Context c)
//         {
//             try
//             {
//                 GetThreeArgs(test, out string arg1Str, out string arg2Str, out string resultStr);
//                 TestDivide(arg1Str, arg2Str, c, "1");  // result irrelevant
//                 Expect(false);
//             }
//             catch (ArithmeticException)
//             {
//                 Expect(true);
//             }
//         }

//         static void TDiv(string test, BigDecimal.Context c)
//         {
//             GetThreeArgs(test, out string arg1Str, out string arg2Str, out string resultStr);
//             TestDivide(arg1Str, arg2Str, c, resultStr);
//         }


//         static void TestDivide(string arg1Str, string arg2Str, BigDecimal.Context c, string resultStr)
//         {
//             BigDecimal arg1 = BigDecimal.Parse(arg1Str);
//             BigDecimal arg2 = BigDecimal.Parse(arg2Str);
//             BigDecimal val = arg1.Divide(arg2, c);
//             string valStr = val.ToScientificString();
//             Expect(valStr).To.Equal(resultStr);
//         }


//         #endregion

//         #region DivideInteger tests

//         [Test]
//         public void SpecTestDivInt()
//         {
//             BigDecimal.Context c9hu = new BigDecimal.Context(9, BigDecimal.RoundingMode.HalfUp);
//             //extended:    1
//             //precision:   9
//             //rounding:    half_up
//             //maxExponent: 384
//             //minexponent: -383

//             TDivI("dvix001 divideint  1     1    ->  1", c9hu);
//             TDivI("dvix002 divideint  2     1    ->  2", c9hu);
//             TDivI("dvix003 divideint  1     2    ->  0", c9hu);
//             TDivI("dvix004 divideint  2     2    ->  1", c9hu);
//             TDivI("dvix005 divideint  0     1    ->  0", c9hu);
//             TDivI("dvix006 divideint  0     2    ->  0", c9hu);
//             TDivI("dvix007 divideint  1     3    ->  0", c9hu);
//             TDivI("dvix008 divideint  2     3    ->  0", c9hu);
//             TDivI("dvix009 divideint  3     3    ->  1", c9hu);

//             TDivI("dvix010 divideint  2.4   1    ->  2", c9hu);
//             TDivI("dvix011 divideint  2.4   -1   ->  -2", c9hu);
//             TDivI("dvix012 divideint  -2.4  1    ->  -2", c9hu);
//             TDivI("dvix013 divideint  -2.4  -1   ->  2", c9hu);
//             TDivI("dvix014 divideint  2.40  1    ->  2", c9hu);
//             TDivI("dvix015 divideint  2.400 1    ->  2", c9hu);
//             TDivI("dvix016 divideint  2.4   2    ->  1", c9hu);
//             TDivI("dvix017 divideint  2.400 2    ->  1", c9hu);
//             TDivI("dvix018 divideint  2.    2    ->  1", c9hu);
//             TDivI("dvix019 divideint  20    20   ->  1", c9hu);

//             TDivI("dvix020 divideint  187   187  ->  1", c9hu);
//             TDivI("dvix021 divideint  5     2    ->  2", c9hu);
//             TDivI("dvix022 divideint  5     2.0    ->  2", c9hu);
//             TDivI("dvix023 divideint  5     2.000  ->  2", c9hu);
//             TDivI("dvix024 divideint  5     0.200  ->  25", c9hu);
//             TDivI("dvix025 divideint  5     0.200  ->  25", c9hu);

//             TDivI("dvix030 divideint  1     2      ->  0", c9hu);
//             TDivI("dvix031 divideint  1     4      ->  0", c9hu);
//             TDivI("dvix032 divideint  1     8      ->  0", c9hu);
//             TDivI("dvix033 divideint  1     16     ->  0", c9hu);
//             TDivI("dvix034 divideint  1     32     ->  0", c9hu);
//             TDivI("dvix035 divideint  1     64     ->  0", c9hu);
//             TDivI("dvix040 divideint  1    -2      -> 0", c9hu); // mod: neg zero
//             TDivI("dvix041 divideint  1    -4      -> 0", c9hu); // mod: neg zero
//             TDivI("dvix042 divideint  1    -8      -> 0", c9hu); // mod: neg zero
//             TDivI("dvix043 divideint  1    -16     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix044 divideint  1    -32     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix045 divideint  1    -64     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix050 divideint -1     2      -> 0", c9hu); // mod: neg zero
//             TDivI("dvix051 divideint -1     4      -> 0", c9hu); // mod: neg zero
//             TDivI("dvix052 divideint -1     8      -> 0", c9hu); // mod: neg zero
//             TDivI("dvix053 divideint -1     16     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix054 divideint -1     32     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix055 divideint -1     64     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix060 divideint -1    -2      ->  0", c9hu);
//             TDivI("dvix061 divideint -1    -4      ->  0", c9hu);
//             TDivI("dvix062 divideint -1    -8      ->  0", c9hu);
//             TDivI("dvix063 divideint -1    -16     ->  0", c9hu);
//             TDivI("dvix064 divideint -1    -32     ->  0", c9hu);
//             TDivI("dvix065 divideint -1    -64     ->  0", c9hu);

//             //-- similar with powers of ten
//             TDivI("dvix160 divideint  1     1         ->  1", c9hu);
//             TDivI("dvix161 divideint  1     10        ->  0", c9hu);
//             TDivI("dvix162 divideint  1     100       ->  0", c9hu);
//             TDivI("dvix163 divideint  1     1000      ->  0", c9hu);
//             TDivI("dvix164 divideint  1     10000     ->  0", c9hu);
//             TDivI("dvix165 divideint  1     100000    ->  0", c9hu);
//             TDivI("dvix166 divideint  1     1000000   ->  0", c9hu);
//             TDivI("dvix167 divideint  1     10000000  ->  0", c9hu);
//             TDivI("dvix168 divideint  1     100000000 ->  0", c9hu);
//             TDivI("dvix170 divideint  1    -1         -> -1", c9hu);
//             TDivI("dvix171 divideint  1    -10        -> 0", c9hu); // mod: neg zero
//             TDivI("dvix172 divideint  1    -100       -> 0", c9hu); // mod: neg zero
//             TDivI("dvix173 divideint  1    -1000      -> 0", c9hu); // mod: neg zero
//             TDivI("dvix174 divideint  1    -10000     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix175 divideint  1    -100000    -> 0", c9hu); // mod: neg zero
//             TDivI("dvix176 divideint  1    -1000000   -> 0", c9hu); // mod: neg zero
//             TDivI("dvix177 divideint  1    -10000000  -> 0", c9hu); // mod: neg zero
//             TDivI("dvix178 divideint  1    -100000000 -> 0", c9hu); // mod: neg zero
//             TDivI("dvix180 divideint -1     1         -> -1", c9hu);
//             TDivI("dvix181 divideint -1     10        -> 0", c9hu); // mod: neg zero
//             TDivI("dvix182 divideint -1     100       -> 0", c9hu); // mod: neg zero
//             TDivI("dvix183 divideint -1     1000      -> 0", c9hu); // mod: neg zero
//             TDivI("dvix184 divideint -1     10000     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix185 divideint -1     100000    -> 0", c9hu); // mod: neg zero
//             TDivI("dvix186 divideint -1     1000000   -> 0", c9hu); // mod: neg zero
//             TDivI("dvix187 divideint -1     10000000  -> 0", c9hu); // mod: neg zero
//             TDivI("dvix188 divideint -1     100000000 -> 0", c9hu); // mod: neg zero
//             TDivI("dvix190 divideint -1    -1         ->  1", c9hu);
//             TDivI("dvix191 divideint -1    -10        ->  0", c9hu);
//             TDivI("dvix192 divideint -1    -100       ->  0", c9hu);
//             TDivI("dvix193 divideint -1    -1000      ->  0", c9hu);
//             TDivI("dvix194 divideint -1    -10000     ->  0", c9hu);
//             TDivI("dvix195 divideint -1    -100000    ->  0", c9hu);
//             TDivI("dvix196 divideint -1    -1000000   ->  0", c9hu);
//             TDivI("dvix197 divideint -1    -10000000  ->  0", c9hu);
//             TDivI("dvix198 divideint -1    -100000000 ->  0", c9hu);

//             //-- some long operand cases here
//             TDivI("dvix070 divideint  999999999     1  ->  999999999", c9hu);
//             TDivI("dvix071 divideint  999999999.4   1  ->  999999999", c9hu);
//             TDivI("dvix072 divideint  999999999.5   1  ->  999999999", c9hu);
//             TDivI("dvix073 divideint  999999999.9   1  ->  999999999", c9hu);
//             TDivI("dvix074 divideint  999999999.999 1  ->  999999999", c9hu);
//             //precision: 6
//             BigDecimal.Context c6hu = new BigDecimal.Context(6, BigDecimal.RoundingMode.HalfUp);

//             TDivIEx("dvix080 divideint  999999999     1  ->  NaN Division_impossible", c6hu);
//             TDivIEx("dvix081 divideint  99999999      1  ->  NaN Division_impossible", c6hu);
//             TDivIEx("dvix082 divideint  9999999       1  ->  NaN Division_impossible", c6hu);
//             TDivI("dvix083 divideint  999999        1  ->  999999", c6hu);
//             TDivI("dvix084 divideint  99999         1  ->  99999", c6hu);
//             TDivI("dvix085 divideint  9999          1  ->  9999", c6hu);
//             TDivI("dvix086 divideint  999           1  ->  999", c6hu);
//             TDivI("dvix087 divideint  99            1  ->  99", c6hu);
//             TDivI("dvix088 divideint  9             1  ->  9", c6hu);

//             //precision: 9
//             TDivI("dvix090 divideint  0.            1    ->  0", c9hu);
//             TDivI("dvix091 divideint  .0            1    ->  0", c9hu);
//             TDivI("dvix092 divideint  0.00          1    ->  0", c9hu);
//             TDivI("dvix093 divideint  0.00E+9       1    ->  0", c9hu);
//             TDivI("dvix094 divideint  0.0000E-50    1    ->  0", c9hu);

//             TDivI("dvix100 divideint  1  1   -> 1", c9hu);
//             TDivI("dvix101 divideint  1  2   -> 0", c9hu);
//             TDivI("dvix102 divideint  1  3   -> 0", c9hu);
//             TDivI("dvix103 divideint  1  4   -> 0", c9hu);
//             TDivI("dvix104 divideint  1  5   -> 0", c9hu);
//             TDivI("dvix105 divideint  1  6   -> 0", c9hu);
//             TDivI("dvix106 divideint  1  7   -> 0", c9hu);
//             TDivI("dvix107 divideint  1  8   -> 0", c9hu);
//             TDivI("dvix108 divideint  1  9   -> 0", c9hu);
//             TDivI("dvix109 divideint  1  10  -> 0", c9hu);
//             TDivI("dvix110 divideint  1  1   -> 1", c9hu);
//             TDivI("dvix111 divideint  2  1   -> 2", c9hu);
//             TDivI("dvix112 divideint  3  1   -> 3", c9hu);
//             TDivI("dvix113 divideint  4  1   -> 4", c9hu);
//             TDivI("dvix114 divideint  5  1   -> 5", c9hu);
//             TDivI("dvix115 divideint  6  1   -> 6", c9hu);
//             TDivI("dvix116 divideint  7  1   -> 7", c9hu);
//             TDivI("dvix117 divideint  8  1   -> 8", c9hu);
//             TDivI("dvix118 divideint  9  1   -> 9", c9hu);
//             TDivI("dvix119 divideint  10 1   -> 10", c9hu);

//             //-- from DiagBigDecimal
//             TDivI("dvix131 divideint  101.3   1     ->  101", c9hu);
//             TDivI("dvix132 divideint  101.0   1     ->  101", c9hu);
//             TDivI("dvix133 divideint  101.3   3     ->  33", c9hu);
//             TDivI("dvix134 divideint  101.0   3     ->  33", c9hu);
//             TDivI("dvix135 divideint  2.4     1     ->  2", c9hu);
//             TDivI("dvix136 divideint  2.400   1     ->  2", c9hu);
//             TDivI("dvix137 divideint  18      18    ->  1", c9hu);
//             TDivI("dvix138 divideint  1120    1000  ->  1", c9hu);
//             TDivI("dvix139 divideint  2.4     2     ->  1", c9hu);
//             TDivI("dvix140 divideint  2.400   2     ->  1", c9hu);
//             TDivI("dvix141 divideint  0.5     2.000 ->  0", c9hu);
//             TDivI("dvix142 divideint  8.005   7     ->  1", c9hu);
//             TDivI("dvix143 divideint  5       2     ->  2", c9hu);
//             TDivI("dvix144 divideint  0       2     ->  0", c9hu);
//             TDivI("dvix145 divideint  0.00    2     ->  0", c9hu);

//             //-- Others
//             TDivI("dvix150 divideint  12345  4.999  ->  2469", c9hu);
//             TDivI("dvix151 divideint  12345  4.99   ->  2473", c9hu);
//             TDivI("dvix152 divideint  12345  4.9    ->  2519", c9hu);
//             TDivI("dvix153 divideint  12345  5      ->  2469", c9hu);
//             TDivI("dvix154 divideint  12345  5.1    ->  2420", c9hu);
//             TDivI("dvix155 divideint  12345  5.01   ->  2464", c9hu);
//             TDivI("dvix156 divideint  12345  5.001  ->  2468", c9hu);
//             TDivI("dvix157 divideint    101  7.6    ->  13", c9hu);

//             //-- Various flavours of divideint by 0
//             //maxexponent: 999999999
//             //minexponent: -999999999
//             TDivIEx("dvix201 divideint  0      0   -> NaN Division_undefined", c9hu);
//             TDivIEx("dvix202 divideint  0.0E5  0   -> NaN Division_undefined", c9hu);
//             TDivIEx("dvix203 divideint  0.000  0   -> NaN Division_undefined", c9hu);
//             TDivIEx("dvix204 divideint  0.0001 0   -> Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix205 divideint  0.01   0   -> Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix206 divideint  0.1    0   -> Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix207 divideint  1      0   -> Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix208 divideint  1      0.0 -> Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix209 divideint 10      0.0 -> Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix210 divideint 1E+100  0.0 -> Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix211 divideint 1E+1000 0   -> Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix214 divideint  -0.0001 0   -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix215 divideint  -0.01   0   -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix216 divideint  -0.1    0   -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix217 divideint  -1      0   -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix218 divideint  -1      0.0 -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix219 divideint -10      0.0 -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix220 divideint -1E+100  0.0 -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix221 divideint -1E+1000 0   -> -Infinity Division_by_zero", c9hu);

//             // TODO: Reactivate these tests when we can handle the align (irrelevant when the numbers are so different)

//             // -- test some cases that are close to exponent overflow
//             //maxexponent: 999999999
//             //minexponent: -999999999
//             //TDivI("dvix270 divideint 1 1e999999999    -> 0", c9hu);
//             //TDivI("dvix271 divideint 1 0.9e999999999  -> 0", c9hu);
//             //TDivI("dvix272 divideint 1 0.99e999999999 -> 0", c9hu);
//             //TDivI("dvix273 divideint 1 0.999999999e999999999 -> 0", c9hu);
//             //TDivIEx("dvix274 divideint 9e999999999    1       -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix275 divideint 9.9e999999999  1       -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix276 divideint 9.99e999999999 1       -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix277 divideint 9.99999999e999999999 1 -> NaN Division_impossible", c9hu);

//             //TDivIEx("dvix280 divideint 0.1 9e-999999999       -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix281 divideint 0.1 99e-999999999      -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix282 divideint 0.1 999e-999999999     -> NaN Division_impossible", c9hu);

//             //TDivIEx("dvix283 divideint 0.1 9e-999999998       -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix284 divideint 0.1 99e-999999998      -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix285 divideint 0.1 999e-999999998     -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix286 divideint 0.1 999e-999999997     -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix287 divideint 0.1 9999e-999999997    -> NaN Division_impossible", c9hu);
//             //TDivIEx("dvix288 divideint 0.1 99999e-999999997   -> NaN Division_impossible", c9hu);

//             //-- GD edge cases: lhs smaller than rhs but more digits
//             TDivI("dvix301  divideint  0.9      2      ->  0", c9hu);
//             TDivI("dvix302  divideint  0.9      2.0    ->  0", c9hu);
//             TDivI("dvix303  divideint  0.9      2.1    ->  0", c9hu);
//             TDivI("dvix304  divideint  0.9      2.00   ->  0", c9hu);
//             TDivI("dvix305  divideint  0.9      2.01   ->  0", c9hu);
//             TDivI("dvix306  divideint  0.12     1      ->  0", c9hu);
//             TDivI("dvix307  divideint  0.12     1.0    ->  0", c9hu);
//             TDivI("dvix308  divideint  0.12     1.00   ->  0", c9hu);
//             TDivI("dvix309  divideint  0.12     1.0    ->  0", c9hu);
//             TDivI("dvix310  divideint  0.12     1.00   ->  0", c9hu);
//             TDivI("dvix311  divideint  0.12     2      ->  0", c9hu);
//             TDivI("dvix312  divideint  0.12     2.0    ->  0", c9hu);
//             TDivI("dvix313  divideint  0.12     2.1    ->  0", c9hu);
//             TDivI("dvix314  divideint  0.12     2.00   ->  0", c9hu);
//             TDivI("dvix315  divideint  0.12     2.01   ->  0", c9hu);

//             //-- overflow and underflow tests [from divide]
//             //maxexponent: 999999999
//             //minexponent: -999999999
//             //TDivI("dvix330 divideint +1.23456789012345E-0 9E+999999999    -> 0", c9hu);
//             //TDivIEx("dvix331 divideint 9E+999999999 +0.23456789012345E-0 -> NaN Division_impossible", c9hu);
//             //TDivI("dvix332 divideint +0.100 9E+999999999    -> 0", c9hu);
//             //TDivI("dvix333 divideint 9E-999999999 +9.100    -> 0", c9hu);
//             //TDivI("dvix335 divideint -1.23456789012345E-0 9E+999999999    -> -0", c9hu);
//             //TDivIEx("dvix336 divideint 9E+999999999 -0.83456789012345E-0 -> NaN Division_impossible", c9hu);
//             //TDivI("dvix337 divideint -0.100 9E+999999999    -> -0", c9hu);
//             //TDivI("dvix338 divideint 9E-999999999 -9.100    -> -0", c9hu);

//             //-- long operand checks
//             //maxexponent: 999
//             //minexponent: -999
//             //precision: 9
//             TDivI("dvix401 divideint 12345678000 100 -> 123456780", c9hu);
//             TDivI("dvix402 divideint 1 12345678000   -> 0", c9hu);
//             TDivI("dvix403 divideint 1234567800  10  -> 123456780", c9hu);
//             TDivI("dvix404 divideint 1 1234567800    -> 0", c9hu);
//             TDivI("dvix405 divideint 1234567890  10  -> 123456789", c9hu);
//             TDivI("dvix406 divideint 1 1234567890    -> 0", c9hu);
//             TDivI("dvix407 divideint 1234567891  10  -> 123456789", c9hu);
//             TDivI("dvix408 divideint 1 1234567891    -> 0", c9hu);
//             TDivI("dvix409 divideint 12345678901 100 -> 123456789", c9hu);
//             TDivI("dvix410 divideint 1 12345678901   -> 0", c9hu);
//             TDivI("dvix411 divideint 1234567896  10  -> 123456789", c9hu);
//             TDivI("dvix412 divideint 1 1234567896    -> 0", c9hu);
//             TDivI("dvix413 divideint 12345678948 100 -> 123456789", c9hu);
//             TDivI("dvix414 divideint 12345678949 100 -> 123456789", c9hu);
//             TDivI("dvix415 divideint 12345678950 100 -> 123456789", c9hu);
//             TDivI("dvix416 divideint 12345678951 100 -> 123456789", c9hu);
//             TDivI("dvix417 divideint 12345678999 100 -> 123456789", c9hu);

//             //precision: 15
//             BigDecimal.Context c15hu = new BigDecimal.Context(15, BigDecimal.RoundingMode.HalfUp);

//             TDivI("dvix441 divideint 12345678000 1 -> 12345678000", c15hu);
//             TDivI("dvix442 divideint 1 12345678000 -> 0", c15hu);
//             TDivI("dvix443 divideint 1234567800  1 -> 1234567800", c15hu);
//             TDivI("dvix444 divideint 1 1234567800  -> 0", c15hu);
//             TDivI("dvix445 divideint 1234567890  1 -> 1234567890", c15hu);
//             TDivI("dvix446 divideint 1 1234567890  -> 0", c9hu);
//             TDivI("dvix447 divideint 1234567891  1 -> 1234567891", c15hu);
//             TDivI("dvix448 divideint 1 1234567891  -> 0", c9hu);
//             TDivI("dvix449 divideint 12345678901 1 -> 12345678901", c15hu);
//             TDivI("dvix450 divideint 1 12345678901 -> 0", c15hu);
//             TDivI("dvix451 divideint 1234567896  1 -> 1234567896", c15hu);
//             TDivI("dvix452 divideint 1 1234567896  -> 0", c15hu);

//             //precision:   9
//             //rounding:    half_up
//             //maxExponent: 999
//             //minexponent: -999

//             //-- more zeros, etc.
//             TDivI("dvix531 divideint 5.00 1E-3    -> 5000", c9hu);
//             TDivIEx("dvix532 divideint 00.00 0.000  -> NaN Division_undefined", c9hu);
//             TDivIEx("dvix533 divideint 00.00 0E-3   -> NaN Division_undefined", c9hu);
//             TDivIEx("dvix534 divideint  0    -0     -> NaN Division_undefined", c9hu);
//             TDivIEx("dvix535 divideint -0     0     -> NaN Division_undefined", c9hu);
//             TDivIEx("dvix536 divideint -0    -0     -> NaN Division_undefined", c9hu);

//             TDivI("dvix541 divideint  0    -1     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix542 divideint -0    -1     ->  0", c9hu);
//             TDivI("dvix543 divideint  0     1     ->  0", c9hu);
//             TDivI("dvix544 divideint -0     1     -> 0", c9hu); // mod: neg zero
//             TDivIEx("dvix545 divideint -1     0     -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix546 divideint -1    -0     ->  Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix547 divideint  1     0     ->  Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix548 divideint  1    -0     -> -Infinity Division_by_zero", c9hu);

//             TDivI("dvix551 divideint  0.0  -1     -> 0", c9hu); // mod: neg zero
//             TDivI("dvix552 divideint -0.0  -1     ->  0", c9hu);
//             TDivI("dvix553 divideint  0.0   1     ->  0", c9hu);
//             TDivI("dvix554 divideint -0.0   1     -> 0", c9hu); // mod: neg zero
//             TDivIEx("dvix555 divideint -1.0   0     -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix556 divideint -1.0  -0     ->  Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix557 divideint  1.0   0     ->  Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix558 divideint  1.0  -0     -> -Infinity Division_by_zero", c9hu);

//             TDivI("dvix561 divideint  0    -1.0   -> 0", c9hu); // mod: neg zero
//             TDivI("dvix562 divideint -0    -1.0   ->  0", c9hu);
//             TDivI("dvix563 divideint  0     1.0   ->  0", c9hu);
//             TDivI("dvix564 divideint -0     1.0   -> 0", c9hu); // mod: neg zero
//             TDivIEx("dvix565 divideint -1     0.0   -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix566 divideint -1    -0.0   ->  Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix567 divideint  1     0.0   ->  Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix568 divideint  1    -0.0   -> -Infinity Division_by_zero", c9hu);

//             TDivI("dvix571 divideint  0.0  -1.0   -> 0", c9hu); // mod: neg zero
//             TDivI("dvix572 divideint -0.0  -1.0   ->  0", c9hu);
//             TDivI("dvix573 divideint  0.0   1.0   ->  0", c9hu);
//             TDivI("dvix574 divideint -0.0   1.0   -> 0", c9hu); // mod: neg zero
//             TDivIEx("dvix575 divideint -1.0   0.0   -> -Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix576 divideint -1.0  -0.0   ->  Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix577 divideint  1.0   0.0   ->  Infinity Division_by_zero", c9hu);
//             TDivIEx("dvix578 divideint  1.0  -0.0   -> -Infinity Division_by_zero", c9hu);

//         }

//         static void TDivI(string test, BigDecimal.Context c)
//         {
//             GetThreeArgs(test, out string arg1Str, out string arg2Str, out string resultStr);
//             TestDivideInt(arg1Str, arg2Str, c, resultStr);
//         }


//         static void TestDivideInt(string arg1Str, string arg2Str, BigDecimal.Context c, string resultStr)
//         {
//             BigDecimal arg1 = BigDecimal.Parse(arg1Str);
//             BigDecimal arg2 = BigDecimal.Parse(arg2Str);
//             BigDecimal val = arg1.DivideInteger(arg2, c);
//             string valStr = val.ToScientificString();
//             Expect(valStr).To.Equal(resultStr);
//         }

//         static void TDivIEx(string test, BigDecimal.Context c)
//         {
//             try
//             {
//                 GetThreeArgs(test, out string arg1Str, out string arg2Str, out string resultStr);
//                 BigDecimal arg1 = BigDecimal.Parse(arg1Str);
//                 BigDecimal arg2 = BigDecimal.Parse(arg2Str);
//                 arg1.DivideInteger(arg2, c);
//                 Expect(false);
//             }
//             catch (ArithmeticException )
//             {
//                 Expect(true);
//             }
//         }


//         #endregion

//         #region Power tests

//         [Test]
//         public void PowerTestsFromSpec()
//         {
//             BigDecimal.Context c9he = new BigDecimal.Context(9, BigDecimal.RoundingMode.HalfEven);
//             BigDecimal.Context c10he = new BigDecimal.Context(10, BigDecimal.RoundingMode.HalfEven);
//             BigDecimal.Context c16he = new BigDecimal.Context(16, BigDecimal.RoundingMode.HalfEven);
//             //extended:    1
//             //precision:   16
//             //rounding:    half_even
//             //maxExponent: 384
//             //minExponent: -383

//             //-- base checks.  Note 0**0 is an error.
//             //TPow("powx001 power    '0'  '0'         -> NaN Invalid_operation
//             TPow("powx002 power    '0'  '1'         -> '0'", c16he);
//             TPow("powx003 power    '0'  '2'         -> '0'", c16he);
//             TPow("powx004 power    '1'  '0'         -> '1'", c16he);
//             TPow("powx005 power    '1'  '1'         -> '1'", c16he);
//             TPow("powx006 power    '1'  '2'         -> '1'", c16he);

//             TPow("powx010 power    '2'  '0'         -> '1'", c16he);
//             TPow("powx011 power    '2'  '1'         -> '2'", c16he);
//             TPow("powx012 power    '2'  '2'         -> '4'", c16he);
//             TPow("powx013 power    '2'  '3'         -> '8'", c16he);
//             TPow("powx014 power    '2'  '4'         -> '16'", c16he);
//             TPow("powx015 power    '2'  '5'         -> '32'", c16he);
//             TPow("powx016 power    '2'  '6'         -> '64'", c16he);
//             TPow("powx017 power    '2'  '7'         -> '128'", c16he);
//             TPow("powx018 power    '2'  '8'         -> '256'", c16he);
//             TPow("powx019 power    '2'  '9'         -> '512'", c16he);
//             TPow("powx020 power    '2'  '10'        -> '1024'", c16he);
//             TPow("powx021 power    '2'  '11'        -> '2048'", c16he);
//             TPow("powx022 power    '2'  '12'        -> '4096'", c16he);
//             TPow("powx023 power    '2'  '15'        -> '32768'", c16he);
//             TPow("powx024 power    '2'  '16'        -> '65536'", c16he);
//             TPow("powx025 power    '2'  '31'        -> '2147483648'", c16he);
//             //-- NB 0 not stripped in next
//             TPow("powx026 power    '2'  '32'        -> '4294967296'", c16he);

//             //precision: 9
//             TPow("powx027 power    '2'  '31'        -> '2.14748365E+9' Inexact Rounded", c9he);
//             //-- NB 0 not stripped in next
//             TPow("powx028 power    '2'  '32'        -> '4.29496730E+9' Inexact Rounded", c9he);
//             //precision: 10
//             TPow("powx029 power    '2'  '31'        -> '2147483648'", c10he);
//             TPow("powx030 power    '2'  '32'        -> '4294967296'", c10he);
//             //precision: 9

//             TPow("powx031 power    '3'  '2'         -> 9", c9he);
//             TPow("powx032 power    '4'  '2'         -> 16", c9he);
//             TPow("powx033 power    '5'  '2'         -> 25", c9he);
//             TPow("powx034 power    '6'  '2'         -> 36", c9he);
//             TPow("powx035 power    '7'  '2'         -> 49", c9he);
//             TPow("powx036 power    '8'  '2'         -> 64", c9he);
//             TPow("powx037 power    '9'  '2'         -> 81", c9he);
//             TPow("powx038 power    '10' '2'         -> 100", c9he);
//             TPow("powx039 power    '11' '2'         -> 121", c9he);
//             TPow("powx040 power    '12' '2'         -> 144", c9he);

//             TPow("powx041 power    '3'  '3'         -> 27", c9he);
//             TPow("powx042 power    '4'  '3'         -> 64", c9he);
//             TPow("powx043 power    '5'  '3'         -> 125", c9he);
//             TPow("powx044 power    '6'  '3'         -> 216", c9he);
//             TPow("powx045 power    '7'  '3'         -> 343", c9he);
//             TPow("powx047 power   '-3'  '3'         -> -27", c9he);
//             TPow("powx048 power   '-4'  '3'         -> -64", c9he);
//             TPow("powx049 power   '-5'  '3'         -> -125", c9he);
//             TPow("powx050 power   '-6'  '3'         -> -216", c9he);
//             TPow("powx051 power   '-7'  '3'         -> -343", c9he);

//             TPow("powx052 power   '10'  '0'         -> 1", c9he);
//             TPow("powx053 power   '10'  '1'         -> 10", c9he);
//             TPow("powx054 power   '10'  '2'         -> 100", c9he);
//             TPow("powx055 power   '10'  '3'         -> 1000", c9he);
//             TPow("powx056 power   '10'  '4'         -> 10000", c9he);
//             TPow("powx057 power   '10'  '5'         -> 100000", c9he);
//             TPow("powx058 power   '10'  '6'         -> 1000000", c9he);
//             TPow("powx059 power   '10'  '7'         -> 10000000", c9he);
//             TPow("powx060 power   '10'  '8'         -> 100000000", c9he);
//             TPow("powx061 power   '10'  '9'         -> 1.00000000E+9 Rounded", c9he);
//             TPow("powx062 power   '10'  '22'        -> 1.00000000E+22 Rounded", c9he);
//             TPow("powx063 power   '10'  '77'        -> 1.00000000E+77 Rounded", c9he);
//             TPow("powx064 power   '10'  '99'        -> 1.00000000E+99 Rounded", c9he);

//             TPow("powx070 power  '0.3'  '0'           -> '1'", c9he);
//             TPow("powx071 power  '0.3'  '1'           -> '0.3'", c9he);
//             //TPow("powx072 power  '0.3'  '1.00'        -> '0.3'", c9he);
//             //TPow("powx073 power  '0.3'  '2.00'        -> '0.09'", c9he);
//             //TPow("powx074 power  '0.3'  '2.000000000' -> '0.09'", c9he);
//             TPow("powx075 power  '6.0'  '1'           -> '6.0'     -- NB zeros not stripped", c9he);
//             TPow("powx076 power  '6.0'  '2'           -> '36.00'   -- ..", c9he);
//             TPow("powx077 power   '-3'  '2'           -> '9'       -- from NetRexx book", c9he);
//             TPow("powx078 power    '4'  '3'           -> '64'      -- .. (sort of)", c9he);

//             TPow("powx080 power   0.1    0            -> 1", c9he);
//             TPow("powx081 power   0.1    1            -> 0.1", c9he);
//             TPow("powx082 power   0.1    2            -> 0.01", c9he);
//             TPow("powx083 power   0.1    3            -> 0.001", c9he);
//             TPow("powx084 power   0.1    4            -> 0.0001", c9he);
//             TPow("powx085 power   0.1    5            -> 0.00001", c9he);
//             TPow("powx086 power   0.1    6            -> 0.000001", c9he);
//             TPow("powx087 power   0.1    7            -> 1E-7", c9he);
//             TPow("powx088 power   0.1    8            -> 1E-8", c9he);
//             TPow("powx089 power   0.1    9            -> 1E-9", c9he);

//             TPow("powx090 power   101    2            -> 10201", c9he);
//             TPow("powx091 power   101    3            -> 1030301", c9he);
//             TPow("powx092 power   101    4            -> 104060401", c9he);
//             TPow("powx093 power   101    5            -> 1.05101005E+10 Inexact Rounded", c9he);
//             TPow("powx094 power   101    6            -> 1.06152015E+12 Inexact Rounded", c9he);
//             TPow("powx095 power   101    7            -> 1.07213535E+14 Inexact Rounded", c9he);

//             //-- negative powers
//             TPow("powx099 power  '1'  '-1'    -> 1", c9he);
//             TPow("powx100 power  '3'  '-1'    -> 0.333333333 Inexact Rounded", c9he);
//             TPow("powx101 power  '2'  '-1'    -> 0.5", c9he);
//             TPow("powx102 power  '2'  '-2'    -> 0.25", c9he);
//             TPow("powx103 power  '2'  '-4'    -> 0.0625", c9he);
//             TPow("powx104 power  '2'  '-8'    -> 0.00390625", c9he);
//             TPow("powx105 power  '2'  '-16'   -> 0.0000152587891 Inexact Rounded", c9he);
//             TPow("powx106 power  '2'  '-32'   -> 2.32830644E-10 Inexact Rounded", c9he);
//             TPow("powx108 power  '2'  '-64'   -> 5.42101086E-20 Inexact Rounded", c9he);
//             TPow("powx110 power  '10'  '-8'   -> 1E-8", c9he);
//             TPow("powx111 power  '10'  '-7'   -> 1E-7", c9he);
//             TPow("powx112 power  '10'  '-6'   -> 0.000001", c9he);
//             TPow("powx113 power  '10'  '-5'   -> 0.00001", c9he);
//             TPow("powx114 power  '10'  '-4'   -> 0.0001", c9he);
//             TPow("powx115 power  '10'  '-3'   -> 0.001", c9he);
//             TPow("powx116 power  '10'  '-2'   -> 0.01", c9he);
//             TPow("powx117 power  '10'  '-1'   -> 0.1", c9he);
//             TPow("powx121 power  '10'  '-77'  -> '1E-77'", c9he);
//             TPow("powx122 power  '10'  '-22'  -> '1E-22'", c9he);

//             TPow("powx123 power   '2'  '-1'   -> '0.5'", c9he);
//             TPow("powx124 power   '2'  '-2'   -> '0.25'", c9he);
//             TPow("powx125 power   '2'  '-4'   -> '0.0625'", c9he);

//             //TPow("powx126 power   '0'  '-1'   -> Infinity", c9he);
//             //TPow("powx127 power   '0'  '-2'   -> Infinity", c9he);
//             //TPow("powx128 power   -0   '-1'   -> -Infinity", c9he);
//             //TPow("powx129 power   -0   '-2'   -> Infinity", c9he);
//         }

//         static void TPow(string test, BigDecimal.Context c)
//         {
//             GetThreeArgs(test, out string arg1Str, out string arg2Str, out string resultStr);
//             TestPower(arg1Str, arg2Str, c, resultStr);
//         }

//         static  void TestPower(string arg1Str, string arg2Str, BigDecimal.Context c, string resultStr)
//         {
//             BigDecimal arg1 = BigDecimal.Parse(arg1Str);
//             int arg2 = Int32.Parse(arg2Str);
//             BigDecimal val = arg1.Power(arg2, c);
//             string valStr = val.ToScientificString();
//             Expect(valStr).To.Equal(resultStr);
//         }
//         #endregion

//         #region MovePoint tests

//         [Test]
//         public void TestMovePoint()
//         {
//             TestMoveLeft("123456789000", 0, "123456789000");
//             TestMoveLeft("123456789000", 1, "12345678900.0");
//             TestMoveLeft("123456789000", 2, "1234567890.00");
//             TestMoveLeft("123456789000", 3, "123456789.000");
//             TestMoveLeft("123456789000", 4, "12345678.9000");
//             TestMoveLeft("123456789000", 5, "1234567.89000");
//             TestMoveLeft("123456789000", 6, "123456.789000");
//             TestMoveLeft("123456789000", 7, "12345.6789000");
//             TestMoveLeft("123456789000", 8, "1234.56789000");
//             TestMoveLeft("123456789000", 9, "123.456789000");
//             TestMoveLeft("123456789000", 10, "12.3456789000");
//             TestMoveLeft("123456789000", 11, "1.23456789000");
//             TestMoveLeft("123456789000", 12, "0.123456789000");
//             TestMoveLeft("123456789000", 13, "0.0123456789000");
//             TestMoveLeft("123456789000", 14, "0.00123456789000");
//             TestMoveLeft("123456789000", 15, "0.000123456789000");
//             TestMoveLeft("123456789000", 16, "0.0000123456789000");
//             TestMoveLeft("123456789000", 17, "0.00000123456789000");

//             TestMoveLeft("123456789000", -1, "1.23456789000E+12");
//             TestMoveLeft("123456789000", -2, "1.23456789000E+13");
//             TestMoveLeft("123456789000", -3, "1.23456789000E+14");
//             TestMoveLeft("123456789000", -4, "1.23456789000E+15");

//             TestMoveLeft("123.456", 0, "123.456");
//             TestMoveLeft("123.456", 1, "12.3456");
//             TestMoveLeft("123.456", 2, "1.23456");
//             TestMoveLeft("123.456", 3, "0.123456");
//             TestMoveLeft("123.456", 4, "0.0123456");
//             TestMoveLeft("123.456", 5, "0.00123456");
//             TestMoveLeft("123.456", 6, "0.000123456");
//             TestMoveLeft("123.456", -1, "1234.56");
//             TestMoveLeft("123.456", -2, "12345.6");
//             TestMoveLeft("123.456", -3, "123456");
//             TestMoveLeft("123.456", -4, "1.23456E+6");
//             TestMoveLeft("123.456", -5, "1.23456E+7");
//             TestMoveLeft("123.456", -6, "1.23456E+8");

//             TestMoveRight("123.456", 0, "123.456");
//             TestMoveRight("123.456", -1, "12.3456");
//             TestMoveRight("123.456", -2, "1.23456");
//             TestMoveRight("123.456", -3, "0.123456");
//             TestMoveRight("123.456", -4, "0.0123456");
//             TestMoveRight("123.456", -5, "0.00123456");
//             TestMoveRight("123.456", -6, "0.000123456");
//             TestMoveRight("123.456", 1, "1234.56");
//             TestMoveRight("123.456", 2, "12345.6");
//             TestMoveRight("123.456", 3, "123456");
//             TestMoveRight("123.456", 4, "1.23456E+6");
//             TestMoveRight("123.456", 5, "1.23456E+7");
//             TestMoveRight("123.456", 6, "1.23456E+8");

//             TestMoveRight("123456789000", 0, "123456789000");
//             TestMoveRight("123456789000", -1, "12345678900.0");
//             TestMoveRight("123456789000", -2, "1234567890.00");
//             TestMoveRight("123456789000", -3, "123456789.000");
//             TestMoveRight("123456789000", -4, "12345678.9000");

//             TestMoveRight("123456789000", 1, "1.23456789000E+12");
//             TestMoveRight("123456789000", 2, "1.23456789000E+13");
//             TestMoveRight("123456789000", 3, "1.23456789000E+14");
//             TestMoveRight("123456789000", 4, "1.23456789000E+15");
//         }

//         static void TestMoveLeft(string strVal, int n, string expectedString)
//         {
//             BigDecimal d = BigDecimal.Parse(strVal);
//             BigDecimal m = d.MovePointLeft(n);
//             string resultStr = m.ToScientificString();
//             Expect(resultStr).To.Equal(expectedString);
//         }


//         static void TestMoveRight(string strVal, int n, string expectedString)
//         {
//             BigDecimal d = BigDecimal.Parse(strVal);
//             BigDecimal m = d.MovePointRight(n);
//             string resultStr = m.ToScientificString();
//             Expect(resultStr).To.Equal(expectedString);
//         }

//         #endregion
//     }
//}