module BigRationalTests

open Expecto
open Clojure.Numerics
open System.Numerics
open System

[<Tests>]
let primaryConstructorTests =
    ftestList "normal form from basic constructor" [

        testCase "Same BigIntegers if GDC = 1" <| fun _ ->
            let n = BigInteger(10)
            let d = BigInteger(7)
            let r = BigRational(n,d)
            Expect.equal r.Numerator n "should have same numerator"
            Expect.equal r.Denominator d "should have same denominator"

        testCase "Reduced BigIntegers if GDC <> 1" <| fun _ ->
            let n = BigInteger(4*3*5)
            let d = BigInteger(2*5*7*9)
            let nr = BigInteger(2)
            let dr = BigInteger(3*7)
            let r = BigRational(n,d)
            Expect.equal r.Numerator nr "should have reduced numerator"
            Expect.equal r.Denominator dr "should have reduced denominator"


        testCase "Should have -/+ given +/- in numerator" <| fun _ ->
            let n = BigInteger(10)
            let d = BigInteger(77)
            let nn = BigInteger.Negate(n)
            let dn = BigInteger.Negate(d)
            let r1 = BigRational(n,dn)
            Expect.equal r1.Numerator nn  "Numerator should be negative"
            Expect.equal r1.Denominator d "Denominator should be positive"

        testCase "Should have -/+ given -/+ in numerator" <| fun _ ->
            let n = BigInteger(10)
            let d = BigInteger(77)
            let nn = BigInteger.Negate(n)
            let dn = BigInteger.Negate(d)
            let r1 = BigRational(nn,d)    
            Expect.equal r1.Numerator nn  "Numerator should be negative"
            Expect.equal r1.Denominator d "Denominator should be positive"

        testCase "Should have +/+ given -/- in numerator" <| fun _ ->
            let n = BigInteger(10)
            let d = BigInteger(77)
            let nn = BigInteger.Negate(n)
            let dn = BigInteger.Negate(d)
            let r1 = BigRational(nn,dn)
            Expect.equal r1.Numerator n  "Numerator should be positive"
            Expect.equal r1.Denominator d "Denominator should be positive"

        testCase "Should fail if denominator is zero" <| fun _ ->
            Expect.throwsT<DivideByZeroException> (fun f -> BigRational(BigInteger.One,BigInteger.Zero) |> ignore) "Fail if denominator is zero"
                
        ]



