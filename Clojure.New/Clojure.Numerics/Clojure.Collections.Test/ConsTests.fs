module ConsTests

open Expecto
open Clojure.Collections


[<Tests>]
let boundedLengthTests = 
    testList "ConsTests" [

        testCase "No-meta ctor has no meta" <| fun _ ->
            let c = Cons("abc",null)
            let m = (c:>IMeta).meta() 
            Expect.isNull m "Meta should be null"

        //testCase "Ctor w/ meta has meta" <| fun _ ->
        //    let m = Simple
        //    let c = Cons()



        ]