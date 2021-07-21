module ConsTests

open Expecto
open Clojure.Collections
open System
open SimpleCollections


let makeConsChain (n:int) = 
    let rec step c i =
        if i = n then c else step (Cons(i,c)) (i+1)
    step (Cons(0,null)) 1


[<Tests>]
let consTests = 
    testList "ConsTests" [

        testCase "No-meta ctor has no meta" <| fun _ ->
            let c = Cons("abc",null)
            let m = (c:>IMeta).meta() 
            Expect.isNull m "Meta should be null"

        testCase "Ctor w/ meta has meta" <| fun _ ->
            let m = SimpleMap.makeSimpleMap 3
            let c = Cons(m,"abc",null)
            Expect.isTrue (Object.ReferenceEquals((c:>IMeta).meta(),m)) "Should get back same meta as put in"

        testCase "Cons.count works" <| fun _ ->
            for i = 1 to 5 do
                Expect.equal ((makeConsChain i :>IPersistentCollection).count()) i "Count value"

        testCase "Cons.seq returns self" <| fun _ ->
            let c = makeConsChain 3
            Expect.isTrue (Object.ReferenceEquals((c:>ISeq).seq(),c)) "Should be self"
            

        testCase "Cons.empty is empty" <| fun _ ->
            let c = makeConsChain 3
            let e = (c:>IPersistentCollection).empty()
            Expect.equal (e.GetType()) (PersistentList.Empty.GetType()) "Empty should be an EmptyList"

        testCase "Cons.Equals" <| fun _ ->
            let c1 = makeConsChain 5
            let c2 = makeConsChain 5
            let c3 = makeConsChain 4
            let c4 = Cons(10,Cons(12,makeConsChain 4))
            let c5 = Cons(10,Cons(11,makeConsChain 4))
            Expect.isTrue (c1.Equals(c1)) "should equal itself"
            Expect.isTrue (c1.Equals(c2)) "should equal same sequence"
            Expect.isFalse (c1.Equals(c3)) "should not equal smaller sequence"
            Expect.isFalse (c3.Equals(c1)) "should not equal larger sequence"
            Expect.isFalse (c4.Equals(c5)) "should not equal sequence with non-matching entry"

        testCase "Cons.GetHashCode" <| fun _ ->
            let c1 = makeConsChain 5
            let c2 = makeConsChain 5
            let c3 = makeConsChain 4
            let c4 = Cons(3,Cons(4,makeConsChain 3))

            Expect.equal (c1.GetHashCode()) (c2.GetHashCode()) "Hash is on value"
            Expect.notEqual (c1.GetHashCode()) (c3.GetHashCode())  "Hash depends on content"
            Expect.notEqual (c1.GetHashCode()) (c4.GetHashCode()) "Hash depends on order"
            


        ]