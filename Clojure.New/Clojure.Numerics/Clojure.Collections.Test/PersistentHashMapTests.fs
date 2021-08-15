module PersistentHashMapTests

open Expecto
open Clojure.Collections
open TestHelpers
open System
open System.Collections.Generic
open System.Collections


// TODO:  many of tese tests are identical to those for PersistentHashMapTests.
// Figure out how to consolidate.
// I've marked the non-duplicate ones with a comment.


[<Tests>]
let basicPersistentHashMapTests =
    testList "Basic PersistentHashMap tests" [
        
        testCase "Create on empty dictionary returns empty map" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            let m = PersistentHashMap.createKV(d)

            Expect.equal (m.count()) 0 "Empty map should have 0 count"

        testCase "Create on non-empty dictionary creates correct map" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentHashMap.createKV(d)

            Expect.equal (m.count()) 2 "Count should match # dict entries"
            Expect.equal (m.valAt(1)) (upcast "a") "m[1]=a"
            Expect.equal (m.valAt(2)) (upcast "b") "m[2]=b"
            Expect.isTrue (m.containsKey(1)) "Check containsKey"
            Expect.isFalse (m.containsKey(3)) "Shouldn't contain some random key"


        // Non-duplicate
        testCase "Create on empty list returns empty map" <| fun _ ->
            let a = ArrayList()
            let m = PersistentHashMap.create1(a) :> IPersistentMap

            Expect.equal (m.count()) 0 "Empty map should have 0 count"

        // Non-duplicate
        testCase "Create on non-empty list returns non-empty map" <| fun _ ->
            let items : obj[] = [|1; "a"; 2; "b"|]
            let a = ArrayList(items)
            let m = PersistentHashMap.create1(a) :> IPersistentMap

            Expect.equal (m.count()) 2 "Count should match # dict entries"
            Expect.equal (m.valAt(1)) (upcast "a") "m[1]=a"
            Expect.equal (m.valAt(2)) (upcast "b") "m[2]=b"
            Expect.isTrue (m.containsKey(1)) "Check containsKey"
            Expect.isFalse (m.containsKey(3)) "Shouldn't contain some random key"

        // Non-duplicate
        testCase "Create on empty ISeq returns empty map" <| fun _ ->
            let items : obj[] = Array.empty
            let a = ArrayList(items)
            let s = PersistentList.create(a).seq()
            let m = PersistentHashMap.create(s) :> IPersistentMap

            Expect.equal (m.count()) 0 "Empty map should have 0 count"

        // Non-duplicate
        testCase "Create on ISeq returns map" <| fun _ ->
            let items : obj[] = [|1; "a"; 2; "b"|]
            let a = ArrayList(items)
            let s = PersistentList.create(a).seq()
            let m = PersistentHashMap.create(s) :> IPersistentMap

            Expect.equal (m.count()) 2 "Count should match # dict entries"
            Expect.equal (m.valAt(1)) (upcast "a") "m[1]=a"
            Expect.equal (m.valAt(2)) (upcast "b") "m[2]=b"
            Expect.isTrue (m.containsKey(1)) "Check containsKey"
            Expect.isFalse (m.containsKey(3)) "Shouldn't contain some random key"

        // Non-duplicate
        testCase "Create on no args return empty map" <| fun _ ->
            let m = PersistentHashMap.create() :> IPersistentMap

            Expect.equal (m.count()) 0 "Empty map should have 0 count"
            Expect.isNull ((m:?>IMeta).meta()) "Empty map should have no meta"

        // Non-duplicate
        testCase "Create on args returns map" <| fun _ ->
            let m = PersistentHashMap.create(1, "a", 2 ,"b") :> IPersistentMap

            Expect.equal (m.count()) 2 "Count should match # dict entries"
            Expect.equal (m.valAt(1)) (upcast "a") "m[1]=a"
            Expect.equal (m.valAt(2)) (upcast "b") "m[2]=b"
            Expect.isFalse (m.containsKey(3)) "Shouldn't contain some random key"







        ]
