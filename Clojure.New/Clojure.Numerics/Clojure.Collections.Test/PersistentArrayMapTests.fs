module PersistentArrayMapTests

open Expecto
open Clojure.Collections
open TestHelpers
open System
open System.Collections.Generic

[<Tests>]
let basicPersistentArrayMapTests =
    testList "Basic PersistentArrayMap tests" [
        
        testCase "Create on empty dictionary returns empty map" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            let m = PersistentArrayMap.create(d)

            Expect.equal (m.count()) 0 "Empty map should have 0 count"

        testCase "Create on non-empty dictionary creates correct map" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)

            Expect.equal (m.count()) 2 "Count should match # dict entries"
            Expect.equal (m.valAt(1)) (upcast "a") "m[1]=a"
            Expect.equal (m.valAt(2)) (upcast "b") "m[2]=b"
            Expect.isTrue (m.containsKey(1)) "Check containsKey"
            Expect.isFalse (m.containsKey(3)) "Shouldn't contain some random key"
        ]

[<Tests>]
let basicPersistentArrayMapAssocTests =
    testList "Basic P.A.M Assoc tests" [
    
        testCase "containsKey on missing key fails" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)

            Expect.isFalse (m.containsKey(3)) "Should not contain key"

        testCase "containsKey on present key succeeds" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)

            Expect.isTrue (m.containsKey(1)) "Should contain key"
            Expect.isTrue (m.containsKey(2)) "Should contain key"

        testCase "containsKey not confused by a value" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)

            Expect.isFalse (m.containsKey("a")) "Should not see value as a key"

        testCase "entryAt returns null on missing key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)

            Expect.isNull (m.entryAt(3)) "Should have null entryAt"

            
        testCase "entryAt returns proper entry for existing key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)
            let me = m.entryAt(1)

            Expect.equal (me.key()) (upcast 1) "Should be the key"
            Expect.equal (me.value()) (upcast "a") "Should be the value" 


        testCase "valAt returns null on missing key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)

            Expect.isNull (m.valAt(3)) "Should have null valAt"

        
        testCase "valAt returns value on existing key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)

            Expect.equal (m.valAt(1)) (upcast "a") "Should have correct value"

        
        testCase "valAt2 returns notFound on missing  key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)

            Expect.equal (m.valAt(3,99)) (upcast 99) "Should have not-found value"


        testCase "valAt2 returns value on existing key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)

            Expect.equal (m.valAt(1,99)) (upcast "a") "Should have correct value"
    ]

[<Tests>]
let basicPersistentArrayMapPersistentCollectionTests =
    testList "Basic P.A.M PersistentCollection tests" [
        
        testCase "count on empty is 0" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)
            let c = m.empty()

            Expect.equal (c.count()) 0 "Empty.count() = 0"

        testCase "count on non-empty returns count of entries" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)
            
            Expect.equal (m.count()) 2 "Count of keys"

        testCase "seq on empty is null" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)
            let c = m.empty()

            Expect.isNull (c.seq()) "Seq on empty should be null"
                    
        testCase "seq on non-empty iterates" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m = PersistentArrayMap.create(d)
            let s = m.seq()
            let me1 = s.first() :?> IMapEntry
            let me2 = s.next().first() :?> IMapEntry
            let last = s.next().next()

            Expect.equal (s.count()) 2 "COunt of seq should be # of entries in map"
            Expect.equal (me1.value()) (m.valAt(me1.key()))  "K/V pair should match map"
            Expect.equal (me2.value()) (m.valAt(me2.key()))  "K/V pair should match map"
            Expect.notEqual (me1.key()) (me2.key()) "Should see different keys"
            Expect.isNull last "end of seq should be null"   
    
    ]


[<Tests>]
let basicPersistentArrayMapPersistentMapTests =
    testList "Basic P.A.M PersistentMap tests" [

        testCase "assoc modifies value for existing key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m1 = PersistentArrayMap.create(d)
            let m2 = m1.assoc(2,"c")

            Expect.equal (m1.count()) 2 "Original map count unchanged"
            Expect.equal (m1.valAt(2)) (upcast "b")  "Original map value unchanged"
            Expect.equal (m2.count()) 2 "Count unchanged"
            Expect.equal (m2.valAt(2)) (upcast "c") "New map has updated value"

        testCase "assoc adds on new key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m1 = PersistentArrayMap.create(d)
            let m2 = m1.assoc(3,"c")

            Expect.equal (m1.count()) 2 "Original map count unchanged"
            Expect.isFalse (m1.containsKey(3))  "Original map does not have new key"
            Expect.equal (m2.count()) 3 "new map has Count update"
            Expect.equal (m2.valAt(3)) (upcast "c") "New map has new key/value"

        testCase "assocEx failes on exising key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m1 = PersistentArrayMap.create(d)
            let f() = m1.assocEx(2,"c") |> ignore

            Expect.throwsT<InvalidOperationException> f "AssocEx throws on existing key"
        
        testCase "assocEx adds on new key" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()
            d.[1] <- "a"
            d.[2] <- "b"

            let m1 = PersistentArrayMap.create(d)
            let m2 = m1.assocEx(3,"c")

            Expect.equal (m1.count()) 2 "Original map count unchanged"
            Expect.isFalse (m1.containsKey(3))  "Original map does not have new key"
            Expect.equal (m2.count()) 3 "new map has Count update"
            Expect.equal (m2.valAt(3)) (upcast "c") "New map has new key/value"

        testCase "without on existing key removes it" <| fun _ ->
            let d : Dictionary<int,string> = Dictionary()

            d.[3] <- "a";
            d.[5] <- "b";
            d.[7] <- "c";

            let m1 = PersistentArrayMap.create(d);
            let m2 = m1.without(5);

            Expect.equal (m1.count()) 3 "Original map has original count"
            Expect.equal (m1.valAt(5)) (upcast "b")  "original map still has original key/val"
            Expect.equal (m2.count()) 2 "without reduces count"
            Expect.isFalse (m2.containsKey(5)) "without removes key/val"

        testCase "without on missing key returns original" <| fun _ ->
             let d : Dictionary<int,string> = Dictionary()

             d.[3] <- "a";
             d.[5] <- "b";
             d.[7] <- "c";

             let m1 = PersistentArrayMap.create(d);
             let m2 = m1.without(4);
             
             Expect.isTrue (m1=m2) "No change"

    ]


        