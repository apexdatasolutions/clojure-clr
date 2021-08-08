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




        ]