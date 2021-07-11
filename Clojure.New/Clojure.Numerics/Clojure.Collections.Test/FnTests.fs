module FnTests

open Expecto
open Clojure.Collections
open Clojure.Fn



type SimpleCons(h,t) =
    let first : obj = h
    let more : ISeq =  t

    static member seqEquiv (s1:ISeq) (s2:ISeq) =
        match s1, s2 with   
        | null, null -> true
        | null, _ -> false
        | _, null -> false
        | _ -> (s1.first() = null && s2.first() = null || s1.first() <> null && s1.first().Equals(s2.first())) && SimpleCons.seqEquiv (s1.next()) (s2.next())

    interface ISeq with
        member x.first() = first
        member x.next() = (x:>ISeq).more().seq()
        member x.more() = if more = null then (SimpleEmptySeq() :> ISeq) else more
        member x.cons(o) = SimpleCons(o,x) :> ISeq

    interface IPersistentCollection with
        member x.count() = 1+ (if more = null then 0 else more.count())
        member x.cons(o) = SimpleCons(o,x) :> IPersistentCollection
        member _.empty() = SimpleEmptySeq() :> IPersistentCollection
        member x.equiv(o) =
            match o with
            | :? Seqable as s -> SimpleCons.seqEquiv (x:>ISeq) (s.seq())
            | _ -> false

    interface Seqable with
        member x.seq() = (x:>ISeq)
   
and  SimpleEmptySeq() =

    interface ISeq with 
        member _.first() = null
        member _.next() = null
        member x.more() = x :> ISeq
        member x.cons(o) = SimpleCons(o,x) :> ISeq 

    interface IPersistentCollection with
        member _.count() = 0
        member x.cons(o) = SimpleCons(o,x) :> IPersistentCollection
        member x.empty() = x:>IPersistentCollection
        member x.equiv(o) = 
            match o with
            | :? Seqable as s -> SimpleCons.seqEquiv (x:>ISeq) (s.seq())
            | _ -> false

    interface Seqable with
        member x.seq() = (x:>ISeq)
       

// Make a super-simple Range sequence to implement ISeq
[<AllowNullLiteral>]
type SimpleRange(s,e) =
    let startVal : int = s
    let endVal : int = s

    interface ISeq with
        member x.first() = upcast startVal 
        member x.next() = if startVal = endVal  then null else SimpleRange(startVal+1,endVal):>ISeq
        member x.more() = 
            let s = (x:>ISeq).next()
            if s = null then (SimpleEmptySeq():>ISeq) else s
        member x.cons(o) = SimpleCons(o,(x:>ISeq)) :> ISeq

    interface IPersistentCollection with
        member _.count() = endVal-startVal+1
        member x.cons(o) =  SimpleCons(o,(x:>ISeq)) :> IPersistentCollection
        member _.empty() = SimpleEmptySeq() :> IPersistentCollection
        member x.equiv(o) = 
            match o with
            | :? Seqable as s -> SimpleCons.seqEquiv (x:>ISeq) (s.seq())
            | _ -> false

    interface Seqable with
        member x.seq() = (x:>ISeq)

[<Tests>]
let emptySeqTests = 
    testList "emptyseq" [
        
        testCase "emptyseq implements ISeq" <| fun _ ->
            let es = SimpleEmptySeq() :> ISeq
            Expect.isNull (es.first()) "emptyseq.first() should be null"
            Expect.isNull (es.next()) "emptyseq.next() should be null"
            Expect.equal (es.more()) es "emptyseq.more should be itself"
            
            let c = es.cons(1)
            Expect.equal (c.first()) (upcast 1) "emptyseq.cons.first should be new item"
            Expect.equal (c.next()) es "emptyseq.cons.next should be itself"


        testCase "emptyseq implements IPersistentCollection" <| fun _ ->
            let es = SimpleEmptySeq() :> IPersistentCollection
            Expect.equal (es.count()) 0 "emptyseq.count should be 0"
            Expect.equal (es.empty()) es "emptyseq.empty should be itself"
            Expect.isTrue (es.equiv(es)) "emptyseq should be equiv of itself"
            Expect.isFalse (es.equiv(es.cons(1))) "emtpyseq should not be equive to a seq with an element"

        testCase "emptyseq implements Seqable" <| fun _ ->
             let es = SimpleEmptySeq() 
             Expect.equal ((es:>Seqable).seq()) (upcast es) "emptyseq.seq should be itself"
        
    ]

let makeConsSeq (n:int) =
    let mutable (c:ISeq) = SimpleEmptySeq() :> ISeq
    for i = n-1 downto 0 do
        c <- c.cons(i)
    c

[<Tests>]
let consTests = 
    testList "conses" [
        
        testCase "cons implements ISeq" <| fun _ ->
            let c = makeConsSeq 4 
            let mutable mc = c
            for i = 0 to 3 do
                Expect.equal (mc.first()) (upcast i) "ith element of c should be i"
                mc <- mc.next()
            
            let c2 = c.cons(10)
            Expect.equal (c2.first()) (upcast 10) "cons.cons.first should be new item"
            Expect.equal (c2.next()) c "cons.cons.next should be itself"


        testCase "cons implements IPersistentCollection" <| fun _ ->
            let c = makeConsSeq 4 
            Expect.equal (c.count()) 4 "cons.count should be item count"
            Expect.equal (c.empty().GetType()) typeof<SimpleEmptySeq> "cons.empty should be an emptySeq"
            Expect.isTrue (c.equiv(c)) "cons should be equiv of itself"
            Expect.isFalse (c.equiv(c.next()))  "cons should not be equiv of its next"

        testCase "cons implements Seqable" <| fun _ ->
             let c = makeConsSeq 4 
             Expect.equal ((c:>Seqable).seq()) (upcast c) "cons.seq should be itself"
        
    ]

//[<Tests>]
//let tests =
//  testList "samples" [
//    testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
//      let subject = true
//      Expect.isTrue subject "I compute, therefore I am."

//    testCase "when true is not (should fail)" <| fun _ ->
//      let subject = false
//      Expect.isTrue subject "I should fail because the subject is false"

//    testCase "I'm skipped (should skip)" <| fun _ ->
//      Tests.skiptest "Yup, waiting for a sunny day..."

//    testCase "I'm always fail (should fail)" <| fun _ ->
//      Tests.failtest "This was expected..."

//    testCase "contains things" <| fun _ ->
//      Expect.containsAll [| 2; 3; 4 |] [| 2; 4 |]
//                         "This is the case; {2,3,4} contains {2,4}"

//    testCase "contains things (should fail)" <| fun _ ->
//      Expect.containsAll [| 2; 3; 4 |] [| 2; 4; 1 |]
//                         "Expecting we have one (1) in there"

//    testCase "Sometimes I want to ༼ノಠل͟ಠ༽ノ ︵ ┻━┻" <| fun _ ->
//      Expect.equal "abcdëf" "abcdef" "These should equal"

//    test "I am (should fail)" {
//      "╰〳 ಠ 益 ಠೃ 〵╯" |> Expect.equal true false
//    }
//  ]
