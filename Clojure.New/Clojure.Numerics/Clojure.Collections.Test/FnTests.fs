module FnTests

open Expecto
open Clojure.Collections
open Clojure.Fn


// I went to this degree of elaboration so I could use this as a general example of implementation of a sequence for tutorial purposes

module Util =

    let checkEquals o1 o2 = obj.ReferenceEquals(o1,o2) || o1 <> null && o1.Equals(o2)
    let rec seqEquals (s1:ISeq) (s2:ISeq) =
        match s1, s2 with   
        | null, null -> true
        | null, _ -> false
        | _, null -> false
        | _ ->  checkEquals (s1.first()) (s2.first()) && seqEquals (s1.next()) (s2.next())
    let seqEquiv s1 s2 = seqEquals s1 s2
    let seqCount (s:ISeq) = 
        let rec step (s:ISeq) cnt = if s = null then cnt else step (s.next()) (cnt+1)
        step s 0
    let getHashCode (s:ISeq) = 
        let combine hc x = 31*hc + if x = null then 0 else x.GetHashCode()
        let rec step (s:ISeq) hc = if s = null then hc else step (s.next()) (combine hc (s.first()))
        step s 1
    let rec seqToString (s:ISeq) = 
        let itemToString (o:obj) =
            match o with
            | :? Seqable as s -> seqToString (s.seq())
            | _ -> o.ToString()
        let rec itemsToString (s:ISeq) =
            if s = null then "" else (itemToString (s.first())) + (itemsToString (s.next()))  
        if s = null then "nil" else "(" + (itemsToString s) + ")"        

type SimpleCons(h,t) =
    let head : obj = h      // I had to restrain myself from calling these car & cdr
    let tail : ISeq =  t
 
    interface ISeq with
        member x.first() = head
        member x.next() = (x:>ISeq).more().seq()
        member x.more() =  if tail <> null then tail else (SimpleEmptySeq() :> ISeq)
        member x.cons(o) = SimpleCons(o,x) :> ISeq

    interface IPersistentCollection with
        member x.count() = 1 + Util.seqCount tail
        member x.cons(o) = (x:>ISeq).cons(o) :> IPersistentCollection
        member _.empty() = SimpleEmptySeq() :> IPersistentCollection
        member x.equiv(o) =
            match o with
            | :? Seqable as s -> Util.seqEquiv (x:>ISeq) (s.seq())
            | _ -> false

    interface Seqable with
        member x.seq() = (x:>ISeq)

    override x.Equals(o) = 
        match o with
        | :? Seqable as s -> Util.seqEquals (x:>ISeq) (s.seq())
        | _ -> false

    override x.GetHashCode() = Util.getHashCode x

    override x.ToString() = Util.seqToString x
        
   
and  SimpleEmptySeq() =

    interface ISeq with 
        member _.first() = null
        member _.next() = null
        member x.more() = x :> ISeq
        member x.cons(o) = SimpleCons(o,x) :> ISeq 

    interface IPersistentCollection with
        member _.count() = 0
        member x.cons(o) = (x:>ISeq).cons(o) :> IPersistentCollection
        member x.empty() = x:>IPersistentCollection
        member x.equiv(o) = x.Equals(o)

    interface Seqable with
        member x.seq() = null

    override x.Equals(o) = 
        match o with
        | :? Seqable as s -> s.seq() = null
        | _ -> false

    override x.GetHashCode() = 1
       
    override x.ToString() = "()"

// Make a super-simple Range sequence to implement ISeq
[<AllowNullLiteral>]
type SimpleRange(s,e) =
    let startVal : int = s
    let endVal : int = e

    interface ISeq with
        member x.first() = upcast startVal 
        member x.next() = (x:>ISeq).more().seq()
        member x.more() = if startVal = endVal then SimpleEmptySeq() :> ISeq else SimpleRange(startVal+1,endVal) :> ISeq
        member x.cons(o) = SimpleCons(o,(x:>ISeq)) :> ISeq

    interface IPersistentCollection with
        member _.count() = endVal-startVal+1
        member x.cons(o) =  (x:>ISeq).cons(o) :> IPersistentCollection
        member _.empty() = SimpleEmptySeq() :> IPersistentCollection
        member x.equiv(o) = 
            match o with
            | :? Seqable as s -> Util.seqEquiv (x:>ISeq) (s.seq())
            | _ -> false

    interface Seqable with
        member x.seq() = (x:>ISeq)

    override x.Equals(o) = 
        match o with
        | :? Seqable as s -> Util.seqEquals (x:>ISeq) (s.seq())
        | _ -> false

    override x.GetHashCode() = Util.getHashCode x

    override x.ToString() = Util.seqToString x
        


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
            Expect.isNull (c.next()) "emptyseq.cons.next should be null"


        testCase "emptyseq implements IPersistentCollection" <| fun _ ->
            let es = SimpleEmptySeq() :> IPersistentCollection
            Expect.equal (es.count()) 0 "emptyseq.count should be 0"
            Expect.equal (es.empty()) es "emptyseq.empty should be itself"
            Expect.isTrue (es.equiv(es)) "emptyseq should be equiv of itself"
            Expect.isFalse (es.equiv(es.cons(1))) "emtpyseq should not be equive to a seq with an element"

        testCase "emptyseq implements Seqable" <| fun _ ->
             let es = SimpleEmptySeq() 
             Expect.isNull ((es:>Seqable).seq()) "emptyseq.seq should be null"
        
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
             Expect.equal ((c:>Seqable).seq()) c "cons.seq should be itself"
        
    ]


[<Tests>]
let rangeTests = 
    testList "ranges" [
        
        testCase "SimpleRange implements ISeq" <| fun _ ->
            let c = SimpleRange(0,3) :> ISeq
            let mutable mc = c
            for i = 0 to 3 do
                Expect.equal (mc.first()) (upcast i) "ith element of this range should be i"
                mc <- mc.next()
            
            let c2 = c.cons(10)
            Expect.equal (c2.first()) (upcast 10) "range.cons.first should be new item"
            Expect.equal (c2.next()) c "range.cons.next should be itself"


        testCase "SimpleRange implements IPersistentCollection" <| fun _ ->
            let c = SimpleRange(0,3)
            let pc = c :> IPersistentCollection
            Expect.equal (pc.count()) 4 "range.count should be item count"
            Expect.equal (pc.empty().GetType()) typeof<SimpleEmptySeq> "range.empty should be an emptySeq"
            Expect.isTrue (pc.equiv(c)) "range should be equiv of itself"
            Expect.isFalse (pc.equiv((c:>ISeq).next()))  "range should not be equiv of its next"

        testCase "SimpleRange implements Seqable" <| fun _ ->
             let c = SimpleRange(0,3)
             Expect.equal ((c:>Seqable).seq()) (upcast c) "cons.seq should be itself"
        
    ]


[<Tests>]
let boundedLengthTests = 
    testList "boundedLength" [

        testCase "boundedLength on range, limit hit" <| fun _ ->
            let r = SimpleRange(0,9) :> ISeq
            Expect.equal (AFn.boundedLength(r,5)) 6 "Over limit should be bound+1"

        testCase "boundedLength on conses, limit hit" <| fun _ ->
            let c = makeConsSeq 9 
            Expect.equal (AFn.boundedLength(c,5)) 6 "Over limit should be bound+1"    

        testCase "boundedLength on range, seq end hit" <| fun _ ->
            let r = SimpleRange(0,9) :> ISeq
            Expect.equal (AFn.boundedLength(r,20)) (r.count()) "under limit on range should be seq count"

        testCase "boundedLength on conses, seq end hit" <| fun _ ->
            let c = makeConsSeq 2
 
            Expect.equal (AFn.boundedLength(c,20)) (c.count()) "under limit on conses should be seq count"    

        testCase "boundedLength on null should be 0" <| fun _ ->
            Expect.equal (AFn.boundedLength(null,20)) 0 "under limit on range should be seq count"
    
    ]


[<Tests>]
let seqLengthTests = 
    testList "seqLength" [

        testCase "seqLength on range" <| fun _ ->
            let r = SimpleRange(0,9) :> ISeq
            Expect.equal (AFn.seqLength r) 10 "seqLength should compute actual length"

        testCase "seqLength on conses" <| fun _ ->
            let c = makeConsSeq 9 
            Expect.equal (AFn.seqLength c) 9 "seqLength should compute actual length"


        testCase "seqLength on null should be 0" <| fun _ ->
            Expect.equal (AFn.seqLength null) 0 "seqLength should be 0 on null"
    ]

[<Tests>]
let seqToArrayTests =
    testList "seqToArray" [
    
        testCase "seqToArray on null returns 0-length array" <| fun _ ->
            Expect.equal (null |>  AFn.seqToArray<obj> |>  Array.length) 0 "Length should be 0"

        testCase "seqToArray on cons should return array with proper elements" <| fun _ ->
            let a = makeConsSeq 3 |> AFn.seqToArray<int>
            Expect.equal (Array.length a) 3 "Should have proper number of elements"
            Expect.equal a.[0] 0 "0-th element should be 0"
            Expect.equal a.[1] 1 "1-th element should be 1"
            Expect.equal a.[2] 2 "2-th element should be 2"
    
    
    ]


type TestFn() =
    inherit AFn()

    interface IFn with
        override x.invoke() = upcast "Zero"
        override x.invoke(arg1) = (unbox arg1) + 1 :> obj
        override x.invoke(arg1,arg2) = (unbox arg1) + (unbox arg2) :> obj
    
    interface IFnArity with
        override x.hasArity(n:int) =
            match n with
            | 0 | 1 | 2 -> true
            | _ -> false

let tf = TestFn()

[<Tests>]
let basicAFnTests = 
    testList "AFn tests" [

        testCase "call invoke()" <| fun _ ->
            let result : string  =  downcast (tf:>IFn).invoke()
            Expect.equal  result "Zero" "Call with no args"

        testCase "call invoke(a)" <| fun _ ->
            let i = 12
            let result: int = downcast (tf:>IFn).invoke(i)
            Expect.equal result (i+1) "call with one arg should add 1"

        testCase "call invoke(a,b)" <| fun _ ->
            let i = 12
            let j = 30
            let result: int = downcast (tf:>IFn).invoke(i,j)
            Expect.equal result (i+j) "call with two args should add them"

        testCase "call invoke with too many args fails" <| fun _ ->
            let f () = (tf:>IFn).invoke(1,2,3,4) |> ignore
            Expect.throwsT<ArityException>  f "Does not accept four arguments"         

        testCase "check hasArity" <| fun _ ->
            Expect.isFalse ((tf:>IFnArity).hasArity 3) "Does not have this arity"
            Expect.isTrue ((tf:>IFnArity).hasArity 0) "has this arity"
            Expect.isTrue ((tf:>IFnArity).hasArity 1) "has this arity"
            Expect.isTrue ((tf:>IFnArity).hasArity 2) "has this arity"        
    ]

