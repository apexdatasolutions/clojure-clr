namespace Clojure.Collections

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Linq


type TypedSeqEnumerator<'T  when 'T :not struct>(s:ISeq) =
    let mutable orig = s
    let mutable next = s
    let mutable isRealized = false
    let mutable curr : 'T option = None

    interface IEnumerator<'T> with
        member x.Current = 
            if next = null then raise <| InvalidOperationException("No current value.")
            match curr with 
            | None -> let v = RT.first(next) :?> 'T in curr <- Some v; v;
            | Some v -> v



    interface IEnumerator with
        member x.Reset() = 
            isRealized <- false;  // TODO - first this -- already realized!  (Note from original code)
            curr <- None
            next <- orig
        member x.MoveNext() =
            if next = null then false
            else
                curr <- None
                if not isRealized
                then 
                    isRealized <- true
                    next <- RT.seq(next)
                else 
                    next <- RT.next(next)
                next <> null
        member x.Current = (x:>IEnumerator<'T>).Current :> obj
    
    member x.Dispose disposing =
        if disposing then
            orig <- null
            curr <- None
            next <- null

    interface IDisposable with
        member x.Dispose() = x.Dispose(true); GC.SuppressFinalize(x)


type SeqEnumerator(s:ISeq) =
    inherit TypedSeqEnumerator<obj>(s)

type IMapEntrySeqEnumerator(s:ISeq) =
    inherit TypedSeqEnumerator<IMapEntry>(s)

[<Sealed>]
type Reduced(v) = 
    let value = v

    interface IDeref with
        member x.deref() = value


[<AbstractClass>]
[<AllowNullLiteral>]
type ASeq(m) =
    inherit Obj(m)

    [<NonSerialized>]
    let mutable hash = 0

    [<NonSerialized>]
    let mutable hasheq = 0

    new() = ASeq(null)
  
    override x.ToString() = RT.printString(x)

    override x.Equals(o) = 
        if obj.ReferenceEquals(x,o) then true
        else
            match o with
            | :? Sequential 
            | :? IList -> 
                let rec step (s1:ISeq) (s2:ISeq) =
                    match s1, s2 with
                    | null, null -> true
                    | _, null -> false
                    | null, _ -> false
                    | _ -> Util.equals(s1.first(),s2.first()) && step (s1.next()) (s2.next())
                step x (RT.seq(o))
            | _ -> false

    override x.GetHashCode() =
        let rec step (xs:ISeq) (h:int) =
            match xs with
            | null -> h
            | _ -> 
                let f = xs.first()
                let fh = Util.hash f
                step (xs.next()) (31*h + fh)
        if hash = 0 then hash <- step ((x:>ISeq).seq()) 1
        hash


    static member doCount (s:ISeq) =
        let rec step (s:ISeq) cnt = 
            match s with
            | null -> cnt
            | :? Counted as c -> cnt + c.count()
            | _ -> step (s.next()) (cnt+1)
        step s 0

    // This is a little bit of stupidity so we can access GetEnumerator through base in derived classes
    // I've been unable to find a better was

    member  x.GetMyEnumerator() = (x:>IEnumerable).GetEnumerator()

    interface ISeq with
        member x.first() = raise <| NotImplementedException("Subclasses of ASeq must implement ISeq.first()")
        member x.next() = raise <| NotImplementedException("Subclasses of ASeq must implement ISeq.next()")
        member x.more() =
            let s = (x:>ISeq).next()
            if s = null then EmptyList.Empty :> ISeq else s
        member x.cons(o) = Cons(o,x) :> ISeq  

    interface IPersistentCollection with
        member x.cons(o) = (x:>ISeq).cons(o) :> IPersistentCollection
        member x.count() = 1 + ASeq.doCount ((x:>ISeq).next())
        member x.empty() = EmptyList.Empty :> IPersistentCollection
        member x.equiv(o) = 
            match o with
            | :? Sequential | :? IList -> 
                let rec step (s1:ISeq) (s2:ISeq) =
                    match s1, s2 with
                    | null, null -> true
                    | _, null -> false
                    | null, _ -> false
                    | _ -> Util.equiv(s1.first(),s2.first()) && step (s1.next()) (s2.next())
                step x (RT.seq(o))    
            | _ -> false

    interface Seqable with
        member x.seq() = x :> ISeq

    // In the original, we also did IList<obj>  We goenthing special form that, I think.


    interface IList with
        member _.Add(_) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Clear() = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Insert(i,v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Remove(v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.RemoveAt(i) = raise <| InvalidOperationException("Cannot modify an immutable sequence")    
        member _.IsFixedSize = true
        member _.IsReadOnly = true
        member x.Item 
            //Java has this: return RT.nth(this, index);
            // THis causes an infinite loop in my code.    TODO:  SEE IF THIS IS STILL TRUE, OR FIND A WORKAROUND?
            // When this was introduces, a change was made in RT.nth that changed the List test in its type dispatch to RandomAccess.
            // CLR does not have the equivalent notion, so I just left it at IList.  BOOM!
            // So, I have to do a sequential search, duplicating some of the code in RT.nth.
            with get index = 
                if index < 0 then raise <| ArgumentOutOfRangeException("index","Index must be non-negative")
                let rec step i (s:ISeq) = 
                    if i = index then s.first()
                    elif s = null then raise <| ArgumentOutOfRangeException("index","Index past end of list")
                    else step (i+1) (s.next())
                step 0 x                                            // TODO: See IndexOf. Should this be called on x or x.seq() ??  Check original Java code.
            and set _ _ = raise <| InvalidOperationException("Cannot modify an immutable sequence")  
        member x.IndexOf(v) =
            let rec step i (s:ISeq) = 
                if isNull s then -1
                else if Util.equiv(s.first(), v) then i
                else step (i+1) (s.next())
            step 0 ((x:>ISeq).seq())
        member x.Contains(v) = 
            let rec step (s:ISeq) = 
                if isNull s then false
                else if Util.equiv(s.first(), v) then true
                else step (s.next())
            step ((x:>ISeq).seq())

    interface IEnumerable with
        member x.GetEnumerator() = new SeqEnumerator(x) :> IEnumerator

    interface ICollection with
        member x.Count = (x:>IPersistentCollection).count()
        member x.IsSynchronized = true
        member x.SyncRoot = upcast x
                 
        member x.CopyTo(arr : Array,idx) =
            if isNull arr then raise <| ArgumentNullException("array")
            if arr.Rank <> 1 then raise <| ArgumentException("Array must be 1-dimensional")
            if idx < 0 then raise <| ArgumentOutOfRangeException("arrayIndex","must be non-negative")
            if arr.Length - idx < (x:>IPersistentCollection).count() then raise <| InvalidOperationException("The number of elements in source is greater than the available space in the array.")
            let rec step (i:int) (s:ISeq) =
                if i < arr.Length && s <> null 
                then 
                    arr.SetValue(s.first(),i)
                    step (i+1) (s.next())
            step idx (x:>ISeq)

    interface IHashEq with
        member x.hasheq() = 
            if hasheq = 0 then hasheq <- Util.hashOrdered(x)
            hasheq

and [<Sealed>] Cons(meta,f:obj,m:ISeq) =
    inherit ASeq(meta)

    let first : obj = f
    let more : ISeq = m

    new(f:obj,m:ISeq) = Cons(null,f,m)

    interface IObj with
        member x.withMeta(m) = if Object.ReferenceEquals(m,meta) then (x:>IObj) else Cons(m,first,more) :> IObj

    interface ISeq with
        member _.first() = first
        member x.next() = (x:>ISeq).more().seq()
        member x.more() = 
            match more with 
            | null ->  upcast EmptyList.Empty
            | _ -> more
        
    interface IPersistentCollection with
        member x.count() = 1 + RT.count(more)


and [<Sealed>] EmptyList(m) =
    inherit Obj(m)

    new() = EmptyList(null)

    static member hasheq = Util.hashOrdered(Enumerable.Empty<Object>())
    static member Empty : EmptyList = EmptyList()

    override x.GetHashCode() = 1
    override x.Equals(o) = 
        match o with    
        | :? Sequential | :? IList -> RT.seq(o) |> isNull 
        | _ -> false

    interface IObj with 
        member x.withMeta(m) = if obj.ReferenceEquals(m,(x:>IMeta).meta()) then x:>IObj else EmptyList(m) :> IObj

    interface ISeq with
        member x.first() = null
        member x.next() = null
        member x.more() = x :> ISeq
        member x.cons(o) = PersistentList((x:>IMeta).meta(),o,null,1) :> ISeq

    interface IPersistentCollection with
        member x.count() = 0
        member x.cons(o) = (x:>ISeq).cons(o) :> IPersistentCollection
        member x.empty() = x :> IPersistentCollection
        member x.equiv(o) = x.Equals(o)

    interface Seqable with
        member x.seq() = null

    interface IPersistentStack with
        member x.peek() = null
        member x.pop() = raise <| InvalidOperationException("Attempt to pop an empty list")

    interface Sequential

    interface IPersistentList

    interface IHashEq with
        member x.hasheq() = EmptyList.hasheq

    interface ICollection with
        member x.CopyTo(a:Array, idx:int) = ()  // no-op
        member x.Count = 0
        member x.IsSynchronized = true
        member x.SyncRoot = upcast x

    static member emptyEnumerator : IEnumerator = Seq.empty<obj>.GetEnumerator() :> IEnumerator

    interface IEnumerable with
        member x.GetEnumerator() = EmptyList.emptyEnumerator

    interface IList with
        member _.Add(_) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Clear() = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Insert(i,v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Remove(v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.RemoveAt(i) = raise <| InvalidOperationException("Cannot modify an immutable sequence")    
        member _.IsFixedSize = true
        member _.IsReadOnly = true
        member x.Item 
            with get index = raise <| ArgumentOutOfRangeException("index")
            and set _ _ = raise <| InvalidOperationException("Cannot modify an immutable sequence")  
        member x.IndexOf(v) = -1
        member x.Contains(v) = false

and [<AllowNullLiteral>] PersistentList(m1,f1,r1,c1) =
    inherit ASeq(m1)
    let first : obj = f1
    let rest : IPersistentList = r1
    let count = c1
    new(first:obj) = PersistentList(null,first,null,1)

    // for backwards compatability
    static member Empty = EmptyList.Empty
    
    static member create(init:IList) = 
        let mutable r = EmptyList.Empty :> IPersistentList
        for i = init.Count-1 downto 0 do
            r <-  downcast r.cons(init.[i]) 
        r

    interface IObj with 
        member x.withMeta(m) = 
            if obj.ReferenceEquals(m,(x:>IMeta).meta()) 
            then x:>IObj 
            else PersistentList(m,first,rest,count) :> IObj

    interface ISeq with
        member x.first() = first
        member x.next() = if count = 1 then null else rest.seq()
        member x.cons(o) = PersistentList((x:>IObj).meta(),o,(x:>IPersistentList),count+1) :> ISeq

    interface IPersistentCollection with
        member x.count() = count
        member x.empty() = (EmptyList.Empty:>IObj).withMeta((x:>IMeta).meta()) :?> IPersistentCollection

    interface IPersistentStack with
        member x.peek() = first
        member x.pop() = 
            match rest with
            | null -> (EmptyList.Empty:>IObj).withMeta((x:>IMeta).meta()) :?> IPersistentStack
            | _ -> rest :> IPersistentStack

    interface IPersistentList

    interface IReduceInit with
        member x.reduce(fn,start) =
            let rec step (s:ISeq) (value:obj) =
                match s with
                | null -> value
                | _ ->
                    match value with
                    | :? Reduced as r -> (r:>IDeref).deref()
                    | _ -> step (s.next()) (fn.invoke(value,s.first()))
            let init = fn.invoke(start,(x:>ISeq).first())
            let ret = step ((x:>ISeq).next()) init
            match ret with 
            | :? Reduced as r -> (r:>IDeref).deref()
            | _ -> ret
            
    interface IReduce with
        member x.reduce(fn) = 
            let rec step (s:ISeq) (value:obj) =
                match s with
                | null -> value
                | _ ->
                    let nextVal = (fn.invoke(value,s.first()))
                    match nextVal with
                    | :? Reduced as r -> (r:>IDeref).deref()
                    | _ -> step (s.next()) nextVal
            step ((x:>ISeq).next()) ((x:>ISeq).first())

// We had to defer this definition until now because we needed PersistentList and Cons

module RT2 =

    let cons (x:obj, coll:obj) : ISeq =
        match coll with
        | null -> upcast PersistentList(x)
        | :? ISeq as s -> upcast Cons(x,s)
        | _ -> upcast Cons(x,RT.seq(coll))

// LazySeq comes up surprisingly early in the food (procedure) chain.
// It relies on very few things external to it, so let's go next.


[<Sealed>][<AllowNullLiteral>]
type LazySeq(m1, fn1, s1) =
    inherit Obj(m1)
    let mutable fn : IFn = fn1
    let mutable s : ISeq = s1
    let mutable sv : obj = null
    new(fn:IFn) = LazySeq(null,fn,null)
    new(m1: IPersistentMap, s1: ISeq) = LazySeq(m1,null,s1)

    override x.GetHashCode() = 
        match (x:>ISeq).seq() with
        | null -> 1
        | _ as s -> Util.hash s


    override x.Equals(o:obj) =
        match  (x:>ISeq).seq(), o with
        | null, :? Sequential 
        | null, :? IList      -> RT.seq(o) = null
        | null, _ -> false
        | _ as s ,_ -> s.Equals(o)

    interface IObj with
        member x.withMeta(meta: IPersistentMap) =
            if ( (x :> IMeta).meta() = meta ) then x :> IObj
            else LazySeq(meta,(x:>ISeq).seq()) :> IObj

    member x.sval() : obj = 
        if not (isNull fn)
        then
            sv <- fn.invoke()
            fn <- null

        match sv with
        | null -> upcast s
        | _ -> sv

 
    interface Seqable with

        [<MethodImpl(MethodImplOptions.Synchronized)>]
        member x.seq() =
            let rec getNext (x:obj) =
                match x with
                | :? LazySeq as ls -> getNext(ls.sval())
                | _ -> x
            
            x.sval() |> ignore
            if not (isNull sv) then
                let ls = sv
                sv <- null
                s <- RT.seq(getNext(ls))
            s


    interface IPersistentCollection with
        member x.count() =
            let rec countAux (s:ISeq) (acc:int) : int =
                match s with  
                | null -> acc
                | _ -> countAux (s.next()) (acc+1)
            countAux s 0
        member x.cons(o) = upcast (x:>ISeq).cons(o)
        member x.empty() = upcast PersistentList.Empty
        member x.equiv(o) = 
            match (x:>ISeq).seq() with
            | null -> 
                match o with
                | :? IList | :? Sequential -> RT.seq(o) = null
                | _ -> false
            | _ as s -> s.equiv(o)

    interface ISeq with
        member x.first() = 
            (x:>ISeq).seq() |> ignore
            if isNull s then null else s.first()
        member x.next() =
            (x:>ISeq).seq() |> ignore
            if isNull s then null else s.next()
        member x.more() =
            (x:>ISeq).seq() |> ignore
            if isNull s then upcast PersistentList.Empty else s.more()         
        member x.cons(o:obj) : ISeq = RT2.cons(o,(x:>ISeq).seq())

    interface IPending with
        member x.isRealized() = isNull fn

    interface IHashEq with
        member x.hasheq() = Util.hashOrdered(x)

    interface IEnumerable with
        member x.GetEnumerator() = upcast new SeqEnumerator(x)

    interface IList with
        member _.Add(_) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Clear() = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Insert(i,v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Remove(v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.RemoveAt(i) = raise <| InvalidOperationException("Cannot modify an immutable sequence")    
        member _.IsFixedSize = true
        member _.IsReadOnly = true
        member x.Item 
            with get index = 
                if index < 0 then raise <| ArgumentOutOfRangeException("index","Index must be non-negative")
                let rec step i (s:ISeq) = 
                    if i = index then s.first()
                    elif s = null then raise <| ArgumentOutOfRangeException("index","Index past end of list")
                    else step (i+1) (s.next())
                step 0 x                                            // TODO: See IndexOf. Should this be called on x or x.seq() ??  Check original Java code.
            and set _ _ = raise <| InvalidOperationException("Cannot modify an immutable sequence")  
        member x.IndexOf(v) =
            let rec step i (s:ISeq) = 
                if isNull s then -1
                else if Util.equiv(s.first(), v) then i
                else step (i+1) (s.next())
            step 0 ((x:>ISeq).seq())
        member x.Contains(v) = 
            let rec step (s:ISeq) = 
                if isNull s then false
                else if Util.equiv(s.first(), v) then true
                else step (s.next())
            step ((x:>ISeq).seq())

    interface ICollection with
        member x.Count = (x:>IPersistentCollection).count()
        member x.IsSynchronized = true
        member x.SyncRoot = upcast x
             
        member x.CopyTo(arr : Array,idx) =
            if isNull arr then raise <| ArgumentNullException("array")
            if idx < 0 then raise <| ArgumentOutOfRangeException("arrayIndex","must be non-negative")
            if arr.Rank <> 1 then raise <| ArgumentException("Array must be 1-dimensional")
            if idx >= arr.Length then raise <|  ArgumentException("index", "must be less than the length")
            if (x:>IPersistentCollection).count() > arr.Length - idx then raise <| InvalidOperationException("Not enough available space from index to end of the array.")
            let rec step (i:int) (s:ISeq) =
                if not (isNull s) then 
                    arr.SetValue(s.first(),i)
                    step (i+1) (s.next())
            step idx (x:>ISeq)







