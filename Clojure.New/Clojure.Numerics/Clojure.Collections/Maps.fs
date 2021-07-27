namespace Clojure.Collections

open System
open System.Collections
open System.Collections.Generic
open System.Reflection


[<Sealed>]
type MapEnumerator(m:IPersistentMap) =
    let ienum : IEnumerator = upcast new SeqEnumerator(m.seq())
    let disposed = false

    member private x.currentKey = (ienum.Current:?>IMapEntry).key()
    member private x.currentVal = (ienum.Current:?>IMapEntry).value()

    interface IDictionaryEnumerator with
        member x.Entry = DictionaryEntry(x.currentKey,x.currentVal)
        member x.Key = x.currentKey
        member x.Value = x.currentVal

    interface IEnumerator with  
        member x.Current = ienum.Current
        member x.MoveNext() = ienum.MoveNext()
        member x.Reset() = ienum.Reset()

    // Do we need this to be IDisposable?  Was in the original


[<Sealed>]
[<AllowNullLiteral>]
type KeySeq(meta, s,e) =
    inherit ASeq(meta)
    let iseq : ISeq = s
    let ienum : IEnumerable = e

    new(s,e) = KeySeq(null,s,e)


    static member create(s:ISeq) : KeySeq =
        match s with
        | null -> null
        | _ ->KeySeq(s,null)

    static member createFromMap(m:IPersistentMap) : KeySeq =
        if isNull m then null
        else
            let s = m.seq()
            if isNull s then null
            else KeySeq(s,m)

    interface ISeq with
        override x.first() =
            match iseq.first() with
            | :? IMapEntry as me -> me.key()
            | :? DictionaryEntry as de -> de.Key
            | _ -> raise <| InvalidCastException("Cannot convert hashtable entry to IMapEntry or DictionaryEntry")
        override x.next() = upcast KeySeq.create(iseq.next())

    interface IObj with
        override x.withMeta(m) =
            if Object.ReferenceEquals(m,meta) then upcast x
            else upcast  KeySeq(m,iseq,ienum)

    static member private keyIterator (e:IEnumerable) : IEnumerator =
        let s = 
            seq {
                    for item in e do
                        yield (item :?> IMapEntry).key()
                }
        upcast s.GetEnumerator()


    interface IEnumerable with
        override x.GetEnumerator() = 
            match ienum with    
            | null -> base.GetMyEnumerator() 
            | :? IMapEnumerable as imi -> imi.keyEnumerator()
            | _ -> KeySeq.keyIterator(ienum)

[<Sealed>]
[<AllowNullLiteral>]
type ValSeq(meta, s,e) =
    inherit ASeq(meta)
    let iseq : ISeq = s
    let ienum : IEnumerable = e

    new(s,e) = ValSeq(null,s,e)


    static member create(s:ISeq) : ValSeq =
        match s with
        | null -> null
        | _ ->ValSeq(s,null)

    static member createFromMap(m:IPersistentMap) : ValSeq =
        if isNull m then null
        else
            let s = m.seq()
            if isNull s then null
            else ValSeq(s,m)

    interface ISeq with
        override x.first() =
            match iseq.first() with
            | :? IMapEntry as me -> me.key()
            | :? DictionaryEntry as de -> de.Key
            | _ -> raise <| InvalidCastException("Cannot convert hashtable entry to IMapEntry or DictionaryEntry")
        override x.next() = upcast ValSeq.create(iseq.next())

    interface IObj with
        override x.withMeta(m) =
            if Object.ReferenceEquals(m,meta) then upcast x
            else upcast  ValSeq(m,iseq,ienum)

    static member keyIterator (e:IEnumerable) : IEnumerator =
        let s = 
            seq {
                    for item in e do
                        yield (item :?> IMapEntry).value()
                }
        upcast s.GetEnumerator()


    interface IEnumerable with
        override x.GetEnumerator() = 
            match ienum with    
            | null -> base.GetMyEnumerator() 
            | :? IMapEnumerable as imi -> imi.keyEnumerator()
            | _ -> ValSeq.keyIterator(ienum)


// TODO: Need to finish this.
// should be MapEntry -> AMapEntry -> APersistetntVector, so we need to get that in here ahead  APersistentVector is 1140+ SLOC.  AMapEntry is over 250.
// We don't need all this just to get APersistentMap going

[<AllowNullLiteral>]
type MapEntry(k,v) =
    let key : obj = k
    let value : obj = v

    static member create(k,v) = MapEntry(k,v)  // not sure why we need this, but here it is

    interface IMapEntry with
        member x.key() = key
        member x.value() = value



[<AbstractClass>]
[<AllowNullLiteral>]
type APersistentMap() =
    inherit AFn()

    let mutable hash = 0
    let mutable hasheq = 0

    // Some static methods
    // Could be put into their own module?

    static member mapEquals(m:IPersistentMap, o:obj) : bool =
        
        let rec step (s:ISeq) (d:IDictionary) =
            if isNull s then true
            else 
                let me : IMapEntry = downcast s.first()
                if d.Contains(me.key) && Util.equals(me.value(),d.[me.key()])
                then step (s.next()) d
                else false            

        let mapDictEquals (m:IPersistentMap) (d:IDictionary) : bool =
            if d.Count <> m.count() 
            then false
            else step (m.seq()) d
               
            
        if Object.ReferenceEquals(m,o) then true
        else
            match o with
            | :? IDictionary as d -> mapDictEquals m d
            | _ -> false

    static member mapHash (m:IPersistentMap) : int =
        let rec step (s:ISeq) h =
            if isNull s then h
            else 
                let me : IMapEntry = downcast s.first()
                let hk = if me.key() |> isNull then 0 else me.key().GetHashCode()
                let hv = if me.value()  |> isNull then 0 else me.value().GetHashCode()
                step (s.next()) (h + hk^^^hv)
        step (m.seq()) 0

    // do we still need this?  Still in this code, but not used here

    static member mapHasheq (m:IPersistentMap) int = Util.hashUnordered(m)
        
           
    // Object overrides

    override x.ToString() = RT.printString(x)

    override x.Equals(o) = APersistentMap.mapEquals(x,o)

    override x.GetHashCode() =
        if hash = 0 then hash <- APersistentMap.mapHash(x)
        hash

    interface IMeta with
        member x.meta() = raise <| NotImplementedException("You must implement meta in derived classes")

    interface IObj with
        member x.withMeta(m) = raise <| NotImplementedException("You must implement withMeta in derived classes")

    interface Counted with
        member x.count() = raise <| NotImplementedException("You must implement count in derived classes")

        

    interface Seqable with
        member x.seq() = raise <| NotImplementedException("You must implement seq in derived classes")

    interface IPersistentCollection with
        member x.count() = (x:>Counted).count()
        member x.empty() = raise <| NotImplementedException("You must implement empty in derived classes")

        member x.cons(o) = upcast (x:>IPersistentMap).cons(o)
        member x.equiv(o) = 
            match o with
            | :? IDictionary as d ->
                if o :? IPersistentMap && not (o :? MapEquivalence)
                then false
                elif d.Count <> (x:>IPersistentCollection).count()
                then false
                else 
                    let rec step (s:ISeq) =                            
                        if isNull s then true
                        else 
                            let me : IMapEntry = downcast s.first()
                            if d.Contains(me.key) && Util.equiv(me.value(),d.[me.key()])
                            then step (s.next())
                            else false
                    step ((x:>Seqable).seq())
            | _ -> false


    interface ILookup with
        member x.valAt(k) = raise <| NotImplementedException("You must implement valAt in derived classes")
        member x.valAt(k,nf) = raise <| NotImplementedException("You must implement valAt in derived classes")


    interface Associative with
        member x.assoc(k,v) = upcast (x:>IPersistentMap).assoc(k,v)
        member x.containsKey(k) = raise <| NotImplementedException("You must implement containsKey in derived classes")
        member x.entryAt(k) = raise <| NotImplementedException("You must implement entryAt in derived classes")


    // TODO: conversion to an IMapEntry could be a protocol. Would simplify code in a lot of places
    interface IPersistentMap with
        member x.assoc(k,v) = raise <| NotImplementedException("You must implement entryAt in derived classes")
        member x.assocEx(k,v) = raise <| NotImplementedException("You must implement entryAt in derived classes")
        member x.without(k) = raise <| NotImplementedException("You must implement entryAt in derived classes")

        member x.count() = (x:>Counted).count()
        member x.cons(o) =
            match o with
            | null -> upcast x
            | :? IMapEntry as e -> (x:>IPersistentMap).assoc(e.key(),e.value())
            | :? DictionaryEntry as e -> (x:>IPersistentMap).assoc(e.Key,e.Value)
            | _ when o.GetType().IsGenericType && o.GetType().Name = "KeyValuePair`2" ->
                let t = o.GetType()
                let k = t.InvokeMember("Key",BindingFlags.GetProperty,null,o,null)
                let v = t.InvokeMember("Value",BindingFlags.GetProperty,null,o,null)
                (x:>IPersistentMap).assoc(k,v)
            | :? IPersistentVector as v ->
                if v.count() = 2 then (x:>IPersistentMap).assoc(v.nth(0),v.nth(1))
                else raise <| ArgumentException("o","Vector arg to map cons must be a pair")
            | _ ->
                let rec step (s:ISeq) (m:IPersistentMap) =
                    if isNull s then m
                    else 
                        let me =  s.first() :?> IMapEntry
                        step (s.next()) (m.assoc(me.key(),me.value()))
                step (RT.seq(o)) x

    interface IFn with
        override x.invoke(arg1) = (x:>ILookup).valAt(arg1)
        override x.invoke(arg1,arg2) = (x:>ILookup).valAt(arg1,arg2)

    interface IHashEq with
        member x.hasheq() =
            if hasheq = 0 then hasheq <- Util.hashUnordered(x :> IEnumerable)
            hasheq


    //interface IDictionary<obj,obj> with
    //    member x.Add(k,v) = raise <| InvalidOperationException("Cannot modify an immutable map")
    //    member x.Keys = KeySeq.create((x:>Seqable).seq())
    //    member x.Values = ValSeq.create((x:>Sequable).seq()) 
    //    member x.Item
    //        with get key = (x:>ILookup).valAt(key)
    //        and set _ _ = raise <| InvalidOperationException("Cannot modify an immutable map") 

    //interface ICollection<KeyValuePair<obj,obj>> with
    //    member x.Add(kv) = raise <| InvalidOperationException("Cannot modify an immutable map")
    //    member x.Clear() = raise <| InvalidOperationException("Cannot modify an immutable map")   
    //    member x.Remove(kv) = raise <| InvalidOperationException("Cannot modify an immutable map") 
    //    member x.IsReadOnly = true
    //    member x.Count = (x:>IPersistentMap).count()
    //    member x.CopyTo(arr, index) = () // IPMLEMENT
    //    member x.Contains(kv) = 
    //        let ok, value = (x:>IDictionary<obj,obj>).TryGetValue(kv.Key)
    //        if not ok then false
    //        elif isNull value then isNull kv.Value
    //        else value.Equals(kv.Value)
    //    member x.GetEnumerator() = 
    //        let mySeq = 
    //            seq { 
    //                    // can't use my usual recursive step function for iteration here because yield needs to be at top level
    //                    let mutable s = (x:>Seqable).seq()
    //                    while not (isNull x) do
    //                        let me : IMapEntry = downcast s.first() 
    //                        yield KeyValuePair<obj,obj>(me.key(),me.value())
    //                        s <- s.next()
    //                }
    //        mySeq.GetEnumerator()


    interface IEnumerable with
        member x.GetEnumerator() : IEnumerator = new SeqEnumerator((x:>Seqable).seq()) :> IEnumerator

    interface IEnumerable<IMapEntry> with
        member x.GetEnumerator() =   // could be a TypedSeqEnumerator?
            let s = 
                seq {
                        let mutable s = (x:>Seqable).seq()
                        while not (isNull s) do
                            yield s.first() :?> IMapEntry
                            s <- s.next()
                    }
            s.GetEnumerator()


            //static member private keyIterator (e:IEnumerable) : IEnumerator =
            //    let s = 
            //        seq {
            //                for item in e do
            //                    yield (item :?> IMapEntry).key()
            //            }
            //    upcast s.GetEnumerator()
      

    interface ICollection with
        member x.IsSynchronized = true
        member x.SyncRoot = upcast x
        member x.Count = (x:>IPersistentMap).count()
        member x.CopyTo(arr,idx) =  
            if isNull arr then raise <| ArgumentNullException("array")
            if idx < 0 then raise <| ArgumentOutOfRangeException("arrayIndex","must be non-negative")
            if arr.Rank <> 1 then raise <| ArgumentException("Array must be 1-dimensional")
            if idx >= arr.Length then raise <|  ArgumentException("index", "must be less than the length")
            if (x:>IPersistentCollection).count() > arr.Length - idx then raise <| InvalidOperationException("Not enough available space from index to end of the array.")
            let rec step (i:int) (s:ISeq) =
                if not (isNull s) then 
                    arr.SetValue(s.first(),i)
                    step (i+1) (s.next())
            step idx ((x:>Seqable).seq())
        
    interface IDictionary with
        member x.IsFixedSize = true
        member x.IsReadOnly = true
        member x.Add(k,v) = raise <| InvalidOperationException("Cannot modify an immutable map")
        member x.Clear() = raise <| InvalidOperationException("Cannot modify an immutable map")   
        member x.Remove(k) = raise <| InvalidOperationException("Cannot modify an immutable map") 
        member x.Keys = upcast KeySeq.create((x:>Seqable).seq())
        member x.Values = upcast ValSeq.create((x:>Seqable).seq())
        member x.Contains(k) = (x:>Associative).containsKey(k)
        member x.Item
            with get key = (x:>ILookup).valAt(key)
            and set _ _ = raise <| InvalidOperationException("Cannot modify an immutable map") 
        member x.GetEnumerator() = upcast new MapEnumerator(x)
       
[<AbstractClass>]
type ATransientMap() =
    inherit AFn()

    abstract ensureEditable : unit -> unit
    abstract doAssoc : obj * obj -> ITransientMap
    abstract doWithout : key:obj -> ITransientMap
    abstract doValAt : obj * obj -> obj
    abstract doCount : unit -> int
    abstract doPersistent : unit -> IPersistentMap

    interface ITransientCollection with
        member x.persistent() = upcast (x:>ITransientMap).persistent()
        member x.conj(o) = upcast x.conj(o)

    member x.conj(value:obj) : ITransientMap =
        x.ensureEditable()
        
        // TODO: add KeyValuePair?  (also not in C# version)
        // TODO: find general method for handling heys
        match value with
        | :? IMapEntry as e -> downcast (x:>ITransientAssociative).assoc(e.key(),e.value())
        | :? DictionaryEntry as e -> downcast (x:>ITransientAssociative).assoc(e.Key,e.Value)
        | :? IPersistentVector as v ->
            if v.count() <> 2 then raise <| ArgumentException("value","vector arg to map conj must be a pair")
            downcast (x:>ITransientAssociative).assoc(v.nth(0),v.nth(1))
        | _ ->
            let mutable ret : ITransientMap = upcast x
            let mutable es : ISeq = RT.seq(value)
            while not (isNull es) do
                let e : IMapEntry = downcast es.first()
                ret <- ret.assoc(e.key(),e.value())
            ret

    static  member private NotFound:obj = obj()

    interface ILookup with
        member x.valAt(key:obj) = (x:>ILookup).valAt(key,null)
        member x.valAt(key:obj, notFound:obj) =
            x.ensureEditable()
            x.doValAt(key,notFound)    

    interface ITransientAssociative with
        member x.assoc(k,v) = upcast (x:>ITransientMap).assoc(k,v)

    interface ITransientAssociative2 with
        member x.containsKey(key:obj) = (x:>ILookup).valAt(key,ATransientMap.NotFound) <> ATransientMap.NotFound
        member x.entryAt(key:obj) = 
            let v = (x:>ILookup).valAt(key,ATransientMap.NotFound)
            if v = ATransientMap.NotFound then null
            else upcast MapEntry.create(key,v)

    interface ITransientMap with    
        member x.assoc(key,value) =
            x.ensureEditable()
            x.doAssoc(key,value)
        member x.without(key) =
            x.ensureEditable()
            x.doWithout(key)
        member x.persistent() =
            x.ensureEditable()
            x.doPersistent()

    interface Counted with
        member x.count() =
            x.ensureEditable()
            x.doCount()

    interface IFn with
        override x.invoke(arg1) = (x:>ILookup).valAt(arg1)
        override x.invoke(arg1,arg2) = (x:>ILookup).valAt(arg1,arg2)

       

// Util.EquivPred
// This was originally a delegate in the C#:
//       public delegate bool EquivPred(object k1, object k2);
// The only use was in PersistentArrayMap, so I think it is okay to move this to a C# type.
// And to move out of Util to here and make it internal.


module EquivPredLib =
    
    type internal EquivPred = (obj * obj) -> bool

    let equivNull(_:obj,k2:obj) = isNull k2
    let equivEquals(k1:obj,k2:obj) = k1.Equals(k2)
    let equivNumber(k1:obj,k2:obj) = Util.isNumeric k2 && Util.numericEquals(k1,k2)
    let equivColl =  Util.pcequiv

    let getEquivPred(k1:obj) : EquivPred =
        match k1 with  
        | null -> equivNull
        | _ when Util.isNumeric(k1) -> equivNumber
        | :? string | :? Symbol -> equivEquals
        | :? ICollection | :? IDictionary -> equivColl
        | _ -> equivEquals

open EquivPredLib
open System.Threading


type PersistentArrayMap(m,a) =
    inherit APersistentMap()
    let meta : IPersistentMap = m
    let kvs : obj array = a
    
    new() = PersistentArrayMap(null,Array.zeroCreate 0)
    new(a) = PersistentArrayMap(null,a)

    member x.create(init: obj[]) = PersistentArrayMap((x:>IMeta).meta(),init) 

    static member internal hashtableThreshold = 16
    static member Empty = PersistentArrayMap()
    
    interface IMeta with    
        member x.meta() = meta

    interface IObj with
        member x.withMeta(m) = 
            if Object.ReferenceEquals(m,meta) then upcast x
            else upcast PersistentArrayMap(m,kvs)

    member private x.indexOfObject(key:obj) = 
        let ep = getEquivPred key
        
        let rec step (idx:int) =
            if idx >= kvs.Length then -1
            elif  ep(key,kvs.[idx]) then idx
            else step (idx+2)
        step 0
    
    member private x.indexOfOKeyword(key:Keyword) =
        
        let rec step (idx:int) =
            if idx >= kvs.Length then -1
            elif  Object.ReferenceEquals(key,kvs.[idx]) then idx
            else step (idx+2)
        step 0

    member private x.indexOfKey(key:obj) =
        match key with
        | :? Keyword as kw -> x.indexOfOKeyword(kw)
        | _ -> x.indexOfObject key

    static member equalKey(k1:obj,k2:obj) =
        match k1 with
        | :? Keyword -> Object.ReferenceEquals(k1,k2)
        | _ -> Util.equiv(k1,k2)


    interface Seqable with
        override x.seq() = if kvs.Length = 0 then null else upcast ArrayMapSeq(kvs,0)

    interface IPersistentCollection with    
        override x.count() = kvs.Length / 2
        override x.empty() = (PersistentArrayMap.Empty:>IObj).withMeta(meta) :?> IPersistentCollection



    interface ILookup with
        member x.valAt(k) = (x:>ILookup).valAt(x,null)
        member x.valAt(k,notFound) = 
            let i = x.indexOfKey(k)
            if i < 0 then notFound
            else kvs.[i+1]


    interface Associative with 
        member x.containsKey(k) = x.indexOfKey(k) >= 0
        member x.entryAt(k) =
            let i = x.indexOfKey(k)
            if i < 0 then null  
            else upcast MapEntry.create(kvs.[i],kvs.[i+1])
        

    interface IPersistentMap with
        member x.assoc(k,v) = 
            let i = x.indexOfKey(k)
            if i >= 0 && Object.ReferenceEquals(kvs.[i+1],v) 
            then upcast x   // no change, no-op
            elif i < 0 && kvs.Length >= PersistentArrayMap.hashtableThreshold 
            then createHT(kvs).assoc(k,v)
            else
                // we will create a new PersistentArrayMap
                let newArray =
                    if i >= 0 
                    then
                        let na : obj[] = downcast kvs.Clone()
                        na.[i+1] <- v
                        na
                    else
                        let na = Array.zeroCreate<obj>(kvs.Length + 2)
                        Array.Copy(kvs,0,na,0,kvs.Length)
                        na.[Array.length(na)-2] <- k
                        na.[Array.length(na)-1] <- v
                        na
                upcast x.create(newArray)
        member x.assocEx(k,v) = 
            let i = x.indexOfKey(k)
            if i >= 0 then raise <| InvalidOperationException("Key already present")
            (x:>IPersistentMap).assoc(k,v)
        member x.without(k) =
            let i = x.indexOfKey(k)
            let newLen = kvs.Length-2
            if i < 0 then upcast x  // key does note exist, no-op
            elif newLen = 0 then downcast (x:>IPersistentCollection).empty()
            else 
                let newArray = Array.zeroCreate newLen
                Array.Copy(kvs,0,newArray,0,i)
                Array.Copy(kvs,i+1,newArray,i,newLen-i)
                upcast x.create(newArray)


    interface IEditableCollection with
        member x.asTransient()  = upcast TransientArrayMap(kvs)


    interface IKVReduce with    
        member x.kvreduce(f,init) =
            let rec step (i:int) (value:obj) =
                if i >= kvs.Length then value   
                else 
                    let v = f.invoke(value,kvs.[i],kvs.[i+1])
                    match v with  // in original, call to RT.isReduced
                    | :? Reduced as r -> (r:>IDeref).deref()
                    | _ -> step (i+2) v
            step 0 init


    interface IMapEnumerable with
        member x.keyEnumerator() = 
            let s = 
                seq {
                        for i in 0 .. 2 .. (kvs.Length-1) do    
                            yield kvs.[i]
                    }
            upcast s.GetEnumerator()

        member x.valEnumerator() =
            let s = 
                seq {
                        for i in 0 .. 2 .. (kvs.Length-1) do    
                            yield kvs.[i+1]
                    }
            upcast s.GetEnumerator()

    interface IEnumerable<IMapEntry> with
        member x.GetEnumerator() : IEnumerator<IMapEntry> =
            let s = 
                seq {
                        for i in 0 .. 2 .. (kvs.Length-1) do    
                            yield MapEntry.create(kvs.[i],kvs.[i+1]) :> IMapEntry
                    }
            s.GetEnumerator()
        member x.GetEnumerator() : IEnumerator = upcast (x:>IEnumerable<IMapEntry>).GetEnumerator()

    static member create (other:IDictionary) : IPersistentMap =
        let mutable ret : ITransientMap = (PersistentArrayMap.Empty:>IEditableCollection).asTransient() :?> ITransientMap
        for o in other do  
            let de = o:?> DictionaryEntry
            ret <- ret.assoc(de.Key,de.Value)
        ret.persistent()

    // if you pass an array to this, this map must become the owner or immutability is screwed
    member x.create([<ParamArray>] init : obj[]) : PersistentArrayMap = PersistentArrayMap((x:>IMeta).meta(),init)

    static member createWithCheck(init:obj[]) : PersistentArrayMap =
        for i in 0 .. 2 .. init.Length-1 do
            for j in i+2 .. 2 .. init.Length-1 do   
                if PersistentArrayMap.equalKey(init.[i],init.[j]) then raise <| ArgumentException("init","Duplicate key " + (init.[i].ToString())) 
        PersistentArrayMap(init)


// public class PersistentArrayMap : APersistentMap, IObj, IEditableCollection, IMapEnumerable, IMapEnumerableTyped<Object,Object>, IEnumerable, IEnumerable<IMapEntry>, IKVReduce




//       [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "ClojureJVM name match")]
//       public static PersistentArrayMap createAsIfByAssoc(Object[] init)
//       {
//           if ((init.Length & 1) == 1)
//               throw new ArgumentException(String.Format("No value supplied for key: {0}", init[init.Length - 1]), "init");

//           // ClojureJVM says: If this looks like it is doing busy-work, it is because it
//           // is achieving these goals: O(n^2) run time like
//           // createWithCheck(), never modify init arg, and only
//           // allocate memory if there are duplicate keys.
//           int n = 0;
//           for (int i = 0; i < init.Length; i += 2)
//           {
//               bool duplicateKey = false;
//               for (int j = 0; j < i; j += 2)
//               {
//                   if (EqualKey(init[i], init[j]))
//                   {
//                       duplicateKey = true;
//                       break;
//                   }
//               }
//               if (!duplicateKey)
//                   n += 2;
//           }
//           if (n < init.Length)
//           {
//               // Create a new shorter array with unique keys, and
//               // the last value associated with each key.  To behave
//               // like assoc, the first occurrence of each key must
//               // be used, since its metadata may be different than
//               // later equal keys.
//               Object[] nodups = new Object[n];
//               int m = 0;
//               for (int i = 0; i < init.Length; i += 2)
//               {
//                   bool duplicateKey = false;
//                   for (int j = 0; j < m; j += 2)
//                   {
//                       if (EqualKey(init[i], nodups[j]))
//                       {
//                           duplicateKey = true;
//                           break;
//                       }
//                   }
//                   if (!duplicateKey)
//                   {
//                       int j;
//                       for (j = init.Length - 2; j >= i; j -= 2)
//                       {
//                           if (EqualKey(init[i], init[j]))
//                           {
//                               break;
//                           }
//                       }
//                       nodups[m] = init[i];
//                       nodups[m + 1] = init[j + 1];
//                       m += 2;
//                   }
//               }
//               if (m != n)
//                   throw new ArgumentException("Internal error: m=" + m);
//               init = nodups;
//           }
//           return new PersistentArrayMap(init);
//       }

//       /// <summary>
//       /// Create an <see cref="IPersistentMap">IPersistentMap</see> to hold the data when 
//       /// an operation causes the threshhold size to be exceeded.
//       /// </summary>
//       /// <param name="init">The array of key/value pairs.</param>
//       /// <returns>A new <see cref="IPersistentMap">IPersistentMap</see>.</returns>
//       [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "ClojureJVM name match")]
//       private IPersistentMap createHT(object[] init)
//       {
//           return PersistentHashMap.create(meta(), init);
//       }


and [<Sealed>] ArrayMapSeq(m,a,i) =
    inherit ASeq(m)
    let kvs : obj[] = a
    let idx : int = i
    new(a,i) = ArrayMapSeq(null,a,i)

    interface IPersistentCollection with
        override x.count() = (kvs.Length - i) / 2

    interface ISeq with
        override x.first() = upcast MapEntry.create(kvs.[i],kvs.[i+1])
        override x.next() =
            let nextIdx = idx+2
            if nextIdx < kvs.Length then upcast ArrayMapSeq(kvs,nextIdx)
            else null

    interface IObj with
        override x.withMeta(m) = 
            if Object.ReferenceEquals(m,(x:>IMeta).meta()) then upcast x 
            else upcast ArrayMapSeq((x:>IMeta).meta(),kvs,idx)

    interface Counted with
        member x.count() = (x:>IPersistentCollection).count()

and TransientArrayMap(a) = 
    inherit ATransientMap()
    [<VolatileField>] 
    let mutable len : int = Math.Max(PersistentArrayMap.hashtableThreshold,a.Length)
    let kvs : obj[] = Array.zeroCreate len
    [<NonSerialized>][<VolatileField>] 
    let mutable owner : Thread = Thread.CurrentThread

    do 
        Array.Copy(a,kvs,a.Length)


    member private x.indexOfKey(key:obj) =
        let rec step (idx:int) =
            if idx >= len then -1
            elif PersistentArrayMap.equalKey(kvs.[idx],key) then idx
            else step (idx+2)
        step 0

    override x.ensureEditable() = if isNull owner then raise <| InvalidOperationException("Transient used after persistent! call")

    override x.doAssoc(k,v) = 
        let i = x.indexOfKey(k)
        if i >= 0
        then  // exists, overwrite value
            if kvs.[i+1] <> v then   kvs.[i+1] <- v
            upcast x
        elif len < kvs.Length
        then    // we have room to add
            kvs.[len] <- k
            kvs.[len+1] <- v
            len <- len+2
            upcast x
        else 
            (PersistentHashMap.create(kvs).asTransient():>ITransientMap).assoc(k,v)

    override x.doWithout(k) =
        let i = x.indexOfKey(k)
        if  i >= 0
        then  // exists, must remove
            if len >= 2
            then // move end pair
                kvs.[i] <- kvs.[kvs.Length-2]
                kvs.[i+1] <- kvs.[kvs.Length-1]
            len <- len-2
        upcast x

    override x.doValAt(k,nf) =
        let i = x.indexOfKey(k)
        if i >= 0 then kvs.[i+1] else nf

    override x.doCount() = len / 2

    override x.doPersistent() =
        x.ensureEditable()
        owner <- null
        let a = Array.zeroCreate len
        Array.Copy(kvs,a,len)
        upcast PersistentArrayMap(a)


   




                

