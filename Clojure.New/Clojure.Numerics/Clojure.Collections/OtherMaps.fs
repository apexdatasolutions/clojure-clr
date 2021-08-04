namespace Clojure.Collections

open System
open System.Collections
open System.Collections.Generic
open Clojure.Collections


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
        member x.GetEnumerator() =
            let s = 
                seq {
                        for i in 0 .. 2 .. (kvs.Length-1) do    
                            yield MapEntry.create(kvs.[i],kvs.[i+1]) :> IMapEntry
                    }
            s.GetEnumerator()

    interface IEnumerable with
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


