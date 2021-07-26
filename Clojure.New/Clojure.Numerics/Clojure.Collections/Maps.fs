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


type PersistentArrayMap(m,a) =
    inherit APersistentMap()
    let meta : IPersistentMap = m
    let kvs : obj array = a
    
    new() = PersistentArrayMap(null,Array.zeroCreate 0)
    new(a) = PersistentArrayMap(null,a)

    static member private hashtableThreshold = 16
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

            



// public class PersistentArrayMap : APersistentMap, IObj, IEditableCollection, IMapEnumerable, IMapEnumerableTyped<Object,Object>, IEnumerable, IEnumerable<IMapEntry>, IKVReduce


//       /// <summary>
//       /// Compare two keys for equality.
//       /// </summary>
//       /// <param name="k1">The first key to compare.</param>
//       /// <param name="k2">The second key to compare.</param>
//       /// <returns></returns>
//       /// <remarks>Handles nulls properly.</remarks>
//       static bool EqualKey(object k1, object k2)
//       {
//           if (k1 is Keyword)
//               return k1 == k2;
//           return Util.equiv(k1, k2);
//       }

//       /// <summary>
//       /// Test if the map contains a key.
//       /// </summary>
//       /// <param name="key">The key to test for membership</param>
//       /// <returns>True if the key is in this map.</returns>
//       public override bool containsKey(object key)
//       {
//           return IndexOfKey(key) >= 0;
//       }

//       /// <summary>
//       /// Returns the key/value pair for this key.
//       /// </summary>
//       /// <param name="key">The key to retrieve</param>
//       /// <returns>The key/value pair for the key, or null if the key is not in the map.</returns>
//       public override IMapEntry entryAt(object key)
//       {
//           int i = IndexOfKey(key);
//           return i >= 0
//               ? (IMapEntry)MapEntry.create(_array[i], _array[i + 1])
//               : null;
//       }

//       /// <summary>
//       /// Gets the value associated with a key.
//       /// </summary>
//       /// <param name="key">The key to look up.</param>
//       /// <returns>The associated value. (Throws an exception if key is not present.)</returns>
//       public override object valAt(object key)
//       {
//           return valAt(key, null);
//       }

//       /// <summary>
//       /// Gets the value associated with a key.
//       /// </summary>
//       /// <param name="key">The key to look up.</param>
//       /// <param name="notFound">The value to return if the key is not present.</param>
//       /// <returns>The associated value (or <c>notFound</c> if the key is not present.</returns>
//       public override object valAt(object key, object notFound)
//       {
//           int i = IndexOfKey(key);
//           return i >= 0
//               ? _array[i + 1]
//               : notFound;
//       }

//       #endregion

//       #region IPersistentCollection members

//       /// <summary>
//       /// Gets the number of items in the collection.
//       /// </summary>
//       /// <returns>The number of items in the collection.</returns>
//       public override int count()
//       {
//           return _array.Length / 2;
//       }

//       /// <summary>
//       /// Gets an ISeq to allow first/rest iteration through the collection.
//       /// </summary>
//       /// <returns>An ISeq for iteration.</returns>
//       public override ISeq seq()
//       {
//           return _array.Length > 0
//               ? new Seq(_array, 0)
//               : null;
//       }

//       /// <summary>
//       /// Gets an empty collection of the same type.
//       /// </summary>
//       /// <returns>An emtpy collection.</returns>
//       public override IPersistentCollection empty()
//       {
//           return (IPersistentCollection)EMPTY.withMeta(meta());
//       }

//       #endregion

//       #region IPersistentMap members

//       /// <summary>
//       /// Add a new key/value pair.
//       /// </summary>
//       /// <param name="key">The key</param>
//       /// <param name="val">The value</param>
//       /// <returns>A new map with key+value added.</returns>
//       /// <remarks>Overwrites an exising value for the <paramref name="key"/>, if present.</remarks>
//       public override IPersistentMap assoc(object key, object val)
//       {
//           int i = IndexOfKey(key);
//           object[] newArray;
//           if (i >= 0)
//           {
//               // already have key, same sized replacement
//               if (_array[i + 1] == val) // no change, no-op
//                   return this;
//               newArray = (object[]) _array.Clone();
//               newArray[i + 1] = val;
//           }
//           else
//           {
//               // new key, grow
//               if (_array.Length >= HashtableThreshold)
//                   return createHT(_array).assoc(key, val);
//               newArray = new object[_array.Length + 2];
//               if (_array.Length > 0)
//                   Array.Copy(_array, 0, newArray, 0, _array.Length);
//               newArray[newArray.Length-2] = key;
//               newArray[newArray.Length - 1] = val;
//           }
//           return create(newArray);
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

//       /// <summary>
//       /// Add a new key/value pair.
//       /// </summary>
//       /// <param name="key">The key</param>
//       /// <param name="val">The value</param>
//       /// <returns>A new map with key+value added.</returns>
//       /// <remarks>Throws an exception if <paramref name="key"/> has a value already.</remarks>
//       public override IPersistentMap assocEx(object key, object val)
//       {
//           int i = IndexOfKey(key);
//           if (i >= 0)
//               throw new InvalidOperationException("Key already present.");
//           return assoc(key, val);
//       }

//       /// <summary>
//       /// Remove a key entry.
//       /// </summary>
//       /// <param name="key">The key to remove</param>
//       /// <returns>A new map with the key removed (or the same map if the key is not contained).</returns>
//       public override IPersistentMap without(object key)
//       {
//           int i = IndexOfKey(key);
//           if (i >= 0)
//           {
//               // key exists, remove
//               int newlen = _array.Length - 2;
//               if (newlen == 0)
//                   return (IPersistentMap)empty();
//               object[] newArray = new object[newlen];
//               Array.Copy(_array, 0, newArray, 0, i);
//               Array.Copy(_array,i+2,newArray,i,newlen-i);
//               return create(newArray);
//           }
//           else
//               return this;             
//       }

//       #endregion

      
//       /// <summary>
//       /// Internal class providing an <see cref="ISeq">ISeq</see> 
//       /// for <see cref="PersistentArrayMap">PersistentArrayMap</see>s.
//       /// </summary>
//       [Serializable]
//       protected sealed class Seq : ASeq, Counted
//       {
//           #region Data

//           /// <summary>
//           /// The array to iterate over.
//           /// </summary>
//           private readonly object[] _array;

//           /// <summary>
//           /// Current index position in the array.
//           /// </summary>
//           private readonly int _i;

//           #endregion

//           #region C-tors & factory methods

//           /// <summary>
//           /// Initialize the sequence to a given array and index.
//           /// </summary>
//           /// <param name="array">The array being sequenced over.</param>
//           /// <param name="i">The current index.</param>
//           public Seq(object[] array, int i)
//           {
//               _array = array;
//               _i = i;
//           }

//           /// <summary>
//           /// Initialize the sequence with given metatdata and array/index.
//           /// </summary>
//           /// <param name="meta">The metadata to attach.</param>
//           /// <param name="array">The array being sequenced over.</param>
//           /// <param name="i">The current index.</param>
//           public Seq(IPersistentMap meta, object[] array, int i)
//               : base(meta)
//           {
//               _array = array;
//               _i = i;
//           }

//           #endregion

//           #region ISeq members

//           /// <summary>
//           /// Gets the first item.
//           /// </summary>
//           /// <returns>The first item.</returns>
//           public override object first()
//           {
//               return MapEntry.create(_array[_i], _array[_i + 1]);
//           }

//           /// <summary>
//           /// Return a seq of the items after the first.  Calls <c>seq</c> on its argument.  If there are no more items, returns nil."
//           /// </summary>
//           /// <returns>A seq of the items after the first, or <c>nil</c> if there are no more items.</returns>
//           public override ISeq next()
//           {
//               return _i + 2 < _array.Length
//                   ? new Seq(_array, _i + 2)
//                   : null;
//           }

//           #endregion

//           #region IPersistentCollection members
//           /// <summary>
//           /// Gets the number of items in the collection.
//           /// </summary>
//           /// <returns>The number of items in the collection.</returns>
//           public override int count()
//           {
//               return (_array.Length - _i) / 2;
//           }

//           #endregion

//           #region IObj members

//           /// <summary>
//           /// Create a copy with new metadata.
//           /// </summary>
//           /// <param name="meta">The new metadata.</param>
//           /// <returns>A copy of the object with new metadata attached.</returns>
//           public override IObj withMeta(IPersistentMap meta)
//           {
//               if (_meta == meta)
//                   return this;

//               return new Seq(meta, _array, _i);
//           }

//           #endregion

//       }

//       #region IEditableCollection Members

//       public ITransientCollection asTransient()
//       {
//           return new TransientArrayMap(_array);
//       }

//       #endregion

//       #region TransientArrayMap class

//       sealed class TransientArrayMap : ATransientMap
//       {
//           #region Data

//           volatile int _len;
//           readonly object[] _array;
           
//           [NonSerialized] volatile Thread _owner;

//           #endregion

//           #region Ctors


//           public TransientArrayMap(object[] array)
//           {
//               _owner = Thread.CurrentThread;
//               _array = new object[Math.Max(HashtableThreshold, array.Length)];
//               Array.Copy(array, _array, array.Length);
//               _len = array.Length;
//           }

//           #endregion

//           #region

//           /// <summary>
//           /// Gets the index of the key in the array.
//           /// </summary>
//           /// <param name="key">The key to search for.</param>
//           /// <returns>The index of the key if found; -1 otherwise.</returns>
//           private int IndexOfKey(object key)
//           {
//               for (int i = 0; i < _len; i += 2)
//                   if (EqualKey(_array[i], key))
//                       return i;
//               return -1;
//           }

//           protected override void EnsureEditable()
//           {
//               if (_owner == null )
//                   throw new InvalidOperationException("Transient used after persistent! call");
//           }

//           protected override ITransientMap doAssoc(object key, object val)
//           {
//               int i = IndexOfKey(key);
//               if (i >= 0) //already have key,
//               {
//                   if (_array[i + 1] != val) //no change, no op
//                       _array[i + 1] = val;
//               }
//               else //didn't have key, grow
//               {
//                   if (_len >= _array.Length)
//                       return ((ITransientMap)PersistentHashMap.create(_array).asTransient()).assoc(key, val);
//                   _array[_len++] = key;
//                   _array[_len++] = val;
//               }
//               return this;
//           }


//           protected override ITransientMap doWithout(object key)
//           {
//               int i = IndexOfKey(key);
//               if (i >= 0) //have key, will remove
//               {
//                   if (_len >= 2)
//                   {
//                       _array[i] = _array[_len - 2];
//                       _array[i + 1] = _array[_len - 1];
//                   }
//                   _len -= 2;
//               }
//               return this;
//           }

//           protected override object doValAt(object key, object notFound)
//           {
//               int i = IndexOfKey(key);
//               if (i >= 0)
//                   return _array[i + 1];
//               return notFound;
//           }

//           protected override int doCount()
//           {
//               return _len / 2;
//           }

//           protected override IPersistentMap doPersistent()
//           {
//               EnsureEditable();
//               _owner = null;
//               object[] a = new object[_len];
//               Array.Copy(_array, a, _len);
//               return new PersistentArrayMap(a);
//           }

//           #endregion
//       }

//       #endregion

//       #region kvreduce

//       public object kvreduce(IFn f, object init)
//       {
//           for (int i = 0; i < _array.Length; i += 2)
//           {
//               init = f.invoke(init, _array[i], _array[i + 1]);
//               if (RT.isReduced(init))
//                   return ((IDeref)init).deref();
//           }
//           return init;
//       }

//       #endregion

//       #region IMapEnumerable, IMapEnumerableTyped, IEnumerable ...

//       public IEnumerator keyEnumerator()
//       {
//           return tkeyEnumerator();
//       }

//       public IEnumerator valEnumerator()
//       {
//           return tvalEnumerator();
//       }

//       public IEnumerator<object> tkeyEnumerator()
//       {
//           for (int i = 0; i < _array.Length; i += 2)
//               yield return _array[i];
//       }

//       public IEnumerator<object> tvalEnumerator()
//       {
//           for (int i = 0; i < _array.Length; i += 2)
//               yield return _array[i + 1];
//       }


//       public override IEnumerator<KeyValuePair<object, object>> GetEnumerator()
//       {
//           for (int i = 0; i < _array.Length; i += 2)
//               yield return new KeyValuePair<object, object>(_array[i], _array[i + 1]);
//       }

//       IEnumerator IEnumerable.GetEnumerator()
//       {
//           return ((IEnumerable<IMapEntry>)this).GetEnumerator();
//       }

//       IEnumerator<IMapEntry> IEnumerable<IMapEntry>.GetEnumerator()
//       {
//           for (int i = 0; i < _array.Length; i += 2)
//               yield return (IMapEntry)MapEntry.create(_array[i], _array[i + 1]);
//       }

//       #endregion

//   }
//}

//       /// <summary>
//       /// Create a <see cref="PersistentArrayMap">PersistentArrayMap</see> (if small enough, else create a <see cref="PersistentHashMap">PersistentHashMap</see>.
//       /// </summary>
//       /// <param name="other">The BCL map to initialize from</param>
//       /// <returns>A new persistent map.</returns>
//       [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "ClojureJVM name match")]
//       public static IPersistentMap create(IDictionary other)
//       {
//           ITransientMap ret = (ITransientMap)EMPTY.asTransient();
//           foreach (DictionaryEntry de in other)
//               ret = ret.assoc(de.Key, de.Value);
//           return ret.persistent();

//       }

//       /// <summary>
//       /// Create a <see cref="PersistentArrayMap">PersistentArrayMap</see> with new data but same metadata as the current object.
//       /// </summary>
//       /// <param name="init">The new key/value array</param>
//       /// <returns>A new <see cref="PersistentArrayMap">PersistentArrayMap</see>.</returns>
//       /// <remarks>The array is used directly.  Do not modify externally or immutability is sacrificed.</remarks>
//       [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "ClojureJVM name match")]
//       PersistentArrayMap create(params object[] init)
//       {
//           return new PersistentArrayMap(meta(), init);
//       }


//       [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "ClojureJVM name match")]
//       public static PersistentArrayMap createWithCheck(Object[] init)
//       {
//           for (int i = 0; i < init.Length; i += 2)
//           {
//               for (int j = i + 2; j < init.Length; j += 2)
//               {
//                   if (EqualKey(init[i], init[j]))
//                       throw new ArgumentException("Duplicate key: " + init[i]);
//               }
//           }
//           return new PersistentArrayMap(init);
//       }


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

