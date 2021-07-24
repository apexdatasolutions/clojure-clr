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
       


//   [Serializable]
//   public abstract class APersistentMap: AFn, IPersistentMap, IDictionary, IEnumerable<IMapEntry>, MapEquivalence, IDictionary<Object,Object>, IHashEq
//   {





//       public bool ContainsKey(object key)
//       {
//           return containsKey(key);
//       }

//       public bool Contains(object key)
//       {
//           return this.containsKey(key);
//       }


//       public virtual IEnumerator<KeyValuePair<object, object>> GetEnumerator()
//       {
//           for (ISeq s = seq(); s != null; s = s.next())
//           {
//               IMapEntry entry = (IMapEntry)s.first();
//               yield return new KeyValuePair<object, object>(entry.key(), entry.val());
//           }
//       }

//       IEnumerator IEnumerable.GetEnumerator()
//       {
//           return ((IEnumerable<IMapEntry>)this).GetEnumerator();
//       }

//       IEnumerator<IMapEntry> IEnumerable<IMapEntry>.GetEnumerator()
//       {
//           for (ISeq s = seq(); s != null; s = s.next())
//               yield return (IMapEntry)s.first();
//       }

//       IDictionaryEnumerator IDictionary.GetEnumerator()
//       {
//           return new MapEnumerator(this);
//       }






//       static readonly object _missingValue = new object();

//       public bool TryGetValue(object key, out object value)
//       {
//           object found = valAt(key, _missingValue);
//           if ( found == _missingValue)
//           {
//               value = null;
//               return false;
//           }

//           value = found;
//           return true;
//       }

//       #endregion

//       #region ICollection Members

//       public void CopyTo(KeyValuePair<object, object>[] array, int arrayIndex)
//       {
//       }


