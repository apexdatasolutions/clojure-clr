
namespace SimpleCollections

open Clojure.Collections
open System.Collections
open System.Collections.Generic
open System

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

    static member makeConsSeq (n:int) =
        let mutable (c:ISeq) = SimpleEmptySeq() :> ISeq
        for i = n-1 downto 0 do
            c <- c.cons(i)
        c
    
        
   
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

[<AllowNullLiteral>]
type SimpleMapEntry(k,v) =
    let key : obj = k
    let value : obj = v

    interface IMapEntry with
        member x.key() = key
        member x.value() = value


[<AllowNullLiteral>]
type SimpleMap(ks,vs) =
    let keys : obj list = ks
    let vals : obj list = vs
    new() = SimpleMap(List.Empty,List.Empty)

    static member mapCompare(m1:IPersistentMap,o:obj) : bool =
        if obj.ReferenceEquals(m1,o) then true
        else match o with
            | :? IPersistentMap as m2 ->
                if m1.count() <> m2.count() then false
                else 
                    let rec step (s:ISeq) =
                        if isNull s then false
                        else 
                            let me : IMapEntry = downcast s.first()
                            if m2.containsKey me.key && m2.valAt(me.key).Equals(me.value)
                            then step (s.next())
                            else false
                    step (m1.seq())
        

    interface IPersistentCollection with
        member x.count() = (x:>IPersistentMap).count()
        member x.cons(o) = (x:>IPersistentMap).cons(o) :> IPersistentCollection

        member x.empty() = SimpleMap() :> IPersistentCollection
        member x.equiv(o) = SimpleMap.mapCompare(x,o)

    interface Seqable with 
        member x.seq() = invalidOp "implement"

    interface ILookup with
        member x.valAt(key) =
            match  List.tryFindIndex (fun k -> k = key) keys with
            | Some idx -> vals.Item(idx)
            | None -> null
        member x.valAt(key,notFound) =
            match  List.tryFindIndex (fun k -> k = key) keys with
            | Some idx -> vals.Item(idx)
            | None -> notFound

    interface Associative with
        member x.containsKey(key) = List.contains key keys
        member x.entryAt(key) = 
            if (x:>Associative).containsKey key
            then SimpleMapEntry(key,(x:>ILookup).valAt(key)) :> IMapEntry
            else null
        member x.assoc(k,v) = upcast (x:>IPersistentMap).assoc(k,v) 

    interface Counted with
        member x.count() = keys.Length

    interface IEnumerable<IMapEntry> with 
        member x.GetEnumerator() : IEnumerator<IMapEntry> = 
            (seq { for i = 0 to keys.Length-1 do yield SimpleMapEntry(keys.Item(i),vals.Item(i)) :> IMapEntry}).GetEnumerator()
    interface IEnumerable with
        member x.GetEnumerator() : IEnumerator = upcast (x:>IEnumerable<IMapEntry>).GetEnumerator() 
            
 
  
    interface IPersistentMap with
        member x.assoc(k,v) = 
            if (x:>Associative).containsKey k
            then (x:>IPersistentMap).without(k).assoc(k,v)  // not the most efficient way, but who cares?
            else SimpleMap(k::keys,v::vals) :>IPersistentMap
        member x.assocEx(k,v) = 
            if (x:>Associative).containsKey(k) 
            then raise <| InvalidOperationException("Key already present.")
            else (x:>IPersistentMap).assoc(k,v)
        member x.without(key) =
            match  List.tryFindIndex (fun k -> k = key) keys with
            | Some idx -> 
                let keysHead, keysTail = List.splitAt idx keys
                let valsHead, valsTail = List.splitAt idx vals
                SimpleMap(keysHead@keysTail.Tail,valsHead@valsTail.Tail) :> IPersistentMap
            | None -> x :> IPersistentMap
        member x.cons(o) = 
            match o with
            | :? IMapEntry as me ->  (x:>IPersistentMap).assoc(me.key(),me.value())
            | _ -> raise <| InvalidOperationException("Can only cons an IMapEntry to this map")
        member x.count() = keys.Length
        
    static member makeSimpleMap (n:int) =
        let keys = seq { for c in 'a' .. 'z' -> box c } |> Seq.take n |> Seq.toList
        let vals = seq { for c in 'A' .. 'Z' -> box c } |> Seq.take n |> Seq.toList
        SimpleMap(keys,vals)


        