
namespace SimpleCollections

open Clojure.Collections
open System.Collections
open System.Collections.Generic
open System

// I went to this degree of elaboration so I could use this as a general example of implementation of a sequence for tutorial purposes

module Util = 

    let checkEquals o1 o2 = obj.ReferenceEquals(o1,o2) || not (isNull o1) && o1.Equals(o2)
    let rec seqEquals (s1:ISeq) (s2:ISeq) =
        match s1, s2 with   
        | null, null -> true
        | null, _ -> false
        | _, null -> false
        | _ ->  checkEquals (s1.first()) (s2.first()) && seqEquals (s1.next()) (s2.next())
    let seqEquiv s1 s2 = seqEquals s1 s2
    let seqCount (s:ISeq) = 
        let rec step (s:ISeq) cnt = if isNull s then cnt else step (s.next()) (cnt+1)
        step s 0
    let getHashCode (s:ISeq) = 
        let combine hc x = 31*hc + if isNull x then 0 else x.GetHashCode()
        let rec step (s:ISeq) hc = if isNull s then hc else step (s.next()) (combine hc (s.first()))
        step s 1
    let rec seqToString (s:ISeq) = 
        let itemToString (o:obj) =
            match o with
            | :? Seqable as s -> seqToString (s.seq())
            | _ -> o.ToString()
        let rec itemsToString (s:ISeq) =
            if isNull s then "" else (itemToString (s.first())) + (itemsToString (s.next()))  
        if isNull s then "nil" else "(" + (itemsToString s) + ")"        

type SimpleCons(head: obj, tail: ISeq ) =
    // I had to restrain myself from calling these car & cdr  
 
    interface ISeq with
        member _.first() = head
        member this.next() = (this:>ISeq).more().seq()
        member _.more() =  if isNull tail then upcast SimpleEmptySeq() else tail 
        member this.cons(o) = upcast SimpleCons(o,this)

    interface IPersistentCollection with
        member _.count() = 1 + Util.seqCount tail
        member this.cons(o) = upcast (this:>ISeq).cons(o)
        member _.empty() = upcast SimpleEmptySeq() 
        member this.equiv(o) =
            match o with
            | :? Seqable as s -> Util.seqEquiv (this:>ISeq) (s.seq())
            | _ -> false

    interface Seqable with
        member this.seq() = upcast this

    override this.Equals(o) = 
        match o with
        | :? Seqable as s -> Util.seqEquals (this:>ISeq) (s.seq())
        | _ -> false

    override this.GetHashCode() = Util.getHashCode this

    override this.ToString() = Util.seqToString this

    static member makeConsSeq (n:int) =
        let mutable (c:ISeq) = upcast SimpleEmptySeq()
        for i = n-1 downto 0 do
            c <- c.cons(i)
        c
    
        
   
and  SimpleEmptySeq() =

    interface ISeq with 
        member _.first() = null
        member _.next() = null
        member this.more() = upcast this
        member this.cons(o) = upcast SimpleCons(o,this)

    interface IPersistentCollection with
        member _.count() = 0
        member this.cons(o) = upcast (this:>ISeq).cons(o)
        member this.empty() = upcast this
        member this.equiv(o) = this.Equals(o)

    interface Seqable with
        member x.seq() = null

    override x.Equals(o) = 
        match o with
        | :? Seqable as s ->  s.seq() |> isNull
        | _ -> false

    override x.GetHashCode() = 1
       
    override x.ToString() = "()"

// Make a super-simple Range sequence to implement ISeq
[<AllowNullLiteral>]
type SimpleRange(startVal: int, endVal: int) =
 
    interface ISeq with
        member _.first() = upcast startVal 
        member this.next() = (this:>ISeq).more().seq()
        member this.more() = if startVal = endVal then upcast SimpleEmptySeq() else upcast SimpleRange(startVal+1,endVal)
        member this.cons(o) = SimpleCons(o,(this:>ISeq)) :> ISeq

    interface IPersistentCollection with
        member _.count() = endVal-startVal+1
        member this.cons(o) = upcast (this:>ISeq).cons(o) 
        member _.empty() = upcast SimpleEmptySeq()
        member this.equiv(o) = 
            match o with
            | :? Seqable as s -> Util.seqEquiv (this:>ISeq) (s.seq())
            | _ -> false

    interface Seqable with
        member this.seq() = (this:>ISeq)

    override this.Equals(o) = 
        match o with
        | :? Seqable as s -> Util.seqEquals (this:>ISeq) (s.seq())
        | _ -> false

    override this.GetHashCode() = Util.getHashCode this

    override this.ToString() = Util.seqToString this

[<AllowNullLiteral>]
type SimpleMapEntry(key:obj, value:obj) =

    interface IMapEntry with
        member _.key() = key
        member _.value() = value


[<AllowNullLiteral>]
type SimpleMap(keys: obj list, vals: obj list) =
 
    new() = SimpleMap(List.Empty,List.Empty)

    static member mapCompare(m1:IPersistentMap,o:obj) : bool =
        if obj.ReferenceEquals(m1,o) then true
        else 
            match o with
            | :? IPersistentMap as m2 ->
                if m1.count() <> m2.count() then false
                else 
                    let rec step (s:ISeq) =
                        if isNull s then 
                            true
                        else 
                            let me : IMapEntry = downcast s.first()
                            if m2.containsKey (me.key()) && m2.valAt(me.key()).Equals(me.value())
                            then step (s.next())
                            else false
                    step (m1.seq())
            | _ -> false
        

    interface IPersistentCollection with
        member this.count() = (this:>IPersistentMap).count()
        member this.cons(o) = (this:>IPersistentMap).cons(o) :> IPersistentCollection

        member _.empty() = SimpleMap() :> IPersistentCollection
        member this.equiv(o) = SimpleMap.mapCompare(this,o)

    interface Seqable with 
        member _.seq() = upcast SimpleMapSeq(keys,vals)

    interface ILookup with
        member _.valAt(key) =
            match  List.tryFindIndex (fun k -> k = key) keys with
            | Some idx -> vals.Item(idx)
            | None -> null
        member _.valAt(key,notFound) =
            match  List.tryFindIndex (fun k -> k = key) keys with
            | Some idx -> vals.Item(idx)
            | None -> notFound

    interface Associative with
        member _.containsKey(key) = List.contains key keys
        member this.entryAt(key) = 
            if (this:>Associative).containsKey key
            then SimpleMapEntry(key,(this:>ILookup).valAt(key)) :> IMapEntry
            else null
        member this.assoc(k,v) = upcast (this:>IPersistentMap).assoc(k,v) 

    interface Counted with
        member _.count() = keys.Length

    interface IEnumerable<IMapEntry> with 
        member _.GetEnumerator() : IEnumerator<IMapEntry> = 
            (seq { for i = 0 to keys.Length-1 do yield SimpleMapEntry(keys.Item(i),vals.Item(i)) :> IMapEntry}).GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator = upcast (this:>IEnumerable<IMapEntry>).GetEnumerator() 
            
    interface IPersistentMap with
        member this.assoc(k,v) = 
            if (this:>Associative).containsKey k
            then (this:>IPersistentMap).without(k).assoc(k,v)  // not the most efficient way, but who cares?
            else SimpleMap(k::keys,v::vals) :>IPersistentMap
        member this.assocEx(k,v) = 
            if (this:>Associative).containsKey(k) 
            then raise <| InvalidOperationException("Key already present.")
            else (this:>IPersistentMap).assoc(k,v)
        member this.without(key) =
            match  List.tryFindIndex (fun k -> k = key) keys with
            | Some idx -> 
                let keysHead, keysTail = List.splitAt idx keys
                let valsHead, valsTail = List.splitAt idx vals
                SimpleMap(keysHead@keysTail.Tail,valsHead@valsTail.Tail) :> IPersistentMap
            | None -> this :> IPersistentMap
        member this.cons(o) = 
            match o with
            | :? IMapEntry as me ->  (this:>IPersistentMap).assoc(me.key(),me.value())
            | _ -> raise <| InvalidOperationException("Can only cons an IMapEntry to this map")
        member _.count() = keys.Length
        
    static member makeSimpleMap (n:int) =
        let keys = seq { for c in 'a' .. 'z' -> box c } |> Seq.take n |> Seq.toList
        let vals = seq { for c in 'A' .. 'Z' -> box c } |> Seq.take n |> Seq.toList
        SimpleMap(keys,vals)

and SimpleMapSeq(keys: obj list, vals: obj list) =

    interface Seqable with
        member this.seq() = upcast this

    interface IPersistentCollection with
        member _.count() = List.length keys 
        member this.cons(o) = upcast SimpleCons(o,this)
        member _.empty() = upcast SimpleEmptySeq()
        member this.equiv(o) = 
            match o with
            | :? Seqable as s -> Util.seqEquiv this (s.seq())
            | _ -> false

    interface ISeq with
        member _.first() = upcast SimpleMapEntry(keys.Head,vals.Head)
        member _.next() = if keys.Length <= 1 then null else upcast SimpleMapSeq(keys.Tail,vals.Tail)
        member this.more() = 
            match (this:>ISeq).next() with
            | null -> upcast SimpleEmptySeq()
            | _ as s -> s
        member this.cons(o) = upcast SimpleCons(o,this)
        
    interface Sequential


module private SHMNOdeOps =

    let cloneAndSet(arr : 'T[], i: int, a : 'T) : 'T[] =
        let clone : 'T[] = downcast arr.Clone()
        clone.[i] <- a
        clone


    let cloneAndSet2(arr: 'T[], i: int, a: 'T, j: int, b: 'T ) : 'T[] =
        let clone : 'T[] = downcast arr.Clone()
        clone.[i] <- a
        clone.[j] <- b
        clone

    let removePair(arr: 'T[], i: int) : 'T[] =
        let newArr : 'T[] = Array.zeroCreate <| arr.Length-2 
        Array.Copy(arr,0,newArr,0,2*i)
        Array.Copy(arr,2*(i+1),newArr,2*i,newArr.Length-2*i)
        newArr

    let getHash(o:obj) : int =
        match o with 
        | null -> 0
        | _ -> o.GetHashCode()

    let mask(hash,shift) = (hash >>> shift) &&& 0x01f

    let bitPos(hash, shift) = 1 <<< mask(hash,shift)
        
    let bitCount(x) =
        let x = x-((x >>> 1) &&& 0x55555555);
        let x = (((x >>> 2) &&& 0x33333333) + (x &&& 0x33333333))
        let x = (((x >>> 4) + x) &&& 0x0f0f0f0f)
        (x * 0x01010101) >>> 24

    let bitIndex(bitmap,bit) = bitCount(bitmap &&& (bit-1))

    let hashToIndex (hash:int) (shift:int) (bitmap:int) : int option =
        let bit = bitPos(hash,shift)
        if bit &&& bitmap = 0 then None else bitIndex(bitmap,bit) |> Some

    let pcequiv(k1:obj, k2:obj) =
        match k1, k2 with
        | :? IPersistentCollection as pc1, _ -> pc1.equiv(k2)
        | _, (:? IPersistentCollection as pc2) -> pc2.equiv(k1) 
        | _ -> k1.Equals(k2)    

    let equiv(k1:obj, k2:obj) =
        if Object.ReferenceEquals(k1,k2) then true
        elif isNull k1 then false
        else pcequiv(k1,k2)

open SHMNOdeOps

type Box(init) =
    let mutable value : bool = init
    new() = Box(false)

    member _.set() = value <- true
    member _.reset() = value <- false
    member _.isSet = value
    member _.isNotSet = not value


type  BNodeEntry = | KeyValue of Key:obj * Value:obj | Node of Node:SHMNode

and  SHMNode = 
    | ArrayNode of Count:int * Nodes : (SHMNode option)[]
    | BitmapNode of Bitmap:int * Entries : BNodeEntry[]
    | CollisionNode of Hash:int * Count: int * KVs: MapEntry[]  

    static member EmptyBitmapNode = BitmapNode(0,Array.empty<BNodeEntry>)

    static member tryFindCNodeIndex(key:obj, kvs:MapEntry[]) =
        kvs 
        |> Array.tryFindIndex (fun kv -> equiv((kv:>IMapEntry).key(),key))

    static member createNode (shift:int) (key1:obj) (val1:obj) (key2hash:int) (key2:obj) (val2:obj) :  SHMNode =
        let key1hash = hash(key1)
        if key1hash = key2hash then
            CollisionNode(key1hash,2,[|MapEntry(key1,val1);MapEntry(key2,val2)|])
        else
            let box = Box()
            let n1 = SHMNode.EmptyBitmapNode.assoc shift key1hash key1 val1 box
            n1.assoc shift key2hash key2 val2 box

    member this.getNodeSeq() = 
        match this with
        | ArrayNode(Count=count; Nodes=nodes) -> raise <| NotImplementedException() 
        | BitmapNode(Bitmap=bitmap; Entries=entries) -> raise <| NotImplementedException() 
        | CollisionNode(Hash=hash; Count=count; KVs=kvs) -> raise <| NotImplementedException() 
    
    member this.find (shift:int) (hash:int) (key:obj) : IMapEntry option =
        match this with
        | ArrayNode(Count=count; Nodes=nodes) -> 
            let idx = mask(hash,shift)
            match nodes.[idx] with
            | None -> None
            | Some node -> node.find (shift+5) hash key
        | BitmapNode(Bitmap=bitmap; Entries=entries) -> 
            match hashToIndex hash shift bitmap with
            | None -> None
            | Some idx ->
                match entries.[idx] with
                | KeyValue(Key=k; Value=v) -> if equiv(key,k) then (MapEntry(k,v) :> IMapEntry) |> Some else None
                | Node(Node=node) -> node.find (shift+5) hash key
        | CollisionNode(Hash=hash; Count=count; KVs=kvs) -> 
            match SHMNode.tryFindCNodeIndex(key,kvs) with  
            | None -> None
            | Some idx -> Some (upcast kvs.[idx])


    member this.find2 (shift:int) (hash:int) (key:obj) (notFound:obj) : obj = 
        match this with
        | ArrayNode(Count=count; Nodes=nodes) -> 
            let idx = mask(hash,shift)
            match nodes.[idx] with
            | None -> notFound
            | Some node -> node.find2 (shift+5) hash key notFound

        | BitmapNode(Bitmap=bitmap; Entries=entries) ->
            match hashToIndex hash shift bitmap with
            | None -> notFound
            | Some idx ->
                match entries.[idx] with
                |KeyValue(Key=k; Value=v) -> if equiv(key,k) then v else notFound
                |Node(Node=node) -> node.find2 (shift+5) hash key notFound
        
        | CollisionNode(Hash=hash; Count=count; KVs=kvs) -> 
            match SHMNode.tryFindCNodeIndex(key,kvs) with  
            | None -> notFound
            | Some idx -> (kvs.[idx]:>IMapEntry).value()

    member this.assoc (shift:int) (hash:int) (key:obj) (value:obj) (addedLeaf:Box) : SHMNode =
        match this with
        | ArrayNode(Count=count ;Nodes=nodes) -> 
            let idx = mask(hash,shift)
            match nodes.[idx] with
            | None -> 
                let newNode = SHMNode.EmptyBitmapNode.assoc (shift+5) hash key value addedLeaf
                ArrayNode(count+1,cloneAndSet(nodes,idx,Some newNode))
            | Some node ->
                let newNode = node.assoc (shift+5) hash key value addedLeaf
                if newNode = node then  this else  ArrayNode(count,cloneAndSet(nodes,idx,Some newNode))

        | BitmapNode(Bitmap=bitmap; Entries=entries) -> 
            match hashToIndex hash shift bitmap with
            | None ->
                let n = bitCount(bitmap)
                if n >= 16 then
                    let nodes : SHMNode option [] = Array.zeroCreate 32   // shoudl be Emmpty?
                    let jdx = mask(hash,shift)  
                    nodes.[jdx] <- SHMNode.EmptyBitmapNode.assoc (shift+5) hash key value addedLeaf |> Some
                    let mutable j = 0
                    for i = 0 to 31 do
                        if ((bitmap>>>i) &&& 1) <> 0 then   
                            nodes.[i] <- 
                                match entries.[j] with 
                                | KeyValue(Key=k; Value=v) -> SHMNode.EmptyBitmapNode.assoc (shift+5) (getHash k) k v addedLeaf |> Some
                                | Node(Node=node) -> node |> Some
                        j <- j+2
                    ArrayNode(n+1,nodes)

                else
                    let bit = bitPos(hash,shift)
                    let idx = bitIndex(bitmap,bit)
                    let newArray : BNodeEntry[] = Array.zeroCreate (n+1)
                    Array.Copy(entries,0,newArray,0,idx)
                    newArray.[idx] <- KeyValue(key,value)
                    Array.Copy(entries,idx,newArray, idx+1,n-idx)
                    addedLeaf.set()
                    BitmapNode((bitmap ||| bit), newArray)

            | Some idx ->
                let entry = entries.[idx]
                match entry with
                | KeyValue(Key=k; Value=v) ->                    
                    if equiv(key,k) then
                        if value = v  then
                            this
                        else
                            BitmapNode(bitmap,cloneAndSet(entries,idx+1,KeyValue(key,value)))
                    else
                        addedLeaf.set()
                        let newNode =  SHMNode.createNode (shift+5) k v hash key value
                        BitmapNode(bitmap,cloneAndSet(entries,idx,Node(newNode)))
                | Node(Node=node) -> 
                    let newNode = node.assoc (shift+5) hash key value addedLeaf
                    if  newNode = node then 
                        this
                    else
                        BitmapNode(bitmap,cloneAndSet(entries,idx,Node(newNode)))

        | CollisionNode(Hash=h;Count=count; KVs=kvs) -> 
            if hash = h then
                match SHMNode.tryFindCNodeIndex(key,kvs) with
                | Some idx ->
                    let kv = kvs.[idx] :> IMapEntry
                    if kv.value() = value then
                        this
                    else
                        CollisionNode(hash,count,cloneAndSet(kvs,idx,MapEntry(key,value)))
                | None ->
                    let newArray : MapEntry[] = count+1 |> Array.zeroCreate
                    Array.Copy(kvs,0,newArray,0,count)
                    newArray.[count] <- MapEntry(key,value)
                    addedLeaf.set()
                    CollisionNode(hash,count+1,newArray)   
            else    
                BitmapNode(bitPos(hash,shift),[| Node(this) |]).assoc shift h key value addedLeaf

    // TODO: do this with sequence functions?
    static member pack (count: int) (nodes: SHMNode option []) (idx: int) : SHMNode =
        let newArray : BNodeEntry [] = count-1 |> Array.zeroCreate 
        let mutable j = 0
        let mutable bitmap = 0
        for i=0 to idx-1 do
            match nodes.[i] with
            | None -> ()
            | Some n -> 
                newArray.[j] <- Node n; 
                bitmap <- bitmap ||| 1 <<< i
                j <- j+1
        for i=idx+1 to nodes.Length-1 do
            match nodes.[i] with
            | None -> ()
            | Some n -> 
                newArray.[j] <- Node n; 
                bitmap <- bitmap ||| 1 <<< i
                j <- j+1
        BitmapNode(bitmap,newArray)
        

    member this.without (shift:int) (hash:int) (key:obj) : SHMNode option =     // Probably needs to return an option
        match this with
        | ArrayNode(Count=count; Nodes=nodes) -> 
            let idx = mask(hash,shift)
            match nodes.[idx] with
            | None -> this |> Some
            | Some node -> 
                match node.without (shift+5) hash key with
                | None ->  // this branch got deleted
                    if count <= 8 then SHMNode.pack count nodes idx |> Some  // shrink
                    else ArrayNode(count-1,cloneAndSet(nodes,idx,None)) |> Some // zero out this entry
                | Some newNode ->
                    if newNode = node then this  |> Some
                    else ArrayNode(count,cloneAndSet(nodes,idx,Some newNode)) |> Some

        | BitmapNode(Bitmap=bitmap; Entries=entries) -> 
            match hashToIndex hash shift bitmap with
            | None -> this |> Some
            | Some idx ->
                let entry = entries.[idx]
                match entry with
                | KeyValue(Key=k; Value=v) -> 
                    if equiv(k,key) then
                        let bit = bitPos(hash,shift)
                        if bitmap = bit then // only one entry
                            None
                        else
                            BitmapNode(bitmap^^^bit,removePair(entries,idx)) |> Some
                    else this |> Some
                | Node(Node=node) ->
                    match node.without (shift+5) hash key with
                    | None -> this |> Some
                    | Some n ->
                        if n = node then this |> Some
                        else BitmapNode(bitmap,cloneAndSet(entries,idx,Node(n))) |> Some
            
        | CollisionNode(Hash=h; Count=count; KVs=kvs) -> 
            match SHMNode.tryFindCNodeIndex(key,kvs) with   
            | None -> this |> Some
            | Some idx ->
                if count = 1 then None else CollisionNode(h,count-1,removePair(kvs,idx)) |> Some

    
    member this.iteratorT (d:KVMangleFn<'T>) : IEnumerator<'T> = 
        match this with
        | ArrayNode(Nodes=nodes) -> 
            let s = 
                seq {
                    for onode in nodes do    
                        match onode with 
                        | None -> ()
                        | Some node ->
                            let ie = node.iteratorT(d)
                            while ie.MoveNext() do
                                yield ie.Current
                    }
            s.GetEnumerator()
        | BitmapNode(Bitmap=bitmap; Entries=entries) -> 
            let s = 
                seq {
                    for entry in entries do
                        match entry with
                        | KeyValue(Key=k;Value=v) ->
                            yield d(k,v)
                        | Node(Node=node) ->
                            let ie = node.iteratorT(d)
                            while ie.MoveNext() do yield ie.Current                
                    }
            s.GetEnumerator()
        | CollisionNode(Hash=h; Count=count; KVs=kvs) -> 
            let s = 
                seq {
                    for kv in kvs do
                        let me = kv :> IMapEntry
                        yield d(me.key(),me.value())                
                }
            s.GetEnumerator()       


type SimpleHashMap = 
    | Empty
    | Rooted of Count: int * Node: SHMNode

    static member notFoundValue = obj()

    interface Counted with
        member this.count() =
            match this with
            | Empty -> 0
            | Rooted(Count=c) -> c

    interface Seqable with
        member this.seq() = 
            match this with
            | Empty -> null
            | Rooted(Node=n) -> n.getNodeSeq()

    interface IPersistentCollection with
        member this.count() = (this:>Counted).count()
        member this.cons(o) = upcast (this:>IPersistentMap).cons(o)
        member this.empty() = upcast Empty
        member this.equiv(o) =  // a bit of a simplification from the Clojure/ClojureCLR version
            match o with
            | :? IDictionary as d ->
                if d.Count <> (this:>IPersistentCollection).count() then 
                    false
                else 
                    let rec step (s:ISeq) =                            
                        if isNull s then true
                        else 
                            let me : IMapEntry = downcast s.first()
                            if d.Contains(me.key()) && Util.equiv(me.value(),d.[me.key()]) then step (s.next())
                            else false
                    step ((this:>Seqable).seq())
            | _ -> false


    interface ILookup with
        member this.valAt(k) = (this:>ILookup).valAt(k,null)
        member this.valAt(k,nf) = 
            match this with
            | Empty -> nf
            | Rooted(Node=n) -> n.find2 0 (hash(k)) k nf 

    interface Associative with  
        member this.containsKey(k) =
            match this with
            | Empty -> false
            | Rooted(Node=n) -> (n.find2 0 (hash(k)) k SimpleHashMap.notFoundValue)  <> SimpleHashMap.notFoundValue
        member this.entryAt(k) = 
            match this with
            | Empty -> null
            | Rooted(Node=n) -> 
                match n.find 0 (hash(k)) k with
                | None -> null
                | Some me -> me
        member this.assoc(k,v) = upcast (this:>IPersistentMap).assoc(k,v)

    interface IPersistentMap with
        member this.count() = (this:>Counted).count()
        member this.assocEx(k,v) = 
            if (this:>Associative).containsKey(k) then raise <| InvalidOperationException("Key already present")
            (this:>IPersistentMap).assoc(k,v)

        member this.cons(o) = 
            match o with 
            | null -> upcast this
            | :? IMapEntry as e -> (this:>IPersistentMap).assoc(e.key(),e.value())
            | _ -> 
                let rec step (s:ISeq) (m:IPersistentMap) =
                    if isNull s then
                        m
                    else
                        let me = s.first() :?> IMapEntry
                        step (s.next()) (m.assoc(me.key(),me.value()))
                step (RT.seq(o)) this

        member this.assoc(k,v) = 
            let addedLeaf = Box()
            let rootToUse = 
                match this with
                | Empty -> SHMNode.EmptyBitmapNode
                | Rooted(Node=n) -> n
            let newRoot = rootToUse.assoc 0 (hash(k)) k v addedLeaf
            if newRoot = rootToUse then
                upcast this
            else
                let count = (this:>Counted).count()
                let updatedCount = if addedLeaf.isSet then count+1 else count


                upcast Rooted(updatedCount,newRoot)

        member this.without(k) = 
            match this with
            | Empty -> upcast this
            | Rooted(Count=c;Node=n) ->
                match  n.without 0 (hash(k)) k with
                | None -> upcast this
                | Some newRoot -> 
                    if newRoot = n then
                        upcast this
                    elif c = 1 then
                        upcast Empty
                    else
                        upcast Rooted(c-1,newRoot)

    member this.MakeEnumerator( d: KVMangleFn<Object> ) : IEnumerator =
        match this with
        | Empty -> upcast Seq.empty.GetEnumerator()
        | Rooted(Node=n) -> upcast n.iteratorT(d)

    member this.MakeEnumeratorT<'T>( d: KVMangleFn<'T> ) : IEnumerator<'T> =
        match this with
        | Empty -> Seq.empty.GetEnumerator()
        | Rooted(Node=n) -> n.iteratorT(d)

    interface IEnumerable<IMapEntry> with   
        member this.GetEnumerator() =  this.MakeEnumeratorT<IMapEntry> (fun (k, v) -> upcast MapEntry.create(k,v))
    
    interface IEnumerable with   
        member this.GetEnumerator() =  this.MakeEnumerator (fun (k, v) -> upcast MapEntry.create(k,v))
    
    interface IMapEnumerable with
        member this.keyEnumerator() = this.MakeEnumerator (fun (k, v) -> k)
        member this.valEnumerator() = this.MakeEnumerator (fun (k, v) -> v)
    
    //interface IFn with
    //    override this.invoke(arg1) = (this:>ILookup).valAt(arg1)
    //    override this.invoke(arg1,arg2) = (this:>ILookup).valAt(arg1,arg2)
   