namespace Clojure.Collections

open System.Collections.Generic


type [<AllowNullLiteral>] Seqable =
    abstract seq : unit -> ISeq

and [<AllowNullLiteral>] IPersistentCollection = 
    inherit Seqable
    abstract count : unit -> int
    abstract cons: obj -> IPersistentCollection
    abstract empty: unit -> IPersistentCollection
    abstract equiv: obj -> bool

and [<AllowNullLiteral>] ISeq =
    inherit IPersistentCollection
    abstract first : unit -> obj
    abstract next : unit -> ISeq
    abstract more : unit -> ISeq
    abstract cons : obj -> ISeq

type ILookup = 
    abstract valAt : key:obj -> obj
    abstract valAt : key: obj * notFound: obj -> obj

type IMapEntry =
    abstract key : unit -> obj
    abstract value : unit -> obj

type Associative = 
    inherit IPersistentCollection
    inherit ILookup
    abstract containsKey : key: obj -> bool
    abstract entryAt : key: obj -> IMapEntry
    abstract assoc : key: obj * value: obj -> Associative

type Sequential = interface end

type Counted =
    abstract count : unit -> int

type Indexed =
    inherit Counted
    abstract nth : i: int -> obj
    abstract nth : i: int * notFound: obj -> obj

type Reversible = 
    abstract rseq: unit -> ISeq

type IPersistentMap =
    inherit Associative
    inherit IEnumerable<IMapEntry>
    inherit Counted
    abstract assoc : key: obj * value: obj -> IPersistentMap
    abstract assocEx : key: obj * value: obj -> IPersistentMap
    abstract without : key: obj -> IPersistentMap
    abstract cons: o:obj -> IPersistentMap
    abstract count: unit -> int  // do we need this?

type IPersistentSet =
    inherit IPersistentCollection
    inherit Counted
    abstract disjoin : key: obj -> IPersistentSet
    abstract contains: key: obj -> bool
    abstract get: key: obj -> obj
    abstract count: unit -> int    // do we need this?

type IPersistentStack =
    inherit IPersistentCollection
    abstract peek : unit -> obj
    abstract pop : unit -> IPersistentStack

type IPersistentList =
    inherit Sequential
    inherit IPersistentStack

type IPersistentVector =
    inherit Associative
    inherit Sequential
    inherit IPersistentStack
    inherit Reversible
    inherit Indexed
    abstract length : unit -> int
    abstract assocN : i: int * value: obj -> IPersistentVector
    abstract cons : o: obj -> IPersistentVector
    abstract count : unit -> int


type IMeta =
    abstract meta : unit -> IPersistentMap

type IObj =
    inherit IMeta
    abstract withMeta : meta: IPersistentVector -> IObj

