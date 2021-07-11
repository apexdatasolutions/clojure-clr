namespace Clojure.Collections

open System.Collections.Generic
open System
open Clojure.Fn




[<AllowNullLiteral>]
type ILookup = 
    abstract valAt : key:obj -> obj
    abstract valAt : key: obj * notFound: obj -> obj

type IMapEntry =
    abstract key : unit -> obj
    abstract value : unit -> obj

[<AllowNullLiteral>]
type Associative = 
    inherit IPersistentCollection
    inherit ILookup
    abstract containsKey : key: obj -> bool
    abstract entryAt : key: obj -> IMapEntry
    abstract assoc : key: obj * value: obj -> Associative

[<AllowNullLiteral>]
type Sequential = interface end

[<AllowNullLiteral>]
type Counted =
    abstract count : unit -> int

[<AllowNullLiteral>]
type Indexed =
    inherit Counted
    abstract nth : i: int -> obj
    abstract nth : i: int * notFound: obj -> obj

[<AllowNullLiteral>]
type Reversible = 
    abstract rseq: unit -> ISeq

[<AllowNullLiteral>]
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

[<AllowNullLiteral>]
type IPersistentStack =
    inherit IPersistentCollection
    abstract peek : unit -> obj
    abstract pop : unit -> IPersistentStack

type IPersistentList =
    inherit Sequential
    inherit IPersistentStack

[<AllowNullLiteral>]
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

type IHashEq =
    abstract hasheq : unit -> int

[<AllowNullLiteral>]
type IMeta =
    abstract meta : unit -> IPersistentMap

[<AllowNullLiteral>]
type IObj =
    inherit IMeta
    abstract withMeta : meta: IPersistentMap -> IObj

[<AbstractClass>][<AllowNullLiteral>]
type Obj(m:IPersistentMap) =

    let mm = m
    new() = Obj(null)

    interface IMeta with
        member x.meta() = mm

    interface IObj with 
        member x.withMeta(m:IPersistentMap) = raise <| NotImplementedException("Needs to be implemented in derived class")
        // I do not know how to indicate that Obj implements the IObj interface without providing an actual implementation.
        // Alternative -- if you base from Obj, remember to implement IObj.


type IDeref =
    abstract deref : unit -> obj

type IReduceInit =
    abstract reduce : IFn * obj -> obj

type IReduce =
    inherit IReduceInit
    abstract reduce : IFn -> obj


type Named =
    abstract getNamespace : unit -> string
    abstract getName : unit -> string

    