namespace Clojure.Collections

open System


[<AbstractClass>]
[<AllowNullLiteral>]
type Obj(m:IPersistentMap) =

    let mm = m
    new() = Obj(null)

    interface IMeta with
        member _.meta() = mm

    interface IObj with 
        member _.withMeta(m) = raise <| NotImplementedException("You must implement withMeta in derived classes")



// Needs to appear before the defintion of RT

[<Sealed>]
type Reduced(v) = 
    let value = v

    interface IDeref with
        member _.deref() = value

