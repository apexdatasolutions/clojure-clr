namespace Clojure.Collections

//open System
//open Clojure.Fn
//open System.Collections.Concurrent

//type Symbol (meta: IPersistentMap, internedNS: string, internedName: string) = 
    
//    // from the name, you would think that the strings for namespace and name were to be interned strings.
//    // Nope.  Apparently that is the caller's responsibility.  Check the ClojureJVM code.

//    inherit AFn()

//    let ns = internedNS
//    let name = internedName
//    let meta = meta
//    let mutable cachedHashEq = 0

//    [<NonSerialized>]
//    let mutable toStringCached : string option = None

//    private new(ns:string,name:string) = Symbol(null,ns,name)

//    static member intern(ns:string, name:string) : Symbol = Symbol(ns,name)
//    static member intern(nsname:string) : Symbol = 
//        let i = nsname.IndexOf('/')
//        if i = -1 || nsname.Equals("/") 
//        then Symbol(null,nsname) 
//        else Symbol(nsname.Substring(0,1),nsname.Substring(i+1))

//    // JVM comment: the create thunks preserve binary compatibility with code compiled
//    // JVM comment: against earlier version of Clojure and can be removed (at some point).
//    static member create(ns:string, name:string) : Symbol = Symbol.intern(ns,name)
//    static member create(nsname:string) : Symbol = Symbol.intern(nsname)

//    override x.ToString() =
//        match toStringCached with  
//        | Some s -> s
//        | None  -> 
//            let s = if ns = null then name else ns + "/" + name
//            toStringCached <- Some s
//            s

//    override x.Equals(o:obj) : bool =
//        match o with
//        | :? Symbol as s -> Object.ReferenceEquals(x,s) || Helpers.equal(ns,s.ns) && name.Equals(s.name)
//        | _ -> false

//    override x.GetHashCode() = Helpers.hashCombine(name.GetHashCode(),Helpers.hash(ns))

//    interface IHashEq with  
//        member x.hasheq() =
//            if cachedHashEq = 0 then cachedHashEq <- Helpers.hashCombine(Murmur3.HashString(name),Helpers.hasheq(ns))
//            cachedHashEq
    
//    interface IObj with
//        member x.withMeta(m:IPersistentMap) = (if Object.ReferenceEquals(meta,m) then x else Symbol(m,ns,name)) :> IObj

//    interface IMeta with    
//        member x.meta() = meta


//    // some accessors, I prefer to use these internally.

//    member x.Namespace : string = ns
//    member x.Name : string = name

//    interface Named with
//        member x.getNamespace() = ns
//        member x.getName() = name

//    interface IFn with
//        member x.invoke(o:obj) : obj = Helpers.get(obj,this)
//        member x.invoke(o:obj, notFound,obj) : obj = Helpers.get(obj,this,notFound)

//    interface IComparable<Symbol> with
//        member x.CompareTo(s:Symbol) : int = 
//            if x.Equals(s) then 0
//            elif ns = null && x.Namespace <> null then -1
//            elif ns <> null then
//                if s.Namespace = null  then 1
//                else 
//                    let nsc = ns.CompareTo(s.Namespace)
//                    if nsc <> 0 then nsc else name.CompareTo(s.Name)
//            else name.CompareTo(s.Name)


//    interface IComparable with
//        member x.CompareTo(o:obj) =
//            match o with
//            | :? Symbol as s -> (x:>IComparable<Symbol>).CompareTo(s)
//            | _ -> invalidArg "o" "must compare to a non-null Symbol"


//    // we also defined operators ==, !=, <, > : TODO: decide if there is any point to them.

//    // Not yet translated  - not clear if they are needed or should be elswhere

//    //let mutable toStringEscCached : string option = None
    
//    //       private static string NameMaybeEscaped(string s)
//    //       public string ToStringEscaped()
//    //       protected Symbol (SerializationInfo info, StreamingContext context)

//    //       public virtual void GetObjectData(SerializationInfo info, StreamingContext context)


//[<Sealed>][<AllowNullLiteral>]
//type Keyword private (s:Symbol) =
//    inherit AFn()
//    let sym = s
//    let hasheq = (s:>IHashEq).hasheq() + 0x9e3779b9

//    [<NonSerialized>]
//    let mutable toStringCached : String option = None

//    member x.Symbol = sym

//    // map from symbols to keywords to uniquify keywords
//    static member symKeyMap = ConcurrentDictionary<Symbol,WeakReference<Keyword>>()

//    static member intern(s: Symbol) : Keyword =
//        let useSym s:Symbol = if (s:>IMeta).meta() = null then s else downcast (s:>IObj).withMeta(null) 

//        let exists, wref = Keyword.symKeyMap.TryGetValue(s)
//        if exists 
//        then
//            let exists, existingKw = wref.TryGetTarget()
//            if exists then existingKw
//            else 
//                // WeakReference timed out.  Set a new Keyword in place
//                // we don't have a timing problem here.  if someone managed to sneak another keyword in here, it won't really matter.
//                let k = Keyword(useSym s)
//                wref.SetTarget(k) 
//                k
//         else
//            let s1 = useSym s
//            let k = Keyword(s1)
//            Keyword.symKeyMap.GetOrAdd(s1,WeakReference<Keyword>(k)) |> ignore
//            // whether we were successful or not, okay to return the one we have in hand
//            k
            
//    static member intern(ns,name) = Keyword.intern(Symbol.intern(ns,name))
//    static member intern(nsname) = Keyword.intern(Symbol.intern(nsname))

//    override x.ToString() =
//        match toStringCached with
//        | Some s -> s
//        | None ->
//            let s = ":" + sym.ToString()
//            toStringCached <- Some s
//            s
        
//    interface IEquatable<Keyword> with 
//        member x.Equals(k:Keyword) : bool = Object.ReferenceEquals(x,k) || sym.Equals(k.Symbol)

//    override x.Equals(o:obj) : bool = 
//        match o with
//        | :? Keyword as k -> (x:>IEquatable<Keyword>).Equals(k)
//        | _ -> false

//    override x.GetHashCode() = sym.GetHashCode() + 0x9e3779b9

//    interface IHashEq with  
//        member x.hasheq() = hasheq

//    // I prefer these for myself.
//    member x.Namespace = sym.Namespace
//    member x.Name = sym.Name

//    interface Named with
//        member x.getNamespace() = sym.Namespace
//        member x.getName() = sym.Name

//    interface IFn with
//        // (:keyword arg) = (gat arg :keyword)
//        member x.invoke(arg1:obj) = 
//            match arg1 with
//            | :? ILookup as ilu -> ilu.valAt(x)
//            | _ -> Helpers.get(arg1,x)
//        // (:keyword arg default) = (gat arg :keyword default)
//        member x.invoke(arg1:obj, notFound:obj) = 
//            match arg1 with
//            | :? ILookup as ilu -> ilu.valAt(x,notFound)
//            | _ -> Helpers.get(arg1,x,notFound)

//    interface IComparable<Keyword> with
//        member x.CompareTo(k:Keyword) = sym.CompareTo(k.symbol)

//    interface IComparable with  
//        member x.CompareTo(o:obj) =
//            match o with 
//            | :? Keyword as k -> (x:>IComparable<Keyword>).CompareTo(k)
//            | _ -> invalidArg "0" "Cannot campre to null or non-Keyword"

//    // we had operator overloads for ==, !=, <, >
//    // TODO: do we need?

//    static member find(s:Symbol) : Keyword =
//        let exists, wr = Keyword.symKeyMap.TryGetValue(s)
//        if exists 
//        then
//            let exists, kw = wr.TryGetTarget()
//            if exists then kw else null
//        else null

//    static member find(ns:string, name:string) : Keyword = Keyword.find(Symbol.intern(ns,name))
//    static member find(nsname:string) : Keyword = Keyword.find(Symbol.intern(nsname))
 
//    // not done yet
//    //       public void GetObjectData(SerializationInfo info, StreamingContext context)
//    //       sealed class KeywordSerializationHelper : IObjectReference
