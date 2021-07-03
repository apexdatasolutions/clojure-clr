namespace Clojure.Collections

open Clojure.Fn
open System
open System.Collections

//[<AbstractClass>]
//type APersistentVector() =
//    inherit AFn()

//    let cachedHash : int = 0
//    let cachedHashEq : int = 0

//    override x.ToString() = Helpers.vectorToString(x)
//    override x.Equals(o) = APersistentVector.doEquals(x,o)

//    static member doEquals(v:IPersistentVector, o: obj) : bool =
//        if Object.ReferenceEquals(v,o) then true
//        else 
//            match o with

//            | :? IPersistentVector as ipv ->
//                if ipv.count() <> v.count() then false
//                else
//                    let mutable ok = true
//                    let mutable i = 0
//                    let n = ipv.count()
//                    while i < n && ok do    
//                        ok <- Helpers.equals (v.nth(i)) (ipv.nth(i))
//                        i <- i+1
//                    ok

//            | :? IList as ilist ->               // do we ever need this?

//                if ilist.Count <> v.count() then false
//                else
//                    let mutable ok = true
//                    let mutable i = 0
//                    let n = ilist.Count
//                    while i < n && ok do    
//                        ok <- Helpers.equals (v.nth(i)) (ilist.[i])
//                        i <- i+1
//                    ok

//            // TODO: Figure this out.  In the original, this is a test for Sequential, which is a marker interface. Then it calls RT.seq to get an ISeq
//            // RT.Seq checks for ASeq and then LazySeq and special cases them.
//            // It then calls private RT.seqFrom, which checks for null, Seqable, IsArray, String, IEnumerable and then blows up.
//            // Anything Sequable is going to be user deinfed, and hence is either Seqable or IEnumerable.  So why not just chek for them?  Because the others cannot be Seqable, being system-defined?
//            // For now, I'm substituting checks for Seqable and IEnumerable and handling them separately.

//            | :? Sequential as sq ->            
//                let ms = Helpers                 
                        
                    
            


    

//[<AbstractClass>]
//type AMapEntry() =
//   inherit APersistentVector()  




