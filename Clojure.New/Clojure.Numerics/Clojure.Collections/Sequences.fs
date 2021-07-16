namespace Clojure.Collections

open System
open System.Collections
open System.Collections.Generic


[<AbstractClass>][<AllowNullLiteral>]
type ASeq(m) =
    inherit Obj(m)
    let mutable hash = 0
    let mutable hasheq = 0
    new() = ASeq(null)
  
    override x.ToString() = Helpers.printString(x)

    override x.Equals(o) = 
        if obj.ReferenceEquals(x,o) then true
        else
            match o with
            | :? Sequential | :? IList -> 
                let rec step (s1:ISeq) (s2:ISeq) =
                    match s1, s2 with
                    | null, null -> true
                    | _, null -> false
                    | null, _ -> false
                    | _ -> Helpers.equals(s1.first(),s2.first()) && step (s1.next()) (s2.next())  // Util.equals
                step x Helpers.seq(o)                                                             // = RT.seq
            | _ -> false

    override x.GetHashCode() =
        if hash = 0 then hash <- Helpers.computeHashCode x
        hash


    static member doCount (s:ISeq) =
        let rec step (s:ISeq) cnt = 
            match s with
            | null -> cnt
            | :? Counted as c -> cnt + c.count()
            | _ -> step (s.next()) (cnt+1)
        step s 0

    interface ISeq with
        member x.more() =
            let s = (x:>ISeq).next()
            if s = null then EmptyList.Empty else s
        member x.cons(o) = Cons(o,x)    

    interface IPersistentCollection with
        member x.cons(o) = (x:>ISeq).const(o)
        member x.count() = 1 + doCount (x:>ISeq).next()
        member x.empty() = EmptyList.Empty
        member x.equiv(o) = 
            match o with
            | :? Sequential | :? IList -> 
                let rec step (s1:ISeq) (s2:ISeq) =
                    match s1, s2 with
                    | null, null -> true
                    | _, null -> false
                    | null, _ -> false
                    | _ -> Helpers.equiv(s1.first(),s2.first()) && step (s1.next()) (s2.next())  // Util.equiv
                step x Helpers.seq(o)                                                             // = RT.seq
            | _ -> false

    interface Seqable with
        member x.seq() = x :> ISeq

    interface IList<obj> with
        //member _.Add(_) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Insert(i,v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        //member _.Remove(v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.RemoveAt(i) = raise <| InvalidOperationException("Cannot modify an immutable sequence")


    interface IList with
        member _.Add(_) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Clear() = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Insert(i,v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.Remove(v) = raise <| InvalidOperationException("Cannot modify an immutable sequence")
        member _.RemoveAt(i) = raise <| InvalidOperationException("Cannot modify an immutable sequence")    
        member _.IsFixedSize = true
        member _.IsReadOnly = true
        member x.Item 
            //Java has this: return RT.nth(this, index);
            // THis causes an infinite loop in my code.    TODO:  SEE IF THIS IS STILL TRUE, OR FIND A WORKAROUND?
            // When this was introduces, a change was made in RT.nth that changed the List test in its type dispatch to RandomAccess.
            // CLR does not have the equivalent notion, so I just left it at IList.  BOOM!
            // So, I have to do a sequential search, duplicating some of the code in RT.nth.
            with get index = 
                let rec step i (s:ISeq) = 
                    if i = index then s.first()
                    elif s = null then raise <| ArgumentOutOfRangeException("index")
                    else step (i+1) (s.next())
                step 0 x                                            // TODO: See IndexOf. Should this be called on x or x.seq() ??  Check original Java code.
            and set _ _ = raise <| InvalidOperationException("Cannot modify an immutable sequence")  
        member x.IndexOf(v) =
            let rec step i (s:ISeq) = 
                if s == null then -1
                else if Helpers.equiv(s.first(), v) then i
                else step (i+1) (s.next())
            step 0 ((x:>ISeq).seq())

               


    interface IEnumerable with
        member x.GetEnumerator() = SeqEnumerator(x)

    interface ICollection with
        // this was in old code -- maybe a mistake??  TODO: Get rid of this if we get everything working
        //member x.CopyTo(arr : obj array,idx) =
        //    if arr = null then raise <| ArgumentNullException("array")
        //    if arr.Rank() <> 1 then raise <| ArgumentException("Array must be 1-dimensional")
        //    if idx < 0 then raise <| ArgumentOutOfRangeException("arrayIndex","must be non-negative")
        //    if arr.Length - idx < (x:>IPersistentCollection).count() then raise <| InvalidOperationException("The number of elements in source is greater than the available space in the array.")
        //    let rec step (i:int) (s:ISeq) =
        //        if i < arr.Length && s <> null 
        //        then 
        //            arr.SetValue(s.first(),i)
        //            step (i+1) (s.next())
        //    step 0 (x:>ISeq)                   
        member x.CopyTo(arr : Array,idx) =
            if arr = null then raise <| ArgumentNullException("array")
            if arr.Rank <> 1 then raise <| ArgumentException("Array must be 1-dimensional")
            if idx < 0 then raise <| ArgumentOutOfRangeException("arrayIndex","must be non-negative")
            if arr.Length - idx < (x:>IPersistentCollection).count() then raise <| InvalidOperationException("The number of elements in source is greater than the available space in the array.")
            let rec step (i:int) (s:ISeq) =
                if i < arr.Length && s <> null 
                then 
                    arr.SetValue(s.first(),i)
                    step (i+1) (s.next())
            step 0 (x:>ISeq)







//       /// <summary>
//       /// Gets the number of elements in the sequence.
//       /// </summary>
//       public int Count
//       {
//           get { return count(); }
//       }

//       /// <summary>
//       /// Gets a value indicating whether access to the collection is thread-safe.
//       /// </summary>
//       public bool IsSynchronized
//       {
//           get { return true; }
//       }

//       public object SyncRoot
//       {
//           get { return this; }
//       }

//       #region IHashEq

//       public int hasheq()
//       {
//           if (_hasheq == 0)
//           {
//               //int hash = 1;
//               //for (ISeq s = seq(); s != null; s = s.next())
//               //    hash = 31 * hash + Util.hasheq(s.first());

//               //_hasheq = hash;
//               _hasheq = Murmur3.HashOrdered(this);
//           }
//           return _hasheq;
//       }

//       #endregion

//   }
//}



//**
//*   Copyright (c) Rich Hickey. All rights reserved.
//*   The use and distribution terms for this software are covered by the
//*   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
//*   which can be found in the file epl-v10.html at the root of this distribution.
//*   By using this software in any fashion, you are agreeing to be bound by
//* 	 the terms of this license.
//*   You must not remove this notice, or any other, from this software.
//**/

///**
//*   Author: David Miller
//**/

//using System;

//namespace clojure.lang
//{
//   /// <summary>
//   /// Implements an immutable cons cell.
//   /// </summary>
//   [Serializable]
//   public sealed class Cons: ASeq
//   {
//       // Any reason not to seal this class?

//       #region Data

//       /// <summary>
//       /// Holds the first value.  (= CAR)
//       /// </summary>
//       private readonly object _first;

//       /// <summary>
//       /// Holds the rest value. (= CDR)
//       /// </summary>
//       private readonly ISeq _more;

//       #endregion

//       #region C-tors

//       /// <summary>
//       /// Initializes a <see cref="Cons">Cons</see> with the given metadata and first/rest.
//       /// </summary>
//       /// <param name="meta">The metadata to attach.</param>
//       /// <param name="first">The first value.</param>
//       /// <param name="more">The rest of the sequence.</param>
//       public Cons(IPersistentMap meta, object first, ISeq more)
//           : base(meta)
//       {
//           _first = first;
//           _more = more;
//       }

//       /// <summary>
//       /// Initializes a <see cref="Cons">Cons</see> with null metadata and given first/rest.
//       /// </summary>
//       /// <param name="first">The first value.</param>
//       /// <param name="more">The rest of the sequence.</param>
//       public Cons(object first, ISeq more)
//       {
//           _first = first;
//           _more = more;
//       }

//       #endregion

//       #region IObj members

//       /// <summary>
//       /// Create a copy with new metadata.
//       /// </summary>
//       /// <param name="meta">The new metadata.</param>
//       /// <returns>A copy of the object with new metadata attached.</returns>
//       public override IObj withMeta(IPersistentMap meta)
//       {
//           return (meta == _meta)
//               ? this
//               : new Cons(meta, _first, _more);
//       }

//       #endregion

//       #region ISeq members

//       /// <summary>
//       /// Gets the first item.
//       /// </summary>
//       /// <returns>The first item.</returns>
//        public override Object first()
//       {
//           return _first;
//       }


//        /// <summary>
//        /// Return a seq of the items after the first.  Calls <c>seq</c> on its argument.  If there are no more items, returns nil."
//        /// </summary>
//        /// <returns>A seq of the items after the first, or <c>nil</c> if there are no more items.</returns>
//        public override ISeq next()
//        {
//            return more().seq();
//        }


//        public override ISeq more()
//       {
//           return _more ?? PersistentList.EMPTY;
//       }

//       #endregion

//       #region IPersistentCollection members

//        /// <summary>
//        /// Gets the number of items in the collection.
//        /// </summary>
//        /// <returns>The number of items in the collection.</returns>
//        public override int count()
//        {
//            return 1 + RT.count(_more);
//        }

//       #endregion
//   }
//}



//////////////////////////////////////////


//open Clojure.Fn
//open System
//open System.Collections
//open Clojure.Collections

//// LazySeq comes up surprisingly early in the food (procedure) chain.
//// It relies on very few things external to it, so we start with it.

//[<Sealed>][<AllowNullLiteral>]
//type LazySeq(m1: IPersistentMap, fn1: IFn, s1: ISeq) =
//    inherit Obj(m1)
//    let mutable fn = fn1
//    let mutable s = s1
//    let mutable sv : obj = null
//    new(fn:IFn) = LazySeq(null,fn,null)
//    new(m1: IPersistentMap, s1: ISeq) = LazySeq(m1,null,s1)

//    override x.GetHashCode() = 
//        match (x:>ISeq).seq() with
//        | null -> 1
//        | _ as s -> Helpers.hash s


//    override x.Equals(o:obj) =
//        match  (x:>ISeq).seq(), o with
//        | null, :? Sequential 
//        | null, :? IList      -> Helpers.seq(o) = null
//        | null, _ -> false
//        | _ as s ,_ -> s.Equals(o)

//    interface IObj with
//        member x.withMeta(meta: IPersistentMap) =
//            if ( (x :> IMeta).meta() = meta ) then x :> IObj
//            else LazySeq(meta,(x:>ISeq).seq()) :> IObj

//    member x.sval() = 
//        if fn <> null 
//        then
//            sv <- fn.invoke()
//            fn <- null

//        match sv with
//        | null -> s :> obj
//        | _ -> sv

     
//     member x.myCons(o:obj) : ISeq = Helpers.cons(0,(x:>ISeq).seq()) 

//    interface ISeq with
//        member x.seq() =
//            let rec getNext (x:obj) =
//                match x with
//                | :? LazySeq as ls -> getNext(ls.sval())
//                | _ -> x

//            x.sval() |> ignore
//            if sv <> null then
//                let ls = sv
//                sv <- null
//                s <- Helpers.seq(getNext(ls))
//            s

//        member x.count() =
//            let rec countAux (s:ISeq) (acc:int) : int =
//                match s with  
//                | null -> acc
//                | _ -> countAux (s.next()) (acc+1)
//            countAux s 0

//        member x.cons(o:obj) : ISeq = x.myCons(o)
//        member x.cons(o:obj) : IPersistentCollection = upcast x.myCons(o)

//        member x.empty() : IPersistentCollection = PersistentList.Empty

//        member x.first() = 
//            (x:>ISeq).seq() |> ignore
//            if s = null then null else s.first()

//        member x.next() =
//            (x:>ISeq).seq() |> ignore
//            if s = null then null else s.next()

//        member x.more() =
//            (x:>ISeq).seq() |> ignore
//            if s = null then PersistentList.Empty else s.more()         


////public sealed class LazySeq : ISeq, Sequential, ICollection, IList, IList<Object>, IPending, IHashEq  // Should we do IList -- has index accessor




////       public bool equiv(object o)
////       {
////           ISeq s = seq();
////           if (s != null)
////               return s.equiv(o);
////           else
////               return (o is Sequential || o is IList) && RT.seq(o) == null;
////       }


////       #endregion

////       #region IPending members

////       public bool isRealized()
////       {
////           return _fn == null;
////       }

////       #endregion

////       #region IList Members

////       public void Add(object item)
////       {
////           throw new InvalidOperationException("Cannot modify immutable sequence");
////       }

////       int IList.Add(object value)
////       {
////           throw new InvalidOperationException("Cannot modify immutable sequence");
////       }

////       public void Clear()
////       {
////           throw new InvalidOperationException("Cannot modify immutable sequence");
////       }

////       public bool Contains(object value)
////       {
////           for (ISeq s = seq(); s != null; s = s.next())
////               if (Util.equiv(s.first(), value))
////                   return true;
////           return false;
////       }

////       public int IndexOf(object value)
////       {
////           ISeq s = seq();
////           for (int i = 0; s != null; s = s.next(), i++)
////               if (Util.equiv(s.first(), value))
////                   return i;
////           return -1;
////       }

////       public void Insert(int index, object value)
////       {
////           throw new InvalidOperationException("Cannot modify immutable sequence");
////       }

////       public bool IsFixedSize
////       {
////           get { return true; }
////       }

////       public bool IsReadOnly
////       {
////           get { return true; }
////       }

////       public bool Remove(object item)
////       {
////           throw new InvalidOperationException("Cannot modify immutable sequence");
////       }

////       void IList.Remove(object value)
////       {
////           throw new InvalidOperationException("Cannot modify immutable sequence");
////       }

////       public void RemoveAt(int index)
////       {
////           throw new InvalidOperationException("Cannot modify immutable sequence");
////       }

////       public object this[int index]
////       {
////           get
////           {
////               if (index < 0)
////                   throw new ArgumentOutOfRangeException(nameof(index),"Index must be non-negative.");

////               ISeq s = seq();
////               for (int i = 0; s != null; s = s.next(), i++)
////                   if (i == index)
////                       return s.first();
////               throw new ArgumentOutOfRangeException(nameof(index), "Index past end of sequence.");
////           }
////           set
////           {
////               throw new InvalidOperationException();
////           }
////       }

////       #endregion

////       #region ICollection Members

////       public void CopyTo(object[] array, int arrayIndex)
////       {
////           if (array == null)
////               throw new ArgumentNullException(nameof(array));
////           if (arrayIndex < 0)
////               throw new ArgumentOutOfRangeException(nameof(arrayIndex), "must be non-negative.");
////           if (array.Rank > 1)
////               throw new ArgumentException("must not be multidimensional",nameof(array));
////           if (arrayIndex >= array.Length)
////               throw new ArgumentException("must be less than the length", nameof(arrayIndex));
////           if (count() > array.Length - arrayIndex)
////               throw new InvalidOperationException("Not enough available space from index to end of the array.");

////           ISeq s = seq();
////           for (int i = arrayIndex; s != null; ++i, s = s.next())
////               array[i] = s.first();
////       }

////       public void CopyTo(Array array, int index)
////       {
////           if (array == null)
////               throw new ArgumentNullException(nameof(array));
////           if (index < 0)
////               throw new ArgumentOutOfRangeException(nameof(index),"must be non-negative.");
////           if (array.Rank > 1)
////               throw new ArgumentException("must not be multidimensional.", nameof(array));
////           if (index >= array.Length)
////               throw new ArgumentException("must be less than the length", nameof(index));
////           if (count() > array.Length - index)
////               throw new InvalidOperationException("Not enough available space from index to end of the array.");

////           ISeq s = seq();
////           for (int i = index; s != null; ++i, s = s.next())
////               array.SetValue(s.first(), i);
////       }

////       public int Count
////       {
////           get { return count(); }
////       }

////       public bool IsSynchronized
////       {
////           get { return true; }
////       }

////       public object SyncRoot
////       {
////           get { return this; }
////       }

////       #endregion

////       #region IEnumerable Members

////       public IEnumerator<object> GetEnumerator()
////       {
////           return new SeqEnumerator(this);
////       }

////       IEnumerator IEnumerable.GetEnumerator()
////       {
////           return new SeqEnumerator(this);
////       }

////       #endregion

////       #region IHashEq members

////       public int hasheq()
////       {
////           return Murmur3.HashOrdered(this);
////       }

////       #endregion

////   }
////}


////[<AbstractClass>]
////type APersistentVector() =
////    inherit AFn()

////    let cachedHash : int = 0
////    let cachedHashEq : int = 0

////    override x.ToString() = Helpers.vectorToString(x)
////    override x.Equals(o) = APersistentVector.doEquals(x,o)

////    static member doEquals(v:IPersistentVector, o: obj) : bool =
////        if Object.ReferenceEquals(v,o) then true
////        else 
////            match o with

////            | :? IPersistentVector as ipv ->
////                if ipv.count() <> v.count() then false
////                else
////                    let mutable ok = true
////                    let mutable i = 0
////                    let n = ipv.count()
////                    while i < n && ok do    
////                        ok <- Helpers.equals (v.nth(i)) (ipv.nth(i))
////                        i <- i+1
////                    ok

////            | :? IList as ilist ->               // do we ever need this?

////                if ilist.Count <> v.count() then false
////                else
////                    let mutable ok = true
////                    let mutable i = 0
////                    let n = ilist.Count
////                    while i < n && ok do    
////                        ok <- Helpers.equals (v.nth(i)) (ilist.[i])
////                        i <- i+1
////                    ok

////            // TODO: Figure this out.  In the original, this is a test for Sequential, which is a marker interface. Then it calls RT.seq to get an ISeq
////            // RT.Seq checks for ASeq and then LazySeq and special cases them.
////            // It then calls private RT.seqFrom, which checks for null, Seqable, IsArray, String, IEnumerable and then blows up.
////            // Anything Sequable is going to be user deinfed, and hence is either Seqable or IEnumerable.  So why not just chek for them?  Because the others cannot be Seqable, being system-defined?
////            // For now, I'm substituting checks for Seqable and IEnumerable and handling them separately.

////            | :? Sequential as sq ->            
////                let ms = Helpers                 
                        
                    
            


    

////[<AbstractClass>]
////type AMapEntry() =
////   inherit APersistentVector()  




