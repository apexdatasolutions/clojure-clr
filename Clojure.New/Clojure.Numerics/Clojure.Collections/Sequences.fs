namespace Clojure.Collections

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




