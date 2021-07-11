namespace Clojure.Collections

//open System
//open System.Collections


//type Reduced(v:obj) =
//    let value = v

//    interface IDeref with
//        member x.deref() = value



//[<AbstractClass>]
//type ASeq(m:IPersistentMap) =
//    inherit Obj(m)

//    [<NonSerialized>]
//    let mutable hashValue: int = 0

//    [<NonSerialized>]
//    let metable hashEqValue: int = 0

//    new() = ASeq(null)

//    override x.ToString() = Helpers.printString(x)

//    override x.Equals(o:obj) = 
//        if Object.ReferenceEquals(x,o) then true
//        else
//            match o with
//            | :? Sequential
//            | :? IList  ->
//                let rec compare (xs:ISeq) (ys:ISeq) =
//                    if xs = null && ys = null then true
//                    elif xs = null || ys = null then false    
//                    elif not (Helpers.equals(xs.first(),ys.first())) then false
//                    else compare (xs.next()) (ys.next())
//                compare ((x:>ISeq).seq()) (Helpers.seq(o))
//            | _ -> false

//    override x.GetHashCode() =
//        if hashValue <> 0 then hashValue
//        else
//            let rec hashIt (xs:ISeq) (h:int) =
//                match xs with
//                | null -> h
//                | _ -> 
//                    let f = xs.first()
//                    let fh = if f = null then 0 else f.GetHashCode()
//                    hashIt (xs.next()) (31*h + fh)
//            hashIt ((x:>ISeq).seq()) 1

//    interface ISeq with
//        member x.first() = raise <| NotImplementedException("You must implement first")  // TODO: is there a way to have unimplemented members of an interface if you are abstract?
//        member x.next() = raise <| NotImplementedException("You must implement next")  // TODO: is there a way to have unimplemented members of an interface if you are abstract?
//        member x.more() =
//            match (x:>ISeq).next() with
//            | null -> PersistentList.EMPTY
//            | _ as s -> s
//        member x.cons(o:obj) : ISeq = Cons(o,(x:>ISeq))
//        member x.cons(o:obj) : IPersistentCollection = Cons(o,(x:>ISeq))
//        member x.count() = 
//            let rec iterate (xs:ISeq) (acc:int) =
//                match xs with
//                | null -> acc
//                | :? Counted as cnted -> acc+cnted.count()
//                | _ -> iterate (xs.next()) acc+1
//            iterate ((x:>ISeq).next()) 1
//        member x.seq() = (x:>ISeq)
//        member x.empty() = PersistentList.Empty

//    interface IHashEq with
//        member x.hasheq() =
//            if hashEqValue = 0 then hasheq <- Murmur2.HashOrdered(x)

//            hashEqValue


//\

////   [Serializable]
////   public abstract class ASeq: Obj, ISeq, Sequential, IList, IList<Object>, IHashEq

////       /// <summary>
////       /// Determine if an object is equivalent to this (handles all collections).
////       /// </summary>
////       /// <param name="o">The object to compare.</param>
////       /// <returns><c>true</c> if the object is equivalent; <c>false</c> otherwise.</returns>
////       public bool equiv(object o)
////       {
////           if (!(o is Sequential || o is IList))
////               return false;

////           ISeq ms = RT.seq(o);

////           for (ISeq s = seq(); s != null; s = s.next(), ms = ms.next())
////           {
////               if (ms == null || !Util.equiv(s.first(), ms.first()))
////                   return false;
////           }

////           return ms == null; // hit end of sequence on both sequences
////       }

////       #endregion

////       #region IList<Object>, IList Members

////       public void Add(object item)
////       {
////           throw new InvalidOperationException("Cannot modify an immutable sequence");
////       }

////       int IList.Add(object value)
////       {
////           throw new InvalidOperationException("Cannot modify an immutable sequence");
////       }

////       public void Clear()
////       {
////           throw new InvalidOperationException("Cannot modify an immutable sequence");
////       }

////       public bool Contains(object value)
////       {
////           for (ISeq s = seq(); s != null; s = s.next())
////               if (Util.equiv(s.first(), value))
////                   return true;

////           return false;
////       }

////       public virtual int IndexOf(object value)
////       {
////           int i = 0;
////           for (ISeq s = seq(); s != null; s = s.next(), i++)
////               if (Util.equiv(s.first(), value))
////                   return i;

////           return -1;
////       }

////       public void Insert(int index, object value)
////       {
////           throw new InvalidOperationException("Cannot modify an immutable sequence");
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
////           throw new InvalidOperationException("Cannot modify an immutable sequence");
////       }

////       void IList.Remove(object value)
////       {
////           throw new InvalidOperationException("Cannot modify an immutable sequence");
////       }

////       public void RemoveAt(int index)
////       {
////           throw new InvalidOperationException("Cannot modify an immutable sequence");
////       }


////       public object this[int index]
////       {
////           get
////           {
////               //Java has this: return RT.nth(this, index);
////               // THis causes an infinite loop in my code.  
////               // When this was introduces, a change was made in RT.nth that changed the List test in its type dispatch to RandomAccess.
////               // CLR does not have the equivalent notion, so I just left it at IList.  BOOM!
////               // So, I have to do a sequential search, duplicating some of the code in RT.nth.
////               ISeq seq = this;
////               for (int i = 0; i <= index && seq != null; ++i, seq = seq.next())
////               {
////                   if (i == index)
////                       return seq.first();
////               }
////               throw new ArgumentOutOfRangeException("index");
////           }
////           set
////           {
////               throw new InvalidOperationException("Cannot modify an immutable sequence");
////           }
////       }

////       #endregion

////       #region ICollection Members

////       public void CopyTo(object[] array, int arrayIndex)
////       {
////           if (array == null)
////               throw new ArgumentNullException("array");

////           if (array.Rank != 1)
////               throw new ArgumentException("Array must be 1-dimensional");

////           if (arrayIndex < 0)
////               throw new ArgumentOutOfRangeException("arrayIndex", "must be non-egative");

////           if (array.Length - arrayIndex < count())
////               throw new InvalidOperationException("The number of elements in source is greater than the available space in the array)");

////           ISeq s = seq();
////           for (int i = arrayIndex; i < array.Length && s != null; ++i, s = s.next())
////               array[i] = s.first();
////       }

////       /// <summary>
////       /// Copies the elements of the sequence to an Array, starting at a particular index.
////       /// </summary>
////       /// <param name="array">The Array that is the destination of the copy.</param>
////       /// <param name="index">The zero-based index in <paramref name="array"/>array</param> at which copying begins.
////       public void CopyTo(Array array, int index)
////       {
////           if (array == null)
////               throw new ArgumentNullException("array");

////           if (index < 0)
////               throw new ArgumentOutOfRangeException("index", "Must be non-negative");

////           if (array.Rank != 1)
////               throw new ArgumentException("must be 1-dimensional","array");

////           if (array.Length - index < count())
////               throw new InvalidOperationException("The number of elements in source is greater than the available space in the array");

////           ISeq s = seq();
////           for (int i = index; i < array.Length && s != null; ++i, s = s.next())
////               array.SetValue(s.first(), i);
////       }

////       /// <summary>
////       /// Gets the number of elements in the sequence.
////       /// </summary>
////       public int Count
////       {
////           get { return count(); }
////       }

////       /// <summary>
////       /// Gets a value indicating whether access to the collection is thread-safe.
////       /// </summary>
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

////       public virtual IEnumerator<object> GetEnumerator()
////       {
////           return new SeqEnumerator(this);
////       }

////       /// <summary>
////       /// Returns an enumerator that iterates through a collection.
////       /// </summary>
////       /// <returns>A <see cref="SeqEnumerator">SeqEnumerator</see> that iterates through the sequence.</returns>
////       IEnumerator IEnumerable.GetEnumerator()
////       {
////           return new SeqEnumerator(this);
////       }

////       #endregion

////   }
////}


//type PersistenList(m1:IPersistentMap, f1:obj, r1: IPersistentList, c1: int) =    
//    ASeq(m1)
//    let first = f1
//    let rest = r1
//    let count = c1
//    new(first:obj) = PersistentList(null,first,null,1)
    
//    static member Empty : EmptyList = EmptyList(null)

//    static member create(init:IList) = 
//        let mutable r = Empty
//        for i = init.Count-1 downto 0
//            r <- upcast r.cons(init.[i])
//        r

//    interface IObj with
//        member x.withMeta(m:IPersistentMap) =
//            if Object.ReferenceEquals(m,(x:>IMeta).meta) then x else PersistentList(m,first,rest,count)

//    interface ISeq with
//        member x.first() = first
//        member x.next() = if count=1 then null else rest.seq()
//        member x.cons(o:obj) : ISeq = PersistentList((x:>IMeta).meta,o,x,count+1)

//    interface IPersistentStack with
//        member x.peek() = first
//        member x.pop() = if rest = null then (Empty:>IObj).withMeta(m) else rest

//    interface IPersistentCollection with
//        member x.count() = count
//        member x.empty() = (Empty:>IObj).withMeta(m)


////   [Serializable]
////   public class PersistentList : ASeq, IPersistentList, IReduce, IList, IList<Object>, Counted
////   {


////       #region IReduce Members

////       /// <summary>
////       /// Reduce the collection using a function.
////       /// </summary>
////       /// <param name="f">The function to apply.</param>
////       /// <returns>The reduced value</returns>
////       public object reduce(IFn f)
////       {
////           object ret = first();
////           for (ISeq s = next(); s != null; s = s.next()) { 
////               ret = f.invoke(ret, s.first());
////               if (RT.isReduced(ret))
////                   return ((IDeref)ret).deref();
////           }
////           return ret;
////       }

////       /// <summary>
////       /// Reduce the collection using a function.
////       /// </summary>
////       /// <param name="f">The function to apply.</param>
////       /// <param name="start">An initial value to get started.</param>
////       /// <returns>The reduced value</returns>
////       public object reduce(IFn f, object start)
////       {
////           object ret = f.invoke(start, first());
////           for (ISeq s = next(); s != null; s = s.next()) {
////               if (RT.isReduced(ret))
////                   return ((IDeref)ret).deref(); 
////               ret = f.invoke(ret, s.first());
////           }
////           if (RT.isReduced(ret))
////               return ((IDeref)ret).deref();
////           return ret;
////       }

////       #endregion

////       /// <summary>
////       /// Represents an empty <see cref="IPersistentList">IPersistentList</see>.
////       /// </summary>
////       [Serializable]
////       public class EmptyList : Obj, IPersistentList, IList, IList<Object>, ISeq, Counted, IHashEq
////       {
////           #region Data

////           static readonly int _hasheq = Murmur3.HashOrdered(Enumerable.Empty<Object>());

////           #endregion

////           #region C-tors

////           /// <summary>
////           /// Initialize an <see cref="EmptyList">PersistentList.EmptyList</see> with given metadata.
////           /// </summary>
////           /// <param name="meta">The metadata to attach.</param>
////           public EmptyList(IPersistentMap meta)
////               : base(meta)
////           {
////           }

////           /// <summary>
////           /// Initialize an <see cref="EmptyList">PersistentList.EmptyList</see> with null metadata.
////           /// </summary>
////           EmptyList()
////           {
////           }


////           #endregion

////           #region Object overrides

////           /// <summary>
////           /// Return the hash code for the object.
////           /// </summary>
////           /// <returns>The hash code</returns>
////           public override int GetHashCode()
////           {
////               return 1;
////           }

////           /// <summary>
////           /// Determines if an object is equal to the current object.
////           /// </summary>
////           /// <param name="obj">The object to compare to.</param>
////           /// <returns><value>true</value> if the object is the same; <value>false</value> otherwise.</returns>
////           /// <remarks>
////           /// Equality is value-based.  Any empty sequence will do.
////           /// </remarks>
////           public override bool Equals(object obj)
////           {
////               return (obj is Sequential || obj is IList) && RT.seq(obj) == null;
////           }

////           public override string ToString()
////           {
////               return "()";
////           }

////           #endregion

////           #region IObj methods

////           public override IPersistentMap meta()
////           {
////               return _meta;
////           }

////           #endregion

////           #region IMeta methods

////           /// <summary>
////           /// Create a copy with new metadata.
////           /// </summary>
////           /// <param name="meta">The new metadata.</param>
////           /// <returns>A copy of the object with new metadata attached.</returns>
////           public override IObj withMeta(IPersistentMap meta)
////           {
////               return meta == _meta
////                   ? this
////                   : new EmptyList(meta);
////           }

////           #endregion

////           #region ISeq Members

////           public object first()
////           {
////               return null;
////           }

////           public ISeq next()
////           {
////               return null;
////           }

////           public ISeq more()
////           {
////               return this;
////           }

////           public ISeq cons(object o)
////           {
////               return new PersistentList(meta(), o, null, 1);
////           }

////           #endregion
           
////           #region IPersistentStack Members

////           /// <summary>
////           /// Peek at the top (first) element in the stack.
////           /// </summary>
////           /// <returns>The top (first) element.  )(Always null.)</returns>
////           public object peek()
////           {
////               return null;
////           }

////           /// <summary>
////           /// Returns a new stack with the top element popped.
////           /// </summary>
////           /// <returns>The new stack.  Always throws an exception.</returns>
////           public IPersistentStack pop()
////           {
////               throw new InvalidOperationException("Can't pop empty list");
////           }

////           #endregion

////           #region IPersistentCollection Members

////           /// <summary>
////           /// Gets the number of items in the collection.
////           /// </summary>
////           /// <returns>The number of items in the collection.  Always zero.</returns>
////           public int count()
////           {
////               return 0;
////           }

////           /// <summary>
////           /// Gets an ISeq to allow first/rest iteration through the collection.
////           /// </summary>
////           /// <returns>An ISeq for iteration.  The sequence is empty, so always null.</returns>
////           public ISeq seq()
////           {
////               return null;
////           }

////           IPersistentCollection IPersistentCollection.cons(object o)
////           {
////               return cons(o);
////           }

////           /// <summary>
////           /// Gets an empty collection of the same type.
////           /// </summary>
////           /// <returns>An emtpy collection.  Always returns itself.</returns>
////           public IPersistentCollection empty()
////           {
////               return this;
////           }

////           /// <summary>
////           /// Determine if an object is equivalent to this (handles all collections).
////           /// </summary>
////           /// <param name="o">The object to compare.</param>
////           /// <returns><c>true</c> if the object is equivalent; <c>false</c> otherwise.</returns>
////           public bool equiv(object o)
////           {
////               return Equals(o);
////           }

////           #endregion

////           #region ICollection Members

////           public void CopyTo(object[] array, int arrayIndex)
////           {
////               // no-op: no items to copy.
////           }

////           public void CopyTo(Array array, int index)
////           {
////               // no-op: no items to copy.
////           }

////           public int Count
////           {
////               get { return 0; }
////           }

////           public bool IsSynchronized
////           {
////               get { return true; }
////           }

////           public object SyncRoot
////           {
////               get { return this; }
////           }

////           #endregion

////           #region IEnumerable Members

////           public IEnumerator<object> GetEnumerator()
////           {
////               yield break;
////           }

////           IEnumerator IEnumerable.GetEnumerator()
////           {
////               yield break;
////           }

////           #endregion

////           #region IList Members

////           public void Add(object item)
////           {
////               throw new InvalidOperationException("Cannot modify an immutable list");
////           }

////           int IList.Add(object value)
////           {
////               throw new InvalidOperationException("Cannot modify an immutable list");
////           }

////           public void Clear()
////           {
////               throw new InvalidOperationException("Cannot modify an immutable list");
////           }

////           public bool Contains(object value)
////           {

////               for (ISeq s = seq(); s != null; s = s.next())
////                   if (Util.equiv(s.first(), value))
////                       return true;
////               return false;
////           }

////           public int IndexOf(object value)
////           {
////               ISeq s = seq();
////               for (int i = 0; s != null; s = s.next(), i++)
////                   if (Util.equiv(s.first(), value))
////                       return i;
////               return -1;
////           }

////           public void Insert(int index, object value)
////           {
////               throw new InvalidOperationException("Cannot modify an immutable list");
////           }

////           public bool IsFixedSize
////           {
////               get { return true; }
////           }

////           public bool IsReadOnly
////           {
////               get { return true; }
////           }

////           public bool Remove(object item)
////           {
////               throw new InvalidOperationException("Cannot modify an immutable list");
////           }
           
////           void IList.Remove(object value)
////           {
////               throw new InvalidOperationException("Cannot modify an immutable list");
////           }

////           public void RemoveAt(int index)
////           {
////               throw new InvalidOperationException("Cannot modify an immutable list");
////           }

////           public object this[int index]
////           {
////               get
////               {
////                   throw new ArgumentOutOfRangeException("index", "Cannot access elements in an empty list");
////               }
////               set
////               {
////                   throw new InvalidOperationException("Cannot modify an immutable list");
////               }
////           }

////           #endregion

////           #region IHashEq members

////           public int hasheq()
////           {
////               return _hasheq;
////           }

////           #endregion
////       }

////       /// <summary>
////       /// Provides a function to create a list from a sequence of arguments. (Internal use only.)
////       /// </summary>
////       /// <remarks>Internal use only.  Used to interface with core.clj.</remarks>
////       public sealed class PLCreator : RestFn
////       {
////           public override int getRequiredArity()
////           {
////               return 0;
////           }

////           /// <summary>
////           /// The creator method.
////           /// </summary>
////           /// <param name="args">A sequence of elements.</param>
////           /// <returns>A new list.</returns>
////           protected override object doInvoke(object args)
////           {
////               if (args is IArraySeq ias)
////               {
////                   object[] argsarray = (object[])ias.ToArray();
////                   IPersistentList ret = EMPTY;
////                   for (int i = argsarray.Length - 1; i >= ias.index(); i--)
////                       ret = (IPersistentList)ret.cons(argsarray[i]);
////                   return ret;
////               }

////               List<object> list = new List<object>();
////               for (ISeq s = RT.seq(args); s != null; s = s.next())
////                   list.Add(s.first());
////               return create(list);
////           }

////           [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "ClojureJVM name match")]
////           static public object invokeStatic(ISeq args)
////           {
////               if (args is IArraySeq ias)
////               {
////                   object[] argsarray = (object[])ias.ToArray();
////                   IPersistentList ret = EMPTY;
////                   for (int i = argsarray.Length - 1; i >= 0; i--)
////                       ret = (IPersistentList)ret.cons(argsarray[i]);
////                   return ret;
////               }

////               List<object> list = new List<object>();
////               for (ISeq s = RT.seq(args); s != null; s = s.next())
////                   list.Add(s.first());
////               return create(list);
////           }
////       }

////       static readonly IFn _creator = new PLCreator();
////       /// <summary>
////       /// An <see cref="IFn">IFn</see> to create a list from a sequence of items.
////       /// </summary>
////       /// <remarks>The name is without our usual leading underscore for compatiblity with core.clj.</remarks>
////       [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "ClojureJVM name match")]
////       public static IFn creator { get { return _creator; } }



////   }
////}



//[<Sealed>]
//type Cons 

////final public class Cons extends ASeq implements Serializable {

////private final Object _first;
////private final ISeq _more;

////public Cons(Object first, ISeq _more){
////	this._first = first;
////	this._more = _more;
////}


////public Cons(IPersistentMap meta, Object _first, ISeq _more){
////	super(meta);
////	this._first = _first;
////	this._more = _more;
////}

////public Object first(){
////	return _first;
////}

////public ISeq next(){
////	return more().seq();
////}

////public ISeq more(){
////	if(_more == null)
////		return PersistentList.EMPTY;
////	return _more;
////}

////public int count(){
////	return 1 + RT.count(_more);
////}

////public Cons withMeta(IPersistentMap meta){
////	if(meta() == meta)
////		return this;
////	return new Cons(meta, _first, _more);
////}
////}






//module Helpers = 

//    let boundedLength(list:ISeq, limit:int) : int =

//        // At some point maybe we need a to model the other sequence functions?

//        let mutable i = 0
//        let mutable c = list
    
//        while c <> null && i <= limit do
//            c <- c.next()
//            i <- i+1

//        i

//    let seqLength (list:ISeq) : int =

//        // At some point maybe we need a to model the other sequence functions?

//        let mutable i = 0
//        let mutable c = list

//        while c <> null && c <> null do
//            c <- c.next()
//            i <- i+1

//        i

//    let seqToArray<'a> (xs:ISeq) : 'a array =
//        if xs = null then Array.zeroCreate(0)
//        else    
//            let a = Array.zeroCreate<'a>(seqLength xs)
//            let mutable i = 0
//            let mutable s = xs

//            while s <> null do
//                a.[i] <- downcast s.first() 
//                s <- s.next()
//                i <- i+1
//            a
              

//    let vectorToString (x:obj) : string = "HELP! WRITE ME!!!  TODO HELL!!!!"

//    //else if (x is IPersistentVector)
//    // {
//    //     IPersistentVector v = x as IPersistentVector;
//    //     int n = v.count();
//    //     w.Write('[');
//    //     for (int i = 0; i < n; i++)
//    //     {
//    //         print(v.nth(i), w);
//    //         if (i < n - 1)
//    //             w.Write(" ");
//    //     }
//    //     w.Write(']');
//    // }
        

//    let equals x y =
//        Object.ReferenceEquals(x,y) || x <> null && x.Equals(y)

//    let hash x =
//        match x with
//        | null -> 0
//        | _ -> x.GetHashCode()

//    // There is a massive circularity with RT.seq.
//    // Original checks for ASeq, LazySeq and uses StringSeq, ArraySeq, chunkEnumeratorSeq
//    // To pull all of those things in would require one massive recursive definition of half of the collection.
//    // If instead, Seqable or ISeq was a protocol, we'd be done.
//    // TODO: PROTOCOLS!!!
//    let seq (x:obj) =
//        match x with
//        | null -> null
//        | :? ISeq as s -> s.seq()
//        | :? Seqable as s -> s.seq()
//        //| :? string as s-> StringSeq.create(s)
//        //| _ when x.GetType().IsArray -> ArraySeq.createFromObject(x)
//        //| :? IEnumerable as e -> chunkEnumeratorSeq(e.GetEnumerator())
//        | _ -> raise <| InvalidOperationException($"Don't know how to create ISeq from {x.GetType().FullName}")


//    let cons(x:obj,coll:obj) : ISeq =
//        match coll with
//        | null -> PersistentList(x)
//        | :? ISeq as s -> Cons(s,x)
//        | _ -> Cons(x,seq(coll))


//        //public static ISeq cons(object x, object coll)
//        //{
//        //    if (coll == null)
//        //        return new PersistentList(x);


//        //    if (coll is ISeq s)
//        //        return new Cons(x, s);

//        //    return new Cons(x, seq(coll));
//        //}
