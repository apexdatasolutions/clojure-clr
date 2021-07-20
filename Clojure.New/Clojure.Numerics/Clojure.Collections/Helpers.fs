namespace Clojure.Collections

open System
open System.IO



// We are going to take the RT static class in the original and split it into pieces.
// Some things are needed fairly early, just to get the collections up and running.
// Some are needed really as part of the Clojure environment at runtime.  Those things will be deferred.

//  Ditto Util.  Keeping the distinctions betweene the two for now so that we can track back to the originals.
// TODO:  merge into a single module?

// TODO:  Some of these functions need to be rewritten to use protocols for extensions.


module RT =

    
    // There is a massive circularity with RT.seq.
    // Original checks for ASeq, LazySeq and uses StringSeq, ArraySeq, chunkEnumeratorSeq
    // To pull all of those things in would require one massive recursive definition of half of the collection.
    // If instead, Seqable or ISeq was a protocol, we'd be done.
    // TODO: PROTOCOLS!!!
    let seq (x:obj) =
        match x with
        | null -> null
        | :? ISeq as s -> s.seq()
        | :? Seqable as s -> s.seq()
        //| :? string as s-> StringSeq.create(s)
        //| _ when x.GetType().IsArray -> ArraySeq.createFromObject(x)
        //| :? IEnumerable as e -> chunkEnumeratorSeq(e.GetEnumerator())
        | _ -> raise <| InvalidOperationException($"Don't know how to create ISeq from {x.GetType().FullName}")
    

    let first(x:obj) = 
        let s = 
            match x with
            | :? ISeq as s -> s
            | _ -> seq(x)
        s.first()

    let next(x:obj) = 
        let s = 
            match x with
            | :? ISeq as s -> s
            | _ -> seq(x)
        if s = null then null else s.next()

    let second(x:obj) = x |> next |> first
    let third(x:obj) = x |> next |> next |> first
    let fourth(x:obj) = x |> next |> next |> next |> first

    
    // Was RT.Length
    let seqLength (list:ISeq) : int =
        let rec step (s:ISeq) cnt = if s = null then cnt else step (s.next()) (cnt+1)
        step list 0

    // the real printer to use in Clojure requires a lot of Clojure infrastructure.
    // We provide a base printer that can be used as a default case later.
    // For now, we install the base printer as
    // The initialization of the Clojure environment will have to install its own printer.


    // Note that our default printer cannot call ToString on the collections -- those methods will be calling this.  Circularity city.
    // However, it can call ToString on items in a collection.

    // 


    type PrintFnType = (obj * TextWriter) -> unit

    let dummyPrinter : PrintFnType = raise <| NotImplementedException("Call to dummyPrinter -- initialization error")
        

    let mutable private metaPrinterFn : PrintFnType = dummyPrinter
    let setMetaPrintFn (prfn: PrintFnType)  : unit = metaPrinterFn <- prfn

    let mutable private printFn : PrintFnType = dummyPrinter
    let setPrintFn (prfn: PrintFnType)  : unit = printFn <- prfn


    // TODO: figure out how to properly incorporate 'readably' into this interface.
    // Probably needs to happen with the functions setthe functions above.

    let rec baseMetaPrinter(x:obj, w:TextWriter) : unit = 
        match x with
        | :? IMeta as xo ->                        // original code has Obj here, but not sure why this is correct.  We only need an IMeta to have metadata.
            let meta = xo.meta()                    // the real version will check for a meta with count=1 and just a tag key and special case that.
            w.Write("#^")
            print(meta,w)
            w.Write(' ')
        | _ -> ()

    and basePrinter(readably: bool, x:obj, w:TextWriter) : unit =

        let printInnerSeq readably (s:ISeq) (w:TextWriter) =
            let rec step (s:ISeq) = 
                if s <> null then basePrinter(readably,s,w)
                if s.next() <> null then w.Write(' ')
                s.next() |> step
            step s

        let baseCharPrinter readably (c:char) (w:TextWriter) =
            if not readably then w.Write(c)
            else
                w.Write('\\')
                match c with
                | '\n' -> w.Write("newline")
                | '\t' -> w.Write("tab")
                | '\r' -> w.Write("return")
                | ' ' -> w.Write("space")
                | '\f' -> w.Write("formfeed")
                | '\b' -> w.Write("backspace")
                | _ -> w.Write(c)

        let baseStringPrinter readably (s:string) (w:TextWriter) =
            if not readably then w.Write(s)
            else
                w.Write('"')
                s |> Seq.iter (fun c -> 
                        match c with
                        | '\n' -> w.Write("\\n")
                        | '\t' -> w.Write("\\t")
                        | '\r' -> w.Write("\\r")
                        | '"' -> w.Write("\\\"")
                        | '\\' -> w.Write("\\\\")
                        | '\f' -> w.Write("\\f")
                        | '\b' -> w.Write("\\b")
                        | _ -> w.Write(c)
                    )
                w.Write('"');

        metaPrinterFn(x,w)
        match x with
        | null -> w.Write("nil")
        | :? ISeq 
        | :? IPersistentList ->
            w.Write('(')
            printInnerSeq readably (seq(x)) w
            w.Write(')')
        | :? String as s -> baseStringPrinter readably s w
        | :? IPersistentMap ->
            let rec step (s:ISeq) = 
                let e : IMapEntry = downcast s.first()
                basePrinter(readably,e.key(),w)
                w.Write(' ')
                basePrinter(readably,e.value(),w)
                if s.next() <> null then w.Write(", ")
                step (s.next())
            w.Write('{');    
            seq(x) |> step
            w.Write('}')
        | :? IPersistentVector as v ->
            let n = v.count()
            w.Write('[');
            for i = 0 to n-1 do
                basePrinter(readably,v.nth(i),w)
                if i < n-1 then w.Write(" ") 
            w.Write(']');
        | :? IPersistentSet ->
            let rec step (s:ISeq) = 
                basePrinter(readably,s.first(),w)
                if not (isNull (s.next())) then w.Write(" ")
                step (s.next())
            w.Write("#{")  
            seq(x) |> step
            w.Write('}')
        | :? Char as ch -> baseCharPrinter readably ch w
        | _ -> w.Write(x.ToString())

        // TODO:  Figure out how best to integrate the rest of these

        //    else if (x is Type type)
        //    {
        //        string tName = type.AssemblyQualifiedName;
        //        if (LispReader.NameRequiresEscaping(tName))
        //            tName = LispReader.VbarEscape(tName);
        //        w.Write("#=");
        //        w.Write(tName);
        //    }
        //    else if (x is BigDecimal && readably)
        //    {
        //        w.Write(x.ToString());
        //        w.Write("M");
        //    }
        //    else if (x is BigInt && readably)
        //    {
        //        w.Write(x.ToString());
        //        w.Write("N");
        //    }
        //    else if (x is BigInteger && readably)
        //    {
        //        w.Write(x.ToString());
        //        w.Write("BIGINT");
        //    }
        //    else if (x is Var)
        //    {
        //        Var v = x as Var;
        //        w.Write("#=(var {0}/{1})", v.Namespace.Name, v.Symbol);
        //    }
        //    else if (x is Regex r)
        //    {
        //        w.Write("#\"{0}\"", r.ToString());
        //    }
        //    //else
        //    //    w.Write(x.ToString());
        //    // The clause above is what Java has, and would have been nice.
        //    // Doesn't work for me, for one reason:  
        //    // When generating initializations for static variables in the classes representing IFns,
        //    //    let's say the value is the double 7.0.
        //    //    we generate code that says   (double)RT.readFromString("7")
        //    //    so we get a boxed int, which CLR won't cast to double.  Sigh.
        //    //    So I need double/float to print a trailing .0 even when integer-valued.
        //    else if (x is double || x is float)
        //    {
        //        string s = x.ToString();
        //        if (!s.Contains('.') && !s.Contains('E'))
        //            s += ".0";
        //        w.Write(s);
        //    }
        //    else
        //        w.Write(x.ToString());
        //}

               
    and print(x:obj, w:TextWriter ) : unit = printFn(x, w)

    setPrintFn (fun (x,w) -> basePrinter(true,x,w))
    setMetaPrintFn metaPrinterFn

    let printString(x:obj) =
        use sw = new StringWriter()
        print(x,sw)
        sw.ToString()

            
module Util =
    
    let hash x =
        match x with
        | null -> 0
        | _ -> x.GetHashCode()
    
    let equals(x,y) =
        Object.ReferenceEquals(x,y) || x <> null && x.Equals(y)
    
    let private isNullableType (t:Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Nullable<_>>

    let private getNonNullabelType (t:Type) = if isNullableType t then t.GetGenericArguments().[0] else t


    // The Clojure initialization will have to add the types System.Numeric.BigInteger, Clojure.Numerics.BigDecimal, Clojure.Numerics.BigRational, Clojure.BigInt
    let mutable private extraNumericTypes : Type list = List.empty

    let addExtraNumericTypes (ts:Type seq) = extraNumericTypes <- extraNumericTypes |> List.append (Seq.toList(ts))
    let removeExtraNumericType (ts:Type seq) = extraNumericTypes <- extraNumericTypes |> List.except ts 
    let isExtraNumericType (t:Type) = extraNumericTypes |> List.contains t

    // Similarly, we need to provide a method for comparing numeric types for equality
    let dummyNumericEquality(x:obj,y:obj) : bool = raise <| NotImplementedException("Called dummy numeric equality function -- initialization error")
 
    let mutable private numericEqualityFn : (obj*obj) -> bool = dummyNumericEquality
    let setNumericEqualityFn (neFn:((obj*obj) -> bool)) = numericEqualityFn <- neFn
        


    let private isNumericType (t:Type) = 
        let t = getNonNullabelType(t)
        if t.IsEnum then false
        else
            match Type.GetTypeCode(t)  with
            | TypeCode.SByte | TypeCode.Byte
            | TypeCode.Int16 | TypeCode.UInt16
            | TypeCode.Int32 | TypeCode.UInt32
            | TypeCode.Int64 | TypeCode.UInt64
            | TypeCode.Single | TypeCode.Double -> true
            | _ when isExtraNumericType t -> true
            | _ -> false

    let private isNumeric (o:obj) = o <> null && isNumericType (o.GetType())


    let equiv(k1:obj, k2:obj) =
        if Object.ReferenceEquals(k1,k2) then true
        elif isNull k1 then false
        else 
            if isNumeric k1 && isNumeric k2 then numericEqualityFn(k1,k2)
            else
                match k1, k2 with
                | :? IPersistentCollection as pc1, _ -> pc1.equiv(k2)
                | _, (:? IPersistentCollection as pc2) -> pc2.equiv(k1) 
                | _ -> k1.Equals(k2)

  


////open System
////open System.Collections


////type Reduced(v:obj) =
////    let value = v







//////       #region IReduce Members

//////       /// <summary>
//////       /// Reduce the collection using a function.
//////       /// </summary>
//////       /// <param name="f">The function to apply.</param>
//////       /// <returns>The reduced value</returns>
//////       public object reduce(IFn f)
//////       {
//////           object ret = first();
//////           for (ISeq s = next(); s != null; s = s.next()) { 
//////               ret = f.invoke(ret, s.first());
//////               if (RT.isReduced(ret))
//////                   return ((IDeref)ret).deref();
//////           }
//////           return ret;
//////       }

//////       /// <summary>
//////       /// Reduce the collection using a function.
//////       /// </summary>
//////       /// <param name="f">The function to apply.</param>
//////       /// <param name="start">An initial value to get started.</param>
//////       /// <returns>The reduced value</returns>
//////       public object reduce(IFn f, object start)
//////       {
//////           object ret = f.invoke(start, first());
//////           for (ISeq s = next(); s != null; s = s.next()) {
//////               if (RT.isReduced(ret))
//////                   return ((IDeref)ret).deref(); 
//////               ret = f.invoke(ret, s.first());
//////           }
//////           if (RT.isReduced(ret))
//////               return ((IDeref)ret).deref();
//////           return ret;
//////       }

//////       #endregion




//////       /// <summary>
//////       /// Provides a function to create a list from a sequence of arguments. (Internal use only.)
//////       /// </summary>
//////       /// <remarks>Internal use only.  Used to interface with core.clj.</remarks>
//////       public sealed class PLCreator : RestFn
//////       {
//////           public override int getRequiredArity()
//////           {
//////               return 0;
//////           }

//////           /// <summary>
//////           /// The creator method.
//////           /// </summary>
//////           /// <param name="args">A sequence of elements.</param>
//////           /// <returns>A new list.</returns>
//////           protected override object doInvoke(object args)
//////           {
//////               if (args is IArraySeq ias)
//////               {
//////                   object[] argsarray = (object[])ias.ToArray();
//////                   IPersistentList ret = EMPTY;
//////                   for (int i = argsarray.Length - 1; i >= ias.index(); i--)
//////                       ret = (IPersistentList)ret.cons(argsarray[i]);
//////                   return ret;
//////               }

//////               List<object> list = new List<object>();
//////               for (ISeq s = RT.seq(args); s != null; s = s.next())
//////                   list.Add(s.first());
//////               return create(list);
//////           }

//////           [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "ClojureJVM name match")]
//////           static public object invokeStatic(ISeq args)
//////           {
//////               if (args is IArraySeq ias)
//////               {
//////                   object[] argsarray = (object[])ias.ToArray();
//////                   IPersistentList ret = EMPTY;
//////                   for (int i = argsarray.Length - 1; i >= 0; i--)
//////                       ret = (IPersistentList)ret.cons(argsarray[i]);
//////                   return ret;
//////               }

//////               List<object> list = new List<object>();
//////               for (ISeq s = RT.seq(args); s != null; s = s.next())
//////                   list.Add(s.first());
//////               return create(list);
//////           }
//////       }

//////       static readonly IFn _creator = new PLCreator();
//////       /// <summary>
//////       /// An <see cref="IFn">IFn</see> to create a list from a sequence of items.
//////       /// </summary>
//////       /// <remarks>The name is without our usual leading underscore for compatiblity with core.clj.</remarks>
//////       [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Naming Styles", Justification = "ClojureJVM name match")]
//////       public static IFn creator { get { return _creator; } }



//////   }
//////}








////module Helpers = 



              

////    let vectorToString (x:obj) : string = "HELP! WRITE ME!!!  TODO HELL!!!!"

////    //else if (x is IPersistentVector)
////    // {
////    //     IPersistentVector v = x as IPersistentVector;
////    //     int n = v.count();
////    //     w.Write('[');
////    //     for (int i = 0; i < n; i++)
////    //     {
////    //         print(v.nth(i), w);
////    //         if (i < n - 1)
////    //             w.Write(" ");
////    //     }
////    //     w.Write(']');
////    // }
        

////    let cons(x:obj,coll:obj) : ISeq =
////        match coll with
////        | null -> PersistentList(x)
////        | :? ISeq as s -> Cons(s,x)
////        | _ -> Cons(x,seq(coll))


////        //public static ISeq cons(object x, object coll)
////        //{
////        //    if (coll == null)
////        //        return new PersistentList(x);


////        //    if (coll is ISeq s)
////        //        return new Cons(x, s);

////        //    return new Cons(x, seq(coll));
////        //}