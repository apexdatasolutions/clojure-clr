namespace Clojure.Fn

open System
open Clojure.Collections


// In ClojureJVM, this would also implement Callable and Runnable -- no exact equivalent here -- shoule we look at Func<>? ThreadDelegate?

[<AllowNullLiteral>]
type IFn =
    abstract invoke : unit -> obj
    abstract invoke : arg1: obj -> obj
    abstract invoke : arg1: obj * arg2: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj  -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj  -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj * arg13: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj * arg13: obj * arg14: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj * arg13: obj * arg14: obj * arg15: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj * arg13: obj * arg14: obj * arg15: obj * arg16: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj * arg13: obj * arg14: obj * arg15: obj * arg16: obj * arg17: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj * arg13: obj * arg14: obj * arg15: obj * arg16: obj * arg17: obj * arg18: obj  -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj * arg13: obj * arg14: obj * arg15: obj * arg16: obj * arg17: obj * arg18: obj * arg19: obj  -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj * arg13: obj * arg14: obj * arg15: obj * arg16: obj * arg17: obj * arg18: obj * arg19: obj * arg20: obj -> obj
    abstract invoke : arg1: obj * arg2: obj * arg3: obj * arg4: obj * arg5: obj * arg6: obj * arg7: obj * arg8: obj * arg9: obj * arg10: obj * arg11: obj * arg12: obj * arg13: obj * arg14: obj * arg15: obj * arg16: obj * arg17: obj * arg18: obj * arg19: obj * arg20: obj * [<ParamArray>] args: obj array-> obj

type IFnArity =
    abstract hasArity : arity: int -> bool

type ArityException(actual0: int, name0: string, cause0: Exception) =
    inherit ArgumentException((sprintf "Wrong number of args(%i) passed to: %s" actual0 name0),cause0)   // TODO: Should use Compiler.demunge(name) in sprintf
    member val name = name0
    member val actual = actual0
    new() = ArityException(-1,"<Unknown>",null)
    new(actual: int, name: string) =  ArityException(actual,name,null)


[<AbstractClass>]
type AFn() =
    interface IFnArity with
        member _.hasArity (arity:int) : bool = false

    // This was in RT.  But only used in AFn and RestFn, so moving to here.
    static member boundedLength(list:ISeq, limit:int) : int =
        let rec step (c:ISeq) i = if c <> null && i <= limit then step (c.next()) (i+1) else i
        step list 0


    // This was in RT.  Should be in Helpers.  TODO: Maybe split Helpers?  It is used in a few other places in the code
    static member seqLength (list:ISeq) : int =
        let rec step (c:ISeq) i = if c <> null then step (c.next()) (i+1) else i
        step list 0
        
    
    // This was in RT.  But only used in RestFn, so moving to here.
    static member seqToArray<'a> (xs:ISeq) : 'a array =
        if xs = null then Array.zeroCreate(0)
        else    
            let a = Array.zeroCreate<'a>(AFn.seqLength xs)
            let rec step (s:ISeq) i =
                if s <> null 
                then
                    a.[i] <- downcast s.first()
                    step (s.next()) (i+1)
                else ()
            step xs 0                   
            a

    member x.WrongArityException (reqArity:int) : ArityException = ArityException(reqArity,x.GetType().FullName)

    interface IFn with  
        member x.invoke () = raise <| x.WrongArityException(0)
        member x.invoke(a1) =  raise <| x.WrongArityException(1)
        member x.invoke(a1,a2) =  raise <| x.WrongArityException(2)
        member x.invoke(a1,a2,a3) =  raise <| x.WrongArityException(3)
        member x.invoke(a1,a2,a3,a4) =  raise <| x.WrongArityException(4)
        member x.invoke(a1,a2,a3,a4,a5) =  raise <| x.WrongArityException(5)
        member x.invoke(a1,a2,a3,a4,a5,a6) =  raise <| x.WrongArityException(6)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7) =  raise <| x.WrongArityException(7)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8) =  raise <| x.WrongArityException(8)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9) =  raise <| x.WrongArityException(9)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) =  raise <| x.WrongArityException(10)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) =  raise <| x.WrongArityException(11)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) =  raise <| x.WrongArityException(12)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) =  raise <| x.WrongArityException(13)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) =  raise <| x.WrongArityException(14)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) =  raise <| x.WrongArityException(15)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) =  raise <| x.WrongArityException(16)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) =  raise <| x.WrongArityException(17)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) =  raise <| x.WrongArityException(18)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) =  raise <| x.WrongArityException(19)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) =  raise <| x.WrongArityException(20)
        member x.invoke(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,[<ParamArray>]args) =  raise <| x.WrongArityException(21)

    // TODO: Check to see if the original use of Util1.Ret is necessary.

    static member applyToHelper( fn: IFn, argList: ISeq ) =

        let mutable al = argList
        let n() = al <- al.next(); al

        match AFn.boundedLength(argList, 20) with
        | 0 -> fn.invoke()
        | 1 -> fn.invoke(al.first())
        | 2 -> fn.invoke(al.first(),al.next().first())
        | 3 -> fn.invoke(al.first(),n().first(),n().first())
        | 4 -> fn.invoke(al.first(),n().first(),n().first(),n().first())
        | 5 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first())
        | 6 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 7 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 8 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 9 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 10 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 11 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 12 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 13 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 14 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 15 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 16 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 17 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 18 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 19 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | 20 -> fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first())
        | _ ->  fn.invoke(al.first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),n().first(),AFn.seqToArray(al.next()))
        

    abstract member applyTo : arglist: ISeq -> obj 
    default x.applyTo arglist = AFn.applyToHelper(x, arglist)

    // TODO: Do we need the implementation of IDynamicMetaObjectProvide.GetMetaObject?


        
