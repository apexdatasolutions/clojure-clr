namespace Clojure.Collections

open System


module Helpers = 

    let boundedLength(list:ISeq, limit:int) : int =

        // At some point maybe we need a to model the other sequence functions?

        let mutable i = 0
        let mutable c = list
    
        while c <> null && i <= limit do
            c <- c.next()
            i <- i+1

        i

    let seqLength (list:ISeq) : int =

        // At some point maybe we need a to model the other sequence functions?

        let mutable i = 0
        let mutable c = list

        while c <> null && c <> null do
            c <- c.next()
            i <- i+1

        i

    let seqToArray<'a> (xs:ISeq) : 'a array =
        if xs = null then Array.zeroCreate(0)
        else    
            let a = Array.zeroCreate<'a>(seqLength xs)
            let mutable i = 0
            let mutable s = xs

            while s <> null do
                a.[i] <- downcast s.first() 
                s <- s.next()
                i <- i+1
            a
              

    let vectorToString (x:obj) : string = "HELP! WRITE ME!!!  TODO HELL!!!!"

    //else if (x is IPersistentVector)
    // {
    //     IPersistentVector v = x as IPersistentVector;
    //     int n = v.count();
    //     w.Write('[');
    //     for (int i = 0; i < n; i++)
    //     {
    //         print(v.nth(i), w);
    //         if (i < n - 1)
    //             w.Write(" ");
    //     }
    //     w.Write(']');
    // }
        

    let equals x y =
        Object.ReferenceEquals(x,y) || x <> null && x.Equals(y)


