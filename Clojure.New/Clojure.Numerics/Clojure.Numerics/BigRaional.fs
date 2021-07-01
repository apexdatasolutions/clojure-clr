namespace Clojure.Numerics

open System
open System.Numerics



[<Sealed>]
type BigRational(n:BigInteger,d:BigInteger) =
    let mutable numerator = n
    let mutable denominator = d
    do  
        if d.IsZero then raise <| DivideByZeroException("Denominator is zero")
        
        let (n1,d1) =   
            if n.Sign = 0 then (BigInteger.Zero,BigInteger.One)
            elif d.Sign < 0 then (-n,-d)
            else (n,d)
        let n2, d2 = BigRational.normalize n1 d1
        numerator <- n2
        denominator <- d2


    
    // integer constructors
    
    new(v:BigInteger) = BigRational(v,BigInteger.One)
    new(x:int32) = BigRational(BigInteger(x),BigInteger.One)
    new(x:int64) = BigRational(BigInteger(x),BigInteger.One)
    new(x:uint32) = BigRational(BigInteger(x),BigInteger.One)
    new(x:uint64) = BigRational(BigInteger(x),BigInteger.One)

    
    // non-integer numeric constructors
    
    //new(x:decimal) = 



    static member private normalize (n:BigInteger) (d:BigInteger)  = 

        let gcd = BigInteger.GreatestCommonDivisor(n,d)
        if gcd.IsOne 
        then (n,d)
        else (n/gcd,d/gcd)

    // some accessors

    member x.Numerator = numerator
    member x.Denominator = denominator

    // Some contants
    static member Zero = BigRational(BigInteger.Zero,BigInteger.One)
    static member One = BigRational(BigInteger.One,BigInteger.One)

    // basic interfaces

    interface IEquatable<BigRational> with
        member x.Equals(y:BigRational) = 
            if x.Denominator = y.Denominator 
            then x.Numerator = y.Numerator
            else x.Numerator*y.Denominator = y.Numerator*x.Denominator
    
    override x.Equals(obj) = 
        match obj with
        | :? BigRational as r -> (x :> IEquatable<BigRational>).Equals(r)
        | _ -> false

    override x.GetHashCode() = x.Numerator.GetHashCode() ^^^ x.Denominator.GetHashCode()

    override x.ToString() = x.Numerator.ToString() + "/" + x.Denominator.ToString()


    interface IComparable<BigRational> with   
        member x.CompareTo(y) = BigInteger.Compare(x.Numerator*y.Denominator,y.Numerator*x.Denominator)


    interface IComparable with  
        member x.CompareTo y = 
            match y with
            | null -> 1
            | :? BigRational as r -> (x :> IComparable<BigRational>).CompareTo(r)
            | _ -> invalidArg "y" "Argument must be of type BigRational"


    // some conversions

    member x.ToBigInteger() = x.Numerator / x.Denominator
    member x.ToBigDecimal() = BigDecimal.Create(x.Numerator) / BigDecimal.Create(x.Denominator)
    member x.ToBigDecimal(c) = BigDecimal.Divide(BigDecimal.Create(x.Numerator), BigDecimal.Create(x.Denominator), c)

    // compatibility with JVM implementation
    member x.BigIntegerValue() = x.ToBigInteger()

    interface IConvertible with
        member x.GetTypeCode() = TypeCode.Object
        member x.ToBoolean(_:IFormatProvider) = not x.Numerator.IsZero
        member x.ToByte(_:IFormatProvider) = Convert.ToByte(x.ToBigInteger())
        member x.ToChar(_:IFormatProvider) = Convert.ToChar(x.ToBigInteger())
        member x.ToDateTime(_:IFormatProvider) = Convert.ToDateTime(x.ToBigInteger())
        member x.ToDecimal(fp:IFormatProvider) = (x.ToBigDecimal(Context.Decimal128) :> IConvertible).ToDecimal(fp)
        member x.ToDouble(fp:IFormatProvider) = (x.ToBigDecimal(Context.Decimal64) :> IConvertible).ToDouble(fp)       
        member x.ToInt16(_:IFormatProvider) = Convert.ToInt16(x.ToBigInteger())
        member x.ToInt32(_:IFormatProvider) = Convert.ToInt32(x.ToBigInteger())
        member x.ToInt64(_:IFormatProvider) = Convert.ToInt64(x.ToBigInteger())
        member x.ToSByte(_:IFormatProvider) = Convert.ToSByte(x.ToBigInteger())    
        member x.ToSingle(fp:IFormatProvider) = (x.ToBigDecimal(Context.Decimal32) :> IConvertible).ToSingle(fp)
        member x.ToString(_:IFormatProvider) = x.ToString()
        member x.ToType(conversionType: Type, fp:IFormatProvider) = Convert.ChangeType((x :> IConvertible).ToDouble(fp),conversionType,fp)
        member x.ToUInt16(_:IFormatProvider) = Convert.ToUInt16(x.ToBigInteger())
        member x.ToUInt32(_:IFormatProvider) = Convert.ToUInt32(x.ToBigInteger())
        member x.ToUInt64(_:IFormatProvider) = Convert.ToUInt64(x.ToBigInteger())

    // Some helpful properties

    member x.IsZero = x.Numerator.IsZero
    member x.IsPositive = x.Numerator.Sign > 0
    member x.IsNegative = x.Numerator.Sign < 0
    member x.Sign = x.Numerator.Sign


    // Arithmetic operations

    // -(c/d) = (-c)/d
    member x.Negate() = if x.IsZero then x else BigRational(-x.Numerator,x.Denominator)
    static member (~-) (x:BigRational) = x.Negate()
    static member (~+) (x:BigRational) = x


    // a/b + c/d = (ad+bc)/bd
    member x.Add(y:BigRational) =  BigRational(x.Numerator * y.Denominator + x.Denominator*y.Numerator,x.Denominator*y.Denominator) 
    static member Add(x:BigRational, y:BigRational) = x.Add(y)
    static member (+) (x:BigRational, y:BigRational) = x.Add(y)


    // a/b - c/d = (ad-bc)/bd
    member x.Subtract(y:BigRational) =  BigRational(x.Numerator * y.Denominator - x.Denominator*y.Numerator,x.Denominator*y.Denominator) 
    static member Subtract(x:BigRational, y:BigRational) = x.Subtract(y)
    static member (-) (x:BigRational, y:BigRational) = x.Subtract(y)


    // a/b * c/d = ac/bd
    member x.Multiply(y:BigRational) =  BigRational(x.Numerator * y.Numerator ,x.Denominator*y.Denominator) 
    static member Multiply(x:BigRational, y:BigRational) = x.Multiply(y)
    static member (*) (x:BigRational, y:BigRational) = x.Multiply(y)

    // a/b / c/d = ad/bc
    member x.Divide(y:BigRational) =  BigRational(x.Numerator * y.Denominator ,x.Numerator*y.Denominator) 
    static member Divide(x:BigRational, y:BigRational) = x.Divide(y)
    static member (/) (x:BigRational, y:BigRational) = x.Divide(y)

    // a/b % c/d = (ad % bc)/bd
    member x.Mod(y:BigRational) =  BigRational(x.Numerator * y.Denominator % x.Denominator*y.Numerator,x.Denominator*y.Denominator) 
    static member Mod(x:BigRational, y:BigRational) = x.Mod(y)
    static member (%) (x:BigRational, y:BigRational) = x.Mod(y)

    // a/b / c/d  == (ad)/(bc)
    // a/b % c/d  == (ad % bc)/bd
    member x.DivRem(y:BigRational, remainder:outref<BigRational>) = 
        let ad = x.Numerator*y.Denominator
        let bc = x.Denominator*y.Numerator
        let bd = x.Denominator*y.Denominator
        remainder <- BigRational(ad % bc, bd)
        ad/bc
    static member DivRem(x:BigRational, y:BigRational, remainder:outref<BigRational>) = x.DivRem(y,&remainder)

    
    // multiplicative inverse of a/b = b/a
    member x.Invert() = BigRational(x.Denominator,x.Numerator)
    static member Invert(x:BigRational) = x.Invert()

    member x.Pow(n:int) = 
        if n = 0
        then
            if x.IsZero 
            then raise <| ArithmeticException("Cannot compute 0**0")
            else BigRational.Zero
        elif n < 0
        then 
            if x.IsZero
            then raise <| ArithmeticException("Cannot raise zero to a negative exponent")
            else x.Invert().Pow(-n)
        else BigRational(BigInteger.Pow(x.Numerator,n),BigInteger.Pow(x.Denominator,n))


    // LCD( a/b, c/d ) = (bd) / GCD(b,d)
    static member LeastCommonDenominator(x:BigRational, y:BigRational) = 
        (x.Denominator * y.Denominator) / BigInteger.GreatestCommonDivisor(x.Denominator,y.Denominator)





   