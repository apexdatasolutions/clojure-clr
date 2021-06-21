namespace Clojure.Numerics

open System
open System.Text
open System.Numerics
open System.Globalization
open System.Runtime.InteropServices
open System.Runtime.CompilerServices


module private ArithmeticHelpers =

    let biFive = BigInteger(5)
    let biTen = BigInteger(10)

    let getBIPrecision (bi : BigInteger) =  
        if bi.IsZero then 1u 
        else
            let signFix = if bi.Sign < 0 then 1u else 0u
            (bi.ToString().Length |> uint) - signFix
           
        // I would do this, but we end up with a one-off error on exact powers of 10 due to Log10 inexactness
        //if bi.IsZero
        //then 1u
        //else
        //    let log = BigInteger.Log10 (if bi.Sign <= 0 then -bi else bi) 
        //    1u+((Math.Floor(log) |> uint32)   


    /// Exponent bias in the 64-bit floating point representation.
    let doubleExponentBias = 1023

    /// The size in bits of the significand in the 64-bit floating point representation.
    let doubleSignificandBitLength = 52

    /// How much to shift to accommodate the exponent and the binary digits of the significand.
    let doubleShiftBias = doubleExponentBias + doubleSignificandBitLength
    
    /// Extract the sign bit from a byte-array representaition of a double.
    let getDoubleSign (v:byte[]) = v.[7] &&& 0x80uy

    /// Extract the significand (AKA mantissa, coefficient) from a byte-array representation of a double.
    let getDoubleSignificand (v:byte[]) = 
        let i1 = (uint v.[0]) ||| ((uint v.[1]) <<< 8) ||| ((uint v.[2]) <<< 16) ||| ((uint v.[3]) <<< 24)
        let i2 = (uint v.[4]) ||| ((uint v.[5]) <<< 8) ||| ((uint (v.[6] &&& 0xFuy)) <<< 16)
        uint64 i1 ||| ((uint64 i2) <<< 32)
        
    /// Extract the exponent from a byte-array representaition of a double.
    let getDoubleBiasedExponent (v:byte[]) = ((uint16 (v.[7] &&& 0x7fuy)) <<< 4) ||| ((uint16 (v.[6] &&& 0xF0uy)) >>> 4)



    let biPowersOfTen = 
        [| 
            BigInteger.One
            BigInteger(10)
            BigInteger(100)
            BigInteger(1000)
            BigInteger(10000)
            BigInteger(100000)
            BigInteger(1000000)
            BigInteger(10000000)
            BigInteger(100000000)
            BigInteger(1000000000)
            BigInteger(10000000000L)
            BigInteger(100000000000L)
        |]

    let biPowerOfTen (n:uint) =
        if n < uint biPowersOfTen.Length then biPowersOfTen.[(int n)]
        else
            let buf = Array.create ((int n)+1) '0'
            buf.[0] <- '1'
            BigInteger.Parse(String(buf))

    let checkExponent (candidate:int64) (isZero:bool) : int option =
        match candidate with
        | x when x <= (int64 Int32.MaxValue) && x >= (int64 Int32.MinValue) -> Some (int32 x)
        | x when isZero && x < 0L -> Some Int32.MinValue
        | x when isZero && x > 0L -> Some Int32.MaxValue
        | _ -> None

    let checkExponentE (candidate:int64) (isZero:bool) : int =
        match checkExponent candidate isZero with
        | Some e -> e
        | None when candidate > 0L -> raise <| ArithmeticException("Overflow in scale")
        | None -> raise <| ArithmeticException("Underflow in scale")

/// Indicates the rounding method to use
type RoundingMode =
/// Round away from 0
    | Up
/// Truncate (round toward 0)
    | Down
/// Round toward positive infinity
    | Ceiling
/// Round toward negative infinity
    | Floor
/// Round to nearest neighbor, round up if equidistant
    | HalfUp
/// Round to nearest neighbor, round down if equidistant
    | HalfDown
/// Round to nearest neighbor, round to even neighbor if equidistant
    | HalfEven
/// <summary>
/// Do not do any rounding
/// </summary>
/// <remarks>This value is not part of the GDAS, but is in java.math.BigDecimal</remarks>
    | Unnecessary

/// Context for rounding
[<Struct>]
type Context =
    { 
        precision : uint32 
        roundingMode : RoundingMode 
    }

    static member Decimal32 = {precision =  7u; roundingMode = HalfEven}
    static member Decimal64 = {precision =  16u; roundingMode = HalfEven}
    static member Decimal128 = {precision =  34u; roundingMode = HalfEven}
    static member Unlimited = {precision =  0u; roundingMode = HalfUp}
    static member Default = {precision =  9ul; roundingMode = HalfUp}
    static member ExtendedDefault precision = {precision=precision; roundingMode = HalfEven}
    static member Create( precision, roundingMode) = {precision = precision; roundingMode = roundingMode }

[<Struct>]
type internal ParserSpan = {Start: int; Length: int}  // shall we go to the trouble of making these uints?

type internal ParseData =
    { wholePart : ParserSpan
      fractionPart : ParserSpan
      exponent: ParserSpan }

type internal ParseResult = ParseResult of ParseData * int


/// <summary>
/// Immutable, arbitrary precision, signed decimal.
/// </summary>
/// <remarks>
/// <para>This class is inspired by the General Decimal Arithmetic Specification (http://speleotrove.com/decimal/decarith.html, 
/// (PDF: http://speleotrove.com/decimal/decarith.pdf).  However, at the moment, the interface and capabilities comes closer
/// to java.math.BigDecimal, primarily because I only needed to mimic j.m.BigDecimal's capabilities to provide a minimum set
/// of functionality for ClojureCLR.</para>
/// <para>Because of this, as in j.m.BigDecimal, the implementation is closest to the X3.274 subset described in Appendix A
/// of the GDAS: infinite values, NaNs, subnormal values and negative zero are not represented, and most conditions throw exceptions. 
/// Exponent limits in the context are not implemented, except a limit to the range of an Int32. 
/// However, we do not do "conversion to shorter" for arith ops.</para>
/// <para>It is our long term intention to convert this to a complete implementation of the standard.</para>
/// <para>The representation is an arbitrary precision integer (the signed coefficient, also called the unscaled value) 
/// and an exponent.  The exponent is limited to the range of an Int32. 
/// The value of a BigDecimal representation is <c>coefficient * 10^exponent</c>. </para>
/// <para> Note: the representation in the GDAS is
/// [sign,coefficient,exponent] with sign = 0/1 for (pos/neg) and an unsigned coefficient. 
/// This yields signed zero, which we do not have.  
/// We used a BigInteger for the signed coefficient.  
/// That class does not have a representation for signed zero.</para>
/// <para>Note: Compared to j.m.BigDecimal, our coefficient = their <c>unscaledValue</c> 
/// and our exponent is the negation of their <c>scale</c>.</para>
/// <para>The representation also track the number of significant digits.  This is usually the number of digits in the coefficient,
/// except when the coeffiecient is zero.  This value is computed lazily and cached.</para>
/// <para>This is not a clean-room implementation.  
/// I examined at other code, especially OpenJDK implementation of java.math.BigDecimal, 
/// to look for special cases and other gotchas.  Then I looked away.  
/// I have tried to give credit in the few places where I pretty much did unthinking translation.  
/// However, there are only so many ways to skim certain cats, so some similarities are unavoidable.</para>
/// </remarks>
type BigDecimal private (coeff, exp, precision) = 
    let mutable precision : uint = precision

    static member Zero = BigDecimal(BigInteger.Zero,0,1u);
    static member One = BigDecimal(BigInteger.One,0,1u)
    static member Ten =  BigDecimal(new BigInteger(10),0,2u);        

    member private x.GetPrecision() = 
        match precision with    
        | 0u -> precision <- Math.Max(ArithmeticHelpers.getBIPrecision(coeff),1u)
        | _ -> ()
        precision

    member _.Coefficient = coeff
    member _.Exponent = exp
    member x.Precision = x.GetPrecision()
    member private x.RawPrecision = precision

    // Rounding/quantize/rescale

    static member private roundingDivide2 x y mode =
        let (q,r) = BigInteger.DivRem(x,y)  
        let isNeg = q.Sign < 0
        let cmp bi = BigInteger.Abs(bi+bi).CompareTo(y)
        let increment = 
            if r.IsZero then false
            else match mode with
                 | Unnecessary -> raise <| ArithmeticException("Rounding is required, but prohibited.")
                 | Ceiling -> not isNeg
                 | Floor -> isNeg
                 | Down-> false
                 | Up -> true
                 | HalfDown -> (cmp r) > 0;
                 | HalfUp -> (cmp r) >= 0
                 | HalfEven -> let c = (cmp r) in (c > 0) || (c = 0 &&  not q.IsEven)
        if increment then 
            if q.Sign < 0 || (q.IsZero && x.Sign < 0) then q - BigInteger.One
            else q + BigInteger.One
        else q

    static member private round (v:BigDecimal) (c:Context): BigDecimal = 
        let vp = v.GetPrecision()
        if ( vp <= c.precision ) 
        then v
        else
            let drop = vp - c.precision
            let divisor = ArithmeticHelpers.biPowerOfTen(drop)
            let rounded = BigDecimal.roundingDivide2 v.Coefficient divisor c.roundingMode
            let exp = ArithmeticHelpers.checkExponentE ((int64 v.Exponent)+(int64 drop)) rounded.IsZero
            let result = BigDecimal(rounded,exp,0u)
            if c.precision > 0u 
            then BigDecimal.round result c
            else result

    static member Round(v:BigDecimal, c:Context) = BigDecimal.round v c 
    member x.Round(c:Context) = BigDecimal.Round(x,c)

    static member Rescale(lhs:BigDecimal, newExponent,mode) : BigDecimal =

        let increaseExponent delta = 
            // delta negative => increasing the exponent => we might have to round to a new precision
            let decrease = -delta |> uint
            let p = lhs.Precision
            if p < decrease then BigDecimal(BigInteger.Zero,newExponent,0u)
            else   
                let newPrecision = p-decrease
                let r = lhs.Round({precision=newPrecision;roundingMode=mode})
                if (r.Exponent = newExponent)
                then r
                else BigDecimal.Rescale(r,newExponent,mode)

        let decreaseExponent delta = 
            // delta positive => decrease the exponent => multiply by 10^some power and don't underflow
            let newCoeff = lhs.Coefficient * ArithmeticHelpers.biPowerOfTen(delta)
            let oldPrec = lhs.Precision
            let newPrec = oldPrec + (if oldPrec = 0u then 0u else delta)
            BigDecimal(newCoeff,newExponent,newPrec)    

            
        let delta = ArithmeticHelpers.checkExponentE ((int64 lhs.Exponent) - (int64 newExponent)) false
        if delta = 0 then lhs
        elif lhs.Coefficient.IsZero then BigDecimal(BigInteger.Zero,newExponent,0u)
        elif delta < 0 then increaseExponent delta
        else decreaseExponent (uint delta)



    static member Quantize(lhs:BigDecimal, rhs:BigDecimal, mode:RoundingMode) = BigDecimal.Rescale(lhs,rhs.Exponent,mode)
    member x.Quantize(v,mode) = BigDecimal.Quantize(x,v,mode)


    static member Create (bi:BigDecimal) = BigDecimal(bi.Coefficient,bi.Exponent,bi.RawPrecision)
    static member Create(coeff, exp, [<Optional; DefaultParameterValue( 0u )>]prec) = BigDecimal(coeff,exp,prec)

    static member Create (v:int32) = BigDecimal(BigInteger(v),0,0u) 
    static member CreateC(v:int32,c) = BigDecimal.round (BigDecimal.Create(v)) c
    
    static member Create (v:int64) = BigDecimal(BigInteger(v),0,0u) 
    static member CreateC(v:int64,c) = BigDecimal.round (BigDecimal.Create(v)) c

    static member Create (v:uint64) = BigDecimal(BigInteger(v),0,0u) 
    static member CreateC(v:uint64,c) = BigDecimal.round (BigDecimal.Create(v)) c

    static member Create (v:BigInteger) = BigDecimal(v,0,0u) 
    static member CreateC(v:BigInteger,c) = BigDecimal.round (BigDecimal.Create(v)) c

    static member Create (v:decimal) = 
        if v = 0m then BigDecimal.Zero
        else 
            let ints = Decimal.GetBits(v)
            let sign = if v < 0m then -1 else 1
            let exp = (ints.[3] &&& 0x00FF0000 ) >>> 16
            let byteLength = Buffer.ByteLength(ints)
            let bytes : byte array = Array.zeroCreate byteLength
            Buffer.BlockCopy(ints,0,bytes,0,byteLength)
            let coeff = BigInteger(ReadOnlySpan(bytes),false)   // Fix sign vs 2complement
            BigDecimal(coeff,-exp,0u)
    static member CreateC(v:decimal, c) = BigDecimal.round (BigDecimal.Create(v)) c

    static member Create (v:double) = 
        if Double.IsNaN(v) then invalidArg "value" "NaN is not supported in BigDecimal"
        if Double.IsInfinity(v) then invalidArg "value" "Infinity is not supported in BigDecimal"

        let dbytes = BitConverter.GetBytes(v)
        let signficand = ArithmeticHelpers.getDoubleSignificand dbytes
        let biasedExp = ArithmeticHelpers.getDoubleBiasedExponent dbytes
        let leftShift = (int biasedExp) - ArithmeticHelpers.doubleShiftBias;
        if signficand = 0UL && biasedExp = 0us then BigDecimal(BigInteger.Zero,0,1u)
        else
            let ( coeff, leftShift ) =
                if ( signficand = 0UL) 
                then ( ( if v < 0.0 then BigInteger.MinusOne else BigInteger.One),
                       (int biasedExp) - ArithmeticHelpers.doubleExponentBias )
                else 
                    let unadjustedCoeff = BigInteger(signficand ||| 0x10000000000000UL)
                    ( (if v < 0.0 then BigInteger.Negate(unadjustedCoeff) else unadjustedCoeff),
                      leftShift)
            let (coeffToUse, expToUse) =
                if leftShift < 0
                    then ( coeff*BigInteger.Pow(ArithmeticHelpers.biFive,-leftShift), leftShift)
                elif leftShift > 0
                    then ( coeff <<< leftShift , 0 )
                else ( coeff, 0 )
            BigDecimal(coeffToUse,expToUse,0u)
                                  


    static member CreateC(v:double, c) = BigDecimal.round (BigDecimal.Create(v)) c

    static member private CreateInternalCh (v:char[]) (c: Context option) = NotImplementedException() |> raise       
    static member Create (v) = BigDecimal.CreateInternalCh v None
    static member CreateC(v,c) = BigDecimal.CreateInternalCh v (Some c) 


    member _.ToScientificString() =
        let sb = StringBuilder(coeff.ToString())
        let coeffLen, negOffset = if coeff.Sign < 0 then sb.Length-1, 1 else sb.Length, 0

        let adjustedExp = (int64 exp) + (int64 coeffLen) - 1L

        if exp <= 0 && adjustedExp >= -6L
        then
            if exp <> 0  // we need a decimal point
            then
                match -exp with
                | _ as numDec when numDec < coeffLen -> sb.Insert(coeffLen-numDec+negOffset, '.') |> ignore
                | _ as numDec when numDec = coeffLen -> sb.Insert(negOffset,"0.") |> ignore
                | _ as numDec ->
                    let numZeros = numDec - coeffLen
                    sb.Insert(negOffset, "0",numZeros) |> ignore
                    sb.Insert(negOffset, "0.") |> ignore
            else ()
        else // using exponential notation
            if coeffLen > 1 then sb.Insert(negOffset+1, '.') |> ignore
            sb.Append('E') |> ignore
            if adjustedExp >= 0L then sb.Append('+') |> ignore
            sb.Append(adjustedExp) |> ignore
        sb.ToString()



    override x.ToString() : String = x.ToScientificString()



    // Using ReadOnlySpan<char> (ROSC) as input to consolidate versions for char arrays, strings, and similar.
    // Unfortunately, using spans has a lot of limitations, so I could not use them through most of the code.
    // Opted instead to convert to a char array and use (offset,length) pairs (ParserSpan) to encode regions.
    // Using a parsing model simlar to parser combinators, but much simpler because we have a simple linear model:
    //  +/- whole-part . fraction-part E +/- exponent
    // All parts are optional with the following constraints:
    //  (1) there must be a digit in whole-part + fraction-part
    //  (2) if there is an E, there must be a digit in exponent 
  
    static member private DoParse (buf : ReadOnlySpan<char> ) : Result<BigDecimal,String> =
        let input = buf.ToArray()
        
        let inputIsEmpty posn = posn >= input.Length

        let isSign ch = ch = '+' || ch = '-'

        let hasLeadingSign = not (inputIsEmpty 0) && isSign input.[0]

        let numDigits (pspan : ParserSpan) =
            match pspan with
            | {Length=0} -> 0
            | {Length=len; Start = posn} when isSign input.[posn] -> len-1
            | {Length=len} -> len

        let parseNonEmpty pr : Result<ParseResult, string> =
            match input.Length with
            | 0 -> Error "No characters"
            | _ -> Ok pr

        let parseDigits (posn:int) (leadingSignOk : bool) (noDigitsOk : bool) : Result<ParserSpan*int, string> =
            let leadingSignPresent = not (inputIsEmpty posn) && (isSign input.[posn])
            if leadingSignPresent &&  not leadingSignOk  
            then Error "Misplaced +/-"
            else
                let startPosn = if leadingSignPresent then posn+1 else posn
                let mutable i = startPosn
                while  i < input.Length  && Char.IsDigit(input.[i]) do  // TODO: use a sequence function for this.
                    i <- i+1
                if i=startPosn  && not noDigitsOk
                then  Error "missing digits"
                else Ok ({Start = posn; Length = i-posn},i) 

        let parseWhole (ParseResult(data,posn)) : Result<ParseResult,string> = 
            match (parseDigits posn true true) with
            | Ok (span, nextPosn) -> Ok (ParseResult({data with wholePart = span},nextPosn))
            | Error msg -> Error msg

        let parseFraction (ParseResult(data,posn) as pr) : Result<ParseResult,string> = 
            if inputIsEmpty posn || input.[posn] <> '.' 
            then Ok pr
            else 
                match parseDigits (posn+1) false true with
                | Ok (start, rest) -> Ok (ParseResult({data with fractionPart = start}, rest))
                | Error msg -> Error msg

                
        let parseExponent (ParseResult(data,posn) as pr) : Result<ParseResult, string> = 
            if inputIsEmpty posn  || (input.[posn] <> 'E' && input.[posn] <> 'e') 
            then Ok pr
            else
                match parseDigits (posn+1) true false with
                | Ok (span, nextPosn) -> Ok (ParseResult({data with exponent = span}, nextPosn))
                | Error msg -> Error "No digits in coefficient"
        
        let parseEmpty (ParseResult(data,posn) as pr) : Result<ParseResult, string> =
            if inputIsEmpty posn then Ok pr else Error "Unused characters at end"

        let parseConsistencyChecks (ParseResult(data,posn) as pr) : Result<ParseResult, string> =
            let numWholePartDigits = if isSign input.[0] then data.wholePart.Length-1 else data.wholePart.Length
            if numWholePartDigits = 0 && data.fractionPart.Length = 0
            then Error "No digits in coefficient"
            else Ok pr

        let leadingZeroCountInSpan (span : ParserSpan) =
            let nonZeroChar c = c <> '0'
            let hasLeadingSign = not (inputIsEmpty span.Start) && isSign input.[span.Start]
            let (skip,maxCount) = if hasLeadingSign then (span.Start+1,span.Length-1) else (span.Start,span.Length)
            let found = 
                input 
                |> Seq.skip skip
                |> Seq.tryFindIndex nonZeroChar
            match found with  // remenmber to skip the leading sign, which will exist when we call this
            | Some index ->  Math.Min(index,maxCount)
            | None -> maxCount

        let leadingZeroCount (data:ParseData) =
            let wholeZeroCount = leadingZeroCountInSpan data.wholePart
            let wholeDigitCount = numDigits data.wholePart
            if wholeZeroCount = wholeDigitCount
            then wholeZeroCount + leadingZeroCountInSpan data.fractionPart
            else wholeZeroCount

        let givenExponent (data:ParseData) =
            match data.exponent.Length with 
            | 0 -> Ok 0
            | _ ->
                match Int32.TryParse((new ReadOnlySpan<char>(input, data.exponent.Start, data.exponent.Length)), NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
                | true, i -> Ok i 
                | _ -> Error "Invalid exponent"

        let computeExponent (data:ParseData) isZero =
            let indicatedExponent = -data.fractionPart.Length
            match givenExponent data with
            | Ok 0 as v -> Ok indicatedExponent
            | Ok exp -> 
                match ArithmeticHelpers.checkExponent ((int64 indicatedExponent) + (int64 exp)) isZero with
                | Some e -> Ok e
                | None -> Error "Invalid exponent"
            | Error msg as v -> Error msg

        let constructBD (data:ParseData) : Result<BigDecimal,string> =
            let digits = Array.zeroCreate(data.wholePart.Length + data.fractionPart.Length)
            Array.Copy(input, data.wholePart.Start, digits, 0, data.wholePart.Length)
            Array.Copy(input, data.fractionPart.Start, digits, data.wholePart.Length, data.fractionPart.Length)
            let bi = (BigInteger.Parse(String(digits)))
            let precision = (numDigits data.wholePart) + data.fractionPart.Length - leadingZeroCount(data)
            let precision = if precision = 0 || bi.IsZero then 1 else precision
            match computeExponent data  bi.IsZero with
            | Ok exp -> Ok (BigDecimal(bi,exp,uint precision))
            | Error msg -> Error msg

        let emptyData = {wholePart = {Start=0;Length=0}; fractionPart={Start=0;Length=0}; exponent={Start=0;Length=0}}
        let result = 
            Ok (ParseResult(emptyData,0))
            |> Result.bind parseNonEmpty
            |> Result.bind parseWhole
            |> Result.bind parseFraction
            |> Result.bind parseExponent
            |> Result.bind parseEmpty
            |> Result.bind parseConsistencyChecks

        match result with
        | Error x -> Error x
        | Ok (ParseResult(data,b)) as pr -> constructBD data

    static member private DoParseE (buf : ReadOnlySpan<char> ) : BigDecimal =
        match BigDecimal.DoParse buf with
        | Ok bd -> bd
        | Error x -> invalidArg "input" x

    static member Parse (s:String) = BigDecimal.DoParseE(s.AsSpan())
    static member Parse (s: String, c) = BigDecimal.round (BigDecimal.Parse(s)) c

    static member TryParse(s:String, value:outref<BigDecimal>) : bool = 
        match BigDecimal.DoParse (s.AsSpan()) with
        | Ok bd -> value <- bd; true
        | Error _ -> false

    static member TryParse(s:String, c, value:outref<BigDecimal>) : bool = 
        match BigDecimal.DoParse (s.AsSpan()) with
        | Ok bd -> value <- BigDecimal.round bd c; true
        | Error _ -> false

    // Arithmetic operations

    member x.Negate() = if x.Coefficient.IsZero then x else BigDecimal(BigInteger.Negate(x.Coefficient),x.Exponent,x.RawPrecision)
    member x.Negate(c) = BigDecimal.round (x.Negate()) c
    static member Negate(x : BigDecimal) = x.Negate()
    static member Negate(x:BigDecimal, c) = x.Negate(c)

    
    member x.Abs() = if x.Coefficient.Sign < 0 then x.Negate() else x
    member x.Abs(c) = if x.Coefficient.Sign < 0 then x.Negate(c) else BigDecimal.round x c
    static member Abs(x:BigDecimal) = x.Abs()
    static member Abs(x:BigDecimal, c) = x.Abs(c)

    /// Align the bigger BigDecimal by increasing its coefficient and decreasing its exponent
    static member private computeAlign (big:BigDecimal) (small:BigDecimal) =
        let deltaExp = (big.Exponent - small.Exponent) |> uint
        BigDecimal(big.Coefficient * ArithmeticHelpers.biPowerOfTen(deltaExp), small.Exponent, 0u)

    /// Create matching pair with the same alignment (exponent)
    static member private align (x:BigDecimal) (y:BigDecimal) =
        if y.Exponent > x.Exponent then x, BigDecimal.computeAlign y x
        elif x.Exponent > y.Exponent then BigDecimal.computeAlign x y, y
        else x, y

    /// Compute the sum with y
    member x.Add (y:BigDecimal) = 
        let xa, ya = BigDecimal.align x y in BigDecimal(xa.Coefficient + ya.Coefficient,xa.Exponent,0u)
       
    /// Compute the sum with y, result rounded by context
    member x.Add(y:BigDecimal, c:Context) = 
        // TODO: Optimize for one arg or the other being zero.
        // TODO: Optimize for differences in exponent along with the desired precision is large enough that the add is irreleveant
        // Translated the Sun Java code pretty directly.
        let result = x.Add(y)
        if c.precision = 0u || c.roundingMode = RoundingMode.Unnecessary then result else BigDecimal.round result c

    /// Compute x+y
    static member Add(x:BigDecimal, y:BigDecimal) = x.Add(y)

    /// Compute x+y, result rounded per the context
    static member Add(x:BigDecimal, y:BigDecimal, c:Context) = x.Add(y,c)
    
    /// Compute x+y
    static member (+) (x:BigDecimal, y:BigDecimal) = x.Add(y)

    /// Compute the difference with y
    member x.Subtract (y:BigDecimal) = 
        let xa, ya = BigDecimal.align x y in BigDecimal(xa.Coefficient - ya.Coefficient,xa.Exponent,0u)
   
    /// Compute the different with y, result rounded by context
    member x.Subtract(y:BigDecimal, c:Context) = 
        // TODO: Optimize for one arg or the other being zero.
        // TODO: Optimize for differences in exponent along with the desired precision is large enough that the add is irreleveant
        // Translated the Sun Java code pretty directly.
        let result = x.Subtract(y)
        if c.precision = 0u || c.roundingMode = RoundingMode.Unnecessary then result else BigDecimal.round result c

    /// Compute x+y
    static member Subtract(x:BigDecimal, y:BigDecimal) = x.Subtract(y)

    /// Compute x+y, result rounded per the context
    static member Subtract(x:BigDecimal, y:BigDecimal, c:Context) = x.Subtract(y,c)

    /// Compute x+y
    static member (-) (x:BigDecimal, y:BigDecimal) = x.Subtract(y)

    /// Compute (-x)
    static member (~-) (x:BigDecimal) = x.Negate()


    /// Compute the product with y
    member x.Multiply (y:BigDecimal) = BigDecimal(x.Coefficient * y.Coefficient, x.Exponent+y.Exponent,0u)

    /// Compute the product with y, result rounded by context
    member x.Multiply(y:BigDecimal, c:Context) = BigDecimal.round (x.Multiply(y)) c

    /// Compute x*y
    static member Multiply(x:BigDecimal, y:BigDecimal) = x.Multiply(y)

    /// Compute x*y, result rounded by context
    static member Multiply(x:BigDecimal, y:BigDecimal, c:Context) = x.Multiply(y,c)

    /// Compute x*y
    static member (*) (x:BigDecimal, y:BigDecimal) = x.Multiply(y)





//           [Serializable]
//           public class BigDecimal : IComparable, IComparable<BigDecimal>, IEquatable<BigDecimal>, IConvertible
//           {



//               #region Factory methods

//               // I went with factory methods rather than constructors so that I could, if I wanted,
//               // return cached values for things such as zero, one, etc.


//               /// <summary>
//               /// Create a BigDecimal from a double.
//               /// </summary>
//               /// <param name="v">The double value</param>
//               /// <returns>A BigDecimal corresponding to the double value.</returns>
//               /// <remarks>Watch out!  BigDecimal.Create(0.1) is not the same as BigDecimal.Parse("0.1").  
//               /// We create exact representations of doubles,
//               /// and 1/10 does not have an exact representation as a double.  So the double 1.0 is not exactly 1/10.</remarks>
//               public static BigDecimal Create(double v)
//               {
//               }

//               /// <summary>
//               /// Create a BigDecimal from a double, rounded as specified.
//               /// </summary>
//               /// <param name="v">The double value</param>
//               /// <param name="c">The rounding context</param>
//               /// <returns>A BigDecimal corresponding to the double value, rounded as specified.</returns>
//               /// <remarks>Watch out!  BigDecimal.Create(0.1) is not the same as BigDecimal.Parse("0.1").  
//               /// We create exact representations of doubles,
//               /// and 1/10 does not have an exact representation as a double.  So the double 1.0 is not exactly 1/10.</remarks>
//               public static BigDecimal Create(double v, Context c)
//               {

//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given Int32.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <returns>A BigDecimal with the same value.</returns>
//               public static BigDecimal Create(int v)
//               {

//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given Int32, rounded appropriately.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <param name="c">The rounding context</param>
//               /// <returns>A BigDecimal with the same value, appropriately rounded</returns>
//               public static BigDecimal Create(int v, Context c)
//               {

//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given Int64.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <returns>A BigDecimal with the same value.</returns>
//               public static BigDecimal Create(long v)
//               {
//                   return new BigDecimal(BigInteger.Create(v), 0);
//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given Int64, rounded appropriately.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <param name="c">The rounding context</param>
//               /// <returns>A BigDecimal with the same value, appropriately rounded</returns>
//               public static BigDecimal Create(long v, Context c)
//               {
//                   BigDecimal d = new(BigInteger.Create(v), 0);
//                   d.RoundInPlace(c);
//                   return d;
//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given UInt64.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <returns>A BigDecimal with the same value.</returns>
//               public static BigDecimal Create(ulong v)
//               {
//                   return new BigDecimal(BigInteger.Create(v), 0);
//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given UInt64, rounded appropriately.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <param name="c">The rounding context</param>
//               /// <returns>A BigDecimal with the same value, appropriately rounded</returns>
//               public static BigDecimal Create(ulong v, Context c)
//               {
//                   BigDecimal d = new(BigInteger.Create(v), 0);
//                   d.RoundInPlace(c);
//                   return d;
//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given Decimal.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <returns>A BigDecimal with the same value.</returns>
//               public static BigDecimal Create(decimal v)
//               {
//                   int[] bits = Decimal.GetBits(v);

//                   uint[] data = new uint[3];
//                   data[0] = (uint)bits[2];
//                   data[1] = (uint)bits[1];
//                   data[2] = (uint)bits[0];

//                   int sign = (bits[3] & 0x80000000) == 0 ? 1 : -1;
//                   int exp = (bits[3] & 0x00FF0000) >> 16;

//                   bool isZero = data[0] == 0U && data[1] == 0U && data[2] == 0U;

//                   BigInteger coeff = isZero ? BigInteger.Zero : new BigInteger(sign, data);

//                   return new BigDecimal(coeff, -exp);
//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given UInt64, rounded appropriately.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <param name="c">The rounding context</param>
//               /// <returns>A BigDecimal with the same value, appropriately rounded</returns>
//               public static BigDecimal Create(decimal v, Context c)
//               {
//                   BigDecimal d = Create(v);
//                   d.RoundInPlace(c);
//                   return d;
//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given BigInteger.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <returns>A BigDecimal with the same value.</returns>
//               public static BigDecimal Create(BigInteger v)
//               {
//                   return new BigDecimal(v, 0);
//               }

//               /// <summary>
//               /// Create a BigDecimal with the same value as the given BigInteger, rounded appropriately.
//               /// </summary>
//               /// <param name="v">The initial value</param>
//               /// <param name="c">The rounding context</param>
//               /// <returns>A BigDecimal with the same value, appropriately rounded</returns>
//               public static BigDecimal Create(BigInteger v, Context c)
//               {
//                   BigDecimal d = new(v, 0);
//                   d.RoundInPlace(c);
//                   return d;
//               }

//               /// <summary>
//               /// Create a BigDecimal by parsing a string.
//               /// </summary>
//               /// <param name="v"></param>
//               /// <returns></returns>
//               public static BigDecimal Create(String v)
//               {
//                   return BigDecimal.Parse(v);
//               }


//               /// <summary>
//               /// Create a BigDecimal by parsing a string.
//               /// </summary>
//               /// <param name="v"></param>
//               /// <returns></returns>        
//               public static BigDecimal Create(String v, Context c)
//               {
//                   return BigDecimal.Parse(v, c);
//               }


//               /// <summary>
//               /// Create a BigDecimal by parsing a character array.
//               /// </summary>
//               /// <param name="v"></param>
//               /// <returns></returns>
//               public static BigDecimal Create(char[] v)
//               {
//                   return BigDecimal.Parse(v);
//               }


//               /// <summary>
//               /// Create a BigDecimal by parsing a character array.
//               /// </summary>
//               /// <param name="v"></param>
//               /// <returns></returns>
//               public static BigDecimal Create(char[] v, Context c)
//               {
//                   return BigDecimal.Parse(v,c);
//               }


//               /// <summary>
//               /// Create a BigDecimal by parsing a segment of character array.
//               /// </summary>
//               /// <param name="v"></param>
//               /// <returns></returns>
//               public static BigDecimal Create(char[] v, int offset, int len)
//               {
//                   return BigDecimal.Parse(v,offset,len);
//               }

//               /// <summary>
//               /// Create a BigDecimal by parsing a segment of character array.
//               /// </summary>
//               /// <param name="v"></param>
//               /// <returns></returns>
//               public static BigDecimal Create(char[] v, int offset, int len, Context c)
//               {
//                   return BigDecimal.Parse(v, offset, len, c);
//               }

//               #endregion

//               #region C-tors

//               /// <summary>
//               /// Creates a copy of given BigDecimal.
//               /// </summary>
//               /// <param name="copy">A copy of the given BigDecimal</param>
//               /// <remarks>Really only needed internally.  BigDecimals are immutable, so why copy?  
//               /// Internally, we sometimes need to copy and modify before releasing into the wild.</remarks>
//#pragma warning disable IDE0051 // Remove unused private members
//               BigDecimal(BigDecimal copy)
//#pragma warning restore IDE0051 // Remove unused private members
//                   : this(copy._coeff,copy._exp,copy._precision)
//               {
//               }

//               /// <summary>
//               /// Create a BigDecimal with given coefficient, exponent, and precision.
//               /// </summary>
//               /// <param name="coeff">The coefficient</param>
//               /// <param name="exp">The exponent</param>
//               /// <param name="precision">The precision</param>
//               /// <remarks>For internal use only.  We can't trust someone outside to set the precision for us.
//               /// Only for use when we know the precision explicitly.</remarks>
//               BigDecimal(BigInteger coeff, int exp, uint precision)
//               {
//                   _coeff = coeff;
//                   _exp = exp;
//                   _precision = precision;
//               }

//               /// <summary>
//               /// Create a BigDecimal with given coefficient and exponent.
//               /// </summary>
//               /// <param name="coeff">The coefficient</param>
//               /// <param name="exp">The exponent</param>
//               public BigDecimal(BigInteger coeff, int exp)
//                   : this(coeff, exp, 0)
//               {
//               }

//               #endregion

//               #region Object overrides

//               public override bool Equals(object obj)
//               {
//                   BigDecimal d = obj as BigDecimal;
//                   if (d == null)
//                       return false;

//                   return Equals(d);
//               }


//               // Stole this from Util.  Eventually, use Murmur.
//               static public int HashCombine(int seed, int hash)
//               {
//                   //a la boost
//                   return (int)(seed ^ (hash + 0x9e3779b9 + (seed << 6) + (seed >> 2)));

//               }

//               public override int GetHashCode()
//               {
//                   // Originall call to Util.hashCombine
//                   return HashCombine(_coeff.GetHashCode(),_exp.GetHashCode());
//               }

//               #endregion

//               #region String parsing

//               /// <summary>
//               /// Create a BigDecimal from a string representation
//               /// </summary>
//               /// <param name="s"></param>
//               /// <returns></returns>
//               public static BigDecimal Parse(string s)
//               {
//                   DoParse(s.ToCharArray(), 0, s.Length, true, out BigDecimal v);
//                   return v;
//               }

//               /// <summary>
//               /// Create a BigDecimal from a string representation, rounded as indicated.
//               /// </summary>
//               /// <param name="s"></param>
//               /// <param name="c"></param>
//               /// <returns></returns>
//               public static BigDecimal Parse(string s, Context c)
//               {
//                   DoParse(s.ToCharArray(), 0, s.Length, true, out BigDecimal v);
//                   v.RoundInPlace(c);
//                   return v;
//               }

//               /// <summary>
//               /// Try to create a BigDecimal from a string representation.
//               /// </summary>
//               /// <param name="s">The string to convert</param>
//               /// <param name="v">Set to the BigDecimal corresponding to the string.</param>
//               /// <returns>True if successful, false if there is an error parsing.</returns>
//               public static bool TryParse(string s, out BigDecimal v)
//               {
//                   return DoParse(s.ToCharArray(),0, s.Length, false, out v);
//               }


//               /// <summary>
//               /// Try to create a BigDecimal from a string representation, rounded as indicated.
//               /// </summary>
//               /// <param name="s">The string to convert</param>
//               /// <param name="c">The rounding context</param>
//               /// <param name="v">Set to the BigDecimal corresponding to the string.</param>
//               /// <returns></returns>
//               public static bool TryParse(string s, Context c, out BigDecimal v)
//               {
//                   bool result;
//                   if ((result = DoParse(s.ToCharArray(), 0, s.Length, false, out v)))
//                       v.RoundInPlace(c);
//                   return result;
//               }

//               /// <summary>
//               /// Create a BigDecimal from an array of characters.
//               /// </summary>
//               /// <param name="buf"></param>
//               /// <returns></returns>
//               public static BigDecimal Parse(char[] buf)
//               {
//                   DoParse(buf, 0, buf.Length, true, out BigDecimal v);
//                   return v;
//               }

//               /// <summary>
//               /// Create a BigDecimal from an array of characters, rounded as indicated.
//               /// </summary>
//               /// <param name="buf"></param>
//               /// <param name="c"></param>
//               /// <returns></returns>
//               public static BigDecimal Parse(char[] buf, Context c)
//               {
//                   DoParse(buf, 0, buf.Length, true, out BigDecimal v);
//                   v.RoundInPlace(c);
//                   return v;
//               }

//               /// <summary>
//               /// Try to create a BigDecimal from an array of characters.
//               /// </summary>
//               /// <param name="buf"></param>
//               /// <param name="v"></param>
//               /// <returns>True if successful; false otherwise</returns>      
//               public static bool TryParse(char[] buf, out BigDecimal v)
//               {
//                   return DoParse(buf, 0, buf.Length, false, out v);
//               }

//               /// <summary>
//               /// Try to create a BigDecimal from an array of characters, rounded as indicated.
//               /// </summary>
//               /// <param name="buf"></param>
//               /// <param name="c"></param>
//               /// <param name="v"></param>
//               /// <returns>True if successful; false otherwise</returns>      
//               public static bool TryParse(char[] buf, Context c, out BigDecimal v)
//               {
//                   bool result;
//                   if ((result = DoParse(buf, 0, buf.Length, false, out v)))
//                       v.RoundInPlace(c);
//                   return result;
//               }


//               /// <summary>
//               /// Create a BigDecimal corresponding to a sequence of characters from an array.
//               /// </summary>
//               /// <param name="buf"></param>
//               /// <param name="offset"></param>
//               /// <param name="len"></param>
//               /// <returns></returns>
//               public static BigDecimal Parse(char[] buf, int offset, int len)
//               {
//                   DoParse(buf, offset, len, true, out BigDecimal v);
//                   return v;
//               }

//               /// <summary>
//               /// Create a BigDecimal corresponding to a sequence of characters from an array, rounded as indicated.
//               /// </summary>
//               /// <param name="buf"></param>
//               /// <param name="offset"></param>
//               /// <param name="len"></param>
//               /// <param name="c"></param>
//               /// <returns></returns>
//               public static BigDecimal Parse(char[] buf, int offset, int len, Context c)
//               {
//                   DoParse(buf, offset, len, true, out BigDecimal v);
//                   v.RoundInPlace(c);
//                   return v;
//               }

//               /// <summary>
//               /// Try to create a BigDecimal corresponding to a sequence of characters from an array.
//               /// </summary>
//               /// <param name="buf"></param>
//               /// <param name="offset"></param>
//               /// <param name="len"></param>
//               /// <param name="v"></param>
//               /// <returns></returns>
//               public static bool TryParse(char[] buf, int offset, int len, out BigDecimal v)
//               {
//                   return DoParse(buf, offset, len, false, out v);
//               }

//               /// <summary>
//               /// Try to create a BigDecimal corresponding to a sequence of characters from an array.
//               /// </summary>
//               /// <param name="buf"></param>
//               /// <param name="offset"></param>
//               /// <param name="len"></param>
//               /// <param name="c"></param>
//               /// <param name="v"></param>
//               /// <returns></returns>
//               public static bool TryParse(char[] buf, int offset, int len, Context c, out BigDecimal v)
//               {
//                   bool result;
//                   if ((result = DoParse(buf, offset, len, false, out v)))
//                       v.RoundInPlace(c);
//                   return result;
//               }


//               #region Conversion to string

//               #endregion

//               #region IComparable Members

//               public int CompareTo(object obj)
//               {
//                   if (obj == null)
//                       return 1;

//                   if (obj is BigDecimal d)
//                       return CompareTo(d);
//                   throw new ArgumentException("Expected a BigDecimal to compare against");
//               }

//               #endregion

//               #region IComparable<BigDecimal> Members

//               public int CompareTo(BigDecimal other)
//               {
//                   BigDecimal d1 = this;
//                   BigDecimal d2 = other;
//                   Align(ref d1, ref d2);
//                   return d1._coeff.CompareTo(d2._coeff);
//               }

//               #endregion

//               #region IEquatable<BigDecimal> Members

//               public bool Equals(BigDecimal other)
//               {
//                   if ( other is null)
//                       return false;
//                   if (_exp != other._exp)
//                       return false;
//                   return _coeff.Equals(other._coeff);
//               }

//               #endregion

//               #region IConvertible Members

//               public TypeCode GetTypeCode()
//               {
//                   return TypeCode.Object;
//               }

//               public bool ToBoolean(IFormatProvider provider)
//               {
//                   return !IsZero;
//               }

//               public byte ToByte(IFormatProvider provider)
//               {
//                   return ToBigInteger().ToByte(provider);
//               }

//               public char ToChar(IFormatProvider provider)
//               {
//                   return ToBigInteger().ToChar(provider);
//               }

//               public DateTime ToDateTime(IFormatProvider provider)
//               {
//                   throw new InvalidCastException();
//               }

//               static readonly BigDecimal ClrDecimalMin = Create(Decimal.MinValue);
//               static readonly BigDecimal ClrDecimalMax = Create(Decimal.MaxValue);
                   
//               public decimal ToDecimal(IFormatProvider provider)
//               {
//                   if (this < ClrDecimalMin || this > ClrDecimalMax)
//                       throw new InvalidOperationException("BigDecimal value out of decimal range");

//                   if ( IsZero )
//                       return Decimal.Zero;

//                   uint[] data = _coeff.GetMagnitude();
//                   int length = data.Length;

//                   if (length <= 3 && -28 <= _exp && _exp <= 0)
//                   {
//                       int lo = 0, mi = 0, hi = 0;

//                       switch (length)
//                       {
//                           case 1:
//                               lo = (int)data[0];
//                               break;
//                           case 2:
//                               lo = (int)data[1];
//                               mi = (int)data[0];
//                               break;
//                           case 3:
//                               lo = (int)data[2];
//                               mi = (int)data[1];
//                               hi = (int)data[0];
//                               break;
//                       }

//                       return new Decimal(lo, mi, hi, IsNegative, (byte)(-_exp));
//                   }

//                   // do it the dumb way
//                   return Decimal.Parse(ToString());
//               }

//               public double ToDouble(IFormatProvider provider)
//               {
//                   // As j.m.BigDecimal puts it: "Somewhat inefficient, but guaranteed to work."
//                   // However, JVM's double parser goes to +/- Infinity when out of range,
//                   // while CLR's throws an exception.
//                   // Hate dealing with that.
//                   try
//                   {
//                       return Double.Parse(ToString(), provider);
//                   }
//                   catch (OverflowException)
//                   {
//                       return IsNegative ? Double.NegativeInfinity : Double.PositiveInfinity;
//                   }
//               }

//               public short ToInt16(IFormatProvider provider)
//               {
//                   return ToBigInteger().ToInt16(provider);
//               }

//               public int ToInt32(IFormatProvider provider)
//               {
//                   return ToBigInteger().ToInt32(provider);
//               }

//               public long ToInt64(IFormatProvider provider)
//               {
//                   return ToBigInteger().ToInt64(provider);
//               }

//               public sbyte ToSByte(IFormatProvider provider)
//               {
//                   return ToBigInteger().ToSByte(provider);
//               }

//               public float ToSingle(IFormatProvider provider)
//               {
//                   return (float)ToDouble(provider);
//               }

//               public string ToString(IFormatProvider provider)
//               {
//                   return ToString();
//               }

//               public object ToType(Type conversionType, IFormatProvider provider)
//               {
//                   return Convert.ChangeType(ToDouble(provider), conversionType, provider);
//               }

//               public ushort ToUInt16(IFormatProvider provider)
//               {
//                   return ToBigInteger().ToUInt16(provider);
//               }

//               public uint ToUInt32(IFormatProvider provider)
//               {
//                   return ToBigInteger().ToUInt32(provider);
//               }

//               public ulong ToUInt64(IFormatProvider provider)
//               {
//                   return ToBigInteger().ToUInt64(provider);
//               }

//               public BigInteger ToBigInteger()
//               {
//                   return Rescale(this, 0, RoundingMode.Down)._coeff;
//               }

//               #endregion

//               #region Arithmetic operators

//               public static bool operator ==(BigDecimal x, BigDecimal y)
//               {
//                   if (ReferenceEquals(x, y))
//                       return true;

//                   return !(x is null) && !(y is null) && x.Equals(y);
//               }

//               public static bool operator !=(BigDecimal x, BigDecimal y)
//               {
//                   return !(x == y);
//               }

//               public static bool operator <(BigDecimal x, BigDecimal y)
//               {
//                   return x.CompareTo(y) < 0;
//               }

//               public static bool operator >(BigDecimal x, BigDecimal y)
//               {
//                   return x.CompareTo(y) > 0;
//               }



//               /// <summary>
//               /// Compute <paramref name="x"/> / <paramref name="y"/>.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <param name="y"></param>
//               /// <returns>The quotient</returns>
//               public static BigDecimal operator /(BigDecimal x, BigDecimal y)
//               {
//                   return x.Divide(y);
//               }

//               /// <summary>
//               /// Compute <paramref name="x"/> % <paramref name="y"/>.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <param name="y"></param>
//               /// <returns>The modulus</returns>
//               public static BigDecimal operator %(BigDecimal x, BigDecimal y)
//               {
//                   return x.Mod(y);
//               }

//               #endregion



//               /// <summary>
//               /// Compute <paramref name="x"/> / <paramref name="y"/>.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <param name="y"></param>
//               /// <returns>The quotient</returns>
//               public static BigDecimal Divide(BigDecimal x, BigDecimal y)
//               {
//                   return x.Divide(y);
//               }

//               /// <summary>
//               /// Compute <paramref name="x"/> / <paramref name="y"/>, with result rounded according to the context.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <param name="y"></param>
//               /// <returns>The quotient</returns>
//               public static BigDecimal Divide(BigDecimal x, BigDecimal y, Context c)
//               {
//                   return x.Divide(y,c);
//               }

//               /// <summary>
//               /// Returns <paramref name="x"/> % <paramref name="y"/>.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <param name="y"></param>
//               /// <returns>The modulus</returns>
//               public static BigDecimal Mod(BigDecimal x, BigDecimal y)
//               {
//                   return x.Mod(y);
//               }


//               /// <summary>
//               /// Returns <paramref name="x"/> % <paramref name="y"/>, with result rounded according to the context.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <param name="y"></param>
//               /// <returns>The modulus</returns>
//               public static BigDecimal Mod(BigDecimal x, BigDecimal y, Context c)
//               {
//                   return x.Mod(y,c);
//               }

//               /// <summary>
//               /// Compute the quotient and remainder of dividing one <see cref="BigInteger"/> by another.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <param name="y"></param>
//               /// <param name="remainder">Set to the remainder after division</param>
//               /// <returns>The quotient</returns>
//               public static BigDecimal DivRem(BigDecimal x, BigDecimal y, out BigDecimal remainder)
//               {
//                   return x.DivRem(y, out remainder);
//               }
               
//               /// <summary>
//               /// Compute the quotient and remainder of dividing one <see cref="BigInteger"/> by another, with result rounded according to the context.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <param name="y"></param>
//               /// <param name="remainder">Set to the remainder after division</param>
//               /// <returns>The quotient</returns>
//               public static BigDecimal DivRem(BigDecimal x, BigDecimal y, Context c, out BigDecimal remainder)
//               {
//                   return x.DivRem(y, c, out remainder);
//               }



//               /// <summary>
//               /// Returns a <see cref="BigInteger"/> raised to an int power.
//               /// </summary>
//               /// <param name="x">The value to exponentiate</param>
//               /// <param name="exp">The exponent</param>
//               /// <returns>The exponent</returns>
//               public static BigDecimal Power(BigDecimal x, int exp)
//               {
//                   return x.Power(exp);
//               }


//               /// <summary>
//               /// Returns a <see cref="BigInteger"/> raised to an int power, with result rounded according to the context.
//               /// </summary>
//               /// <param name="x">The value to exponentiate</param>
//               /// <param name="exp">The exponent</param>
//               /// <returns>The exponent</returns>
//               public static BigDecimal Power(BigDecimal x, int exp, Context c)
//               {
//                   return x.Power(exp,c);
//               }


//               /// <summary>
//               /// Returns this.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <returns></returns>
//               public static BigDecimal Plus(BigDecimal x)
//               {
//                   return x;
//               }

//               public static BigDecimal Plus(BigDecimal x, Context c)
//               {
//                   return x.Plus(c);
//               }

//               /// <summary>
//               /// Returns the negation of this.
//               /// </summary>
//               /// <param name="x"></param>
//               /// <returns></returns>
//               public static BigDecimal Minus(BigDecimal x)
//               {
//                   return x.Negate();
//               }

//               public static BigDecimal Minus(BigDecimal x, Context c)
//               {
//                   return x.Negate(c);
//               }


//               #endregion

//               #region Arithmetic methods


           





//               /// <summary>
//               /// Returns this / y.
//               /// </summary>
//               /// <param name="y">The divisor</param>
//               /// <returns>The quotient</returns>
//               /// <exception cref="ArithmeticException">If rounding mode is RoundingMode.UNNECESSARY and we have a repeating fraction"</exception>
//               /// <remarks>I completely ripped off the OpenJDK implementation.  
//               /// Their analysis of the basic algorithm I could not compete with.</remarks>
//               public BigDecimal Divide(BigDecimal divisor)
//               {
//                   BigDecimal dividend = this;

//                   if ( divisor._coeff.IsZero ) // x/0
//                   { 
//                       if ( dividend._coeff.IsZero ) // 0/0
//                           throw new ArithmeticException("Division undefined (0/0)"); // NaN
//                       throw new ArithmeticException("Division by zero"); // INF
//                   }

//                   // Calculate preferred exponent
//                   int preferredExp = 
//                       (int)Math.Max(Math.Min((long)dividend._exp - divisor._exp,
//                                               Int32.MaxValue),
//                                     Int32.MinValue);

//                   if ( dividend._coeff.IsZero )  // 0/y
//                       return new BigDecimal(BigInteger.Zero,preferredExp);



//                   /*  OpenJDK says:
//                    * If the quotient this/divisor has a terminating decimal
//                    * expansion, the expansion can have no more than
//                    * (a.precision() + ceil(10*b.precision)/3) digits.
//                    * Therefore, create a MathContext object with this
//                    * precision and do a divide with the UNNECESSARY rounding
//                    * mode.
//                    */
//                   Context c = new( (uint)Math.Min(dividend.GetPrecision() +
//                                                          (long)Math.Ceiling(10.0*divisor.GetPrecision()/3.0),
//                                                                   Int32.MaxValue),
//                                                     RoundingMode.Unnecessary);
//                   BigDecimal quotient;
//                   try
//                   {
//                       quotient = dividend.Divide(divisor, c);
//                   }
//                   catch (ArithmeticException )
//                   {
//                       throw new ArithmeticException("Non-terminating decimal expansion; no exact representable decimal result.");
//                   }

//                   int quotientExp = quotient._exp;

//                   // divide(BigDecimal, mc) tries to adjust the quotient to
//                   // the desired one by removing trailing zeros; since the
//                   // exact divide method does not have an explicit digit
//                   // limit, we can add zeros too.

//                   if (preferredExp < quotientExp)
//                       return Rescale(quotient, preferredExp, RoundingMode.Unnecessary);

//                   return quotient;
//               }


//               /// <summary>
//               /// Returns this / y.
//               /// </summary>
//               /// <param name="y">The divisor</param>
//               /// <param name="mc">The context</param>
//               /// <returns>The quotient</returns>
//               /// <remarks>
//               /// <para>The specification talks about the division algorithm in terms of repeated subtraction.
//               /// I'll try to re-analyze this in terms of divisions on integers.</para>
//               /// <para>Assume we want to divide one BigDecimal by another:</para>
//               /// <code> [x,a] / [y,b] = [(x/y), a-b]</code>
//               /// <para>where [x,a] signifies x is integer, a is exponent so [x,a] has value x * 10^a.
//               /// Here, (x/y) indicates a result rounded to the desired precision p. For the moment, assume x, y non-negative.</para>
//               /// <para>We want to compute (x/y) using integer-only arithmetic, yielding a quotient+remainder q+r
//               /// where q has up to p precision and r is used to compute the rounding.  So actually, the result will be [q, a-b+c],
//               /// where c is some adjustment factor to make q be in the range [0,10^0).</para>
//               /// <para>We will need to adjust either x or y to make sure we can compute x/y and make q be in this range.</para>
//               /// <para>Let px be the precision of x (number of digits), let py be the precision of y. Then </para>
//               /// <code>
//               /// x = x' * 10^px
//               /// y = y' * 10^py
//               /// </code>
//               /// <para>where x' and y' are in the range [.1,1).  However, we'd really like to have:</para>
//               /// <code>
//               /// (a) x' in [.1,1)
//               /// (b) y' in [x',10*x')
//               /// </code>
//               /// <para>So that  x'/y' is in the range (.1,1].  
//               /// We can use y' as defined above if y' meets (b), else multiply y' by 10 (and decrease py by 1). 
//               /// Having done this, we now have</para>
//               /// <code>
//               ///  x/y = (x'/y') * 10^(px-py)
//               /// </code>
//               /// <para>
//               /// This gives us  10^(px-py-1) &lt; x/y &lt 10^(px-py).
//               /// We'd like q to have p digits of precision.  So,
//               /// <code>
//               /// if px-py = p, ok.
//               /// if px-py &lt; p, multiply x by 10^(p - (px-py)).
//               /// if px-py &gt; p, multiply y by 10^(px-py-p).
//               /// </code>
//               /// <para>Using these adjusted values of x and y, divide to get q and r, round using those, then adjust the exponent.</para>
//               /// </remarks>
//               public BigDecimal Divide(BigDecimal rhs, Context c)
//               {
//                   if (c.Precision == 0)
//                       return Divide(rhs);

//                   BigDecimal lhs = this;

//                   long preferredExp = (long)lhs._exp - rhs._exp;

//                   // Deal with x or y being zero.

//                   if (rhs._coeff.IsZero)
//                   {      // x/0
//                       if (lhs._coeff.IsZero)    // 0/0
//                           throw new ArithmeticException("Division undefined");  // NaN
//                       throw new ArithmeticException("Division by zero");  // Inf
//                   }
//                   if (lhs._coeff.IsZero)        // 0/y
//                       return new BigDecimal(BigInteger.Zero,
//                                             (int)Math.Max(Math.Min(preferredExp,
//                                                                    Int32.MaxValue),
//                                                           Int32.MinValue));
//                   int xprec = (int)lhs.GetPrecision();
//                   int yprec = (int)rhs.GetPrecision();

//                   // Determine if we need to make an adjustment to get x', y' into relation (b).
//                   BigInteger x = lhs._coeff;
//                   BigInteger y = rhs._coeff;

//                   BigInteger xtest = x.Abs();
//                   BigInteger ytest = y.Abs();
//                   if (xprec < yprec)
//                       xtest = x.Multiply(BIPowerOfTen(yprec - xprec));
//                   else if (xprec > yprec)
//                       ytest = y.Multiply(BIPowerOfTen(xprec - yprec));


//                   int adjust = 0;
//                   if (ytest < xtest)
//                   {
//                       y = y.Multiply(BigInteger.Ten);
//                       adjust = 1;
//                   }

//                   // Now make sure x and y themselves are in the proper range.

//                   int delta = (int)c.Precision - (xprec - yprec);
//                   if ( delta > 0 )
//                       x = x.Multiply(BIPowerOfTen(delta));
//                   else if ( delta < 0 )
//                       y = y.Multiply(BIPowerOfTen(-delta));

//                   BigInteger roundedInt = RoundingDivide2(x, y, c.RoundingMode);

//                   int exp = CheckExponent(preferredExp - delta + adjust, roundedInt.IsZero);

//                   BigDecimal result = new(roundedInt, exp);

//                   result.RoundInPlace(c);


//                   // Thanks to the OpenJDK implementation for pointing this out.
//                   // TODO: Have ROundingDivide2 return a flag indicating if the remainder is 0.  Then we can lose the multiply.
//                   if (result.Multiply(rhs).CompareTo(this) == 0)
//                   {
//                       // Apply preferred scale rules for exact quotients
//                       return result.StripZerosToMatchExponent(preferredExp);
//                   }
//                   else
//                   {
//                       return result;
//                   }      

           
//                  // if (c.RoundingMode == RoundingMode.Ceiling ||
//                  //     c.RoundingMode == RoundingMode.Floor)
//                  // {
//                  //     // OpenJDK code says:
//                  //     // The floor (round toward negative infinity) and ceil
//                  //     // (round toward positive infinity) rounding modes are not
//                  //     // invariant under a sign flip.  If xprime/yprime has a
//                  //     // different sign than lhs/rhs, the rounding mode must be
//                  //     // changed.
//                  //     if ((xprime._coeff.Signum != lhs._coeff.Signum) ^
//                  //         (yprime._coeff.Signum != rhs._coeff.Signum))
//                  //     {
//                  //         c = new Context(c.Precision,
//                  //                              (c.RoundingMode == RoundingMode.Ceiling) ?
//                  //                              RoundingMode.Floor : RoundingMode.Ceiling);
//                  //     }
//                  // }
//               }



//               /// <summary>
//               /// Returns this % y
//               /// </summary>
//               /// <param name="y">The divisor</param>
//               /// <returns>The modulus</returns>
//               public BigDecimal Mod(BigDecimal y)
//               {
//                   DivRem(y, out BigDecimal r);
//                   return r;
//               }


//               /// <summary>
//               /// Returns this % y
//               /// </summary>
//               /// <param name="y">The divisor</param>
//               /// <returns>The modulus</returns>
//               public BigDecimal Mod(BigDecimal y, Context c)
//               {
//                   DivRem(y, c, out BigDecimal r);
//                   return r;
//               }

//               /// <summary>
//               /// Returns the quotient and remainder of this divided by another.
//               /// </summary>
//               /// <param name="y">The divisor</param>
//               /// <param name="remainder">The remainder</param>
//               /// <returns>The quotient</returns>
//               public BigDecimal DivRem(BigDecimal y, out BigDecimal remainder)
//               {
//                   // x = q * y + r
//                   BigDecimal q = this.DivideInteger(y);
//                   remainder = this - q * y;
//                   return q;
//               }


//               /// <summary>
//               /// Returns the quotient and remainder of this divided by another.
//               /// </summary>
//               /// <param name="y">The divisor</param>
//               /// <param name="remainder">The remainder</param>
//               /// <returns>The quotient</returns>
//               public BigDecimal DivRem(BigDecimal y, Context c, out BigDecimal remainder)
//               {
//                   // x = q * y + r
//                   if (c.RoundingMode == RoundingMode.Unnecessary)
//                       return DivRem(y, out remainder);

//                   BigDecimal q = this.DivideInteger(y,c);
//                   remainder = this - q * y;
//                   return q;
//               }


//               /// <summary>
//               /// 
//               /// </summary>
//               /// <param name="y"></param>
//               /// <param name="c"></param>
//               /// <returns></returns>
//               /// <remarks>I am indebted to the OpenJDK implementation for the algorithm.
//               /// <para>However, the spec I'm working from specifies an exponent of zero always!
//               /// The OpenJDK implementation does otherwise.  So I've modified it to yield a zero exponent.</para>
//               /// </remarks>
//               public BigDecimal DivideInteger(BigDecimal y, Context c)
//               {
//                   if (c.Precision == 0 ||                        // exact result
//                       (this.Abs().CompareTo(y.Abs()) < 0)) // zero result
//                       return DivideInteger(y);

//                   // Calculate preferred scale
//                   //int preferredExp = (int)Math.Max(Math.Min((long)this._exp - y._exp,
//                   //                                            Int32.MaxValue),Int32.MinValue);
//                   int preferredExp = 0;

//                   /*  OpenJKD says:
//                    * Perform a normal divide to mc.precision digits.  If the
//                    * remainder has absolute value less than the divisor, the
//                    * integer portion of the quotient fits into mc.precision
//                    * digits.  Next, remove any fractional digits from the
//                    * quotient and adjust the scale to the preferred value.
//                    */
//                   BigDecimal result = this.Divide(y, new Context(c.Precision,RoundingMode.Down));
//                   int resultExp = result._exp;

//                   if (resultExp > 0)
//                   {
//                       /*
//                        * Result is an integer. See if quotient represents the
//                        * full integer portion of the exact quotient; if it does,
//                        * the computed remainder will be less than the divisor.
//                        */
//                       BigDecimal product = result.Multiply(y);
//                       // If the quotient is the full integer value,
//                       // |dividend-product| < |divisor|.
//                       if (this.Subtract(product).Abs().CompareTo(y.Abs()) >= 0)
//                       {
//                           throw new ArithmeticException("Division impossible");
//                       }
//                   }
//                   else if (resultExp < 0)
//                   {
//                       /*
//                        * Integer portion of quotient will fit into precision
//                        * digits; recompute quotient to scale 0 to avoid double
//                        * rounding and then try to adjust, if necessary.
//                        */
//                       result = Rescale(result,0, RoundingMode.Down);
//                   }
//                   // else resultExp == 0;

//                   //int precisionDiff;
//                   if ((preferredExp < resultExp) &&
//                       (/*precisionDiff = */(int)(c.Precision - result.GetPrecision())) > 0)
//                   {
//                       //return Rescale(result, resultExp + Math.Max(precisionDiff, preferredExp - resultExp), RoundingMode.Unnecessary);
//                       return Rescale(result, 0, RoundingMode.Unnecessary);

//                   }
//                   else
//                       return result.StripZerosToMatchExponent(preferredExp);
//               }


//               /// <summary>
//               /// Return the integer part of this / y.
//               /// </summary>
//               /// <param name="y"></param>
//               /// <returns></returns>
//               /// <remarks>I am indebted to the OpenJDK implementation for the algorithm.
//               /// <para>However, the spec I'm working from specifies an exponent of zero always!
//               /// The OpenJDK implementation does otherwise.  So I've modified it to yield a zero exponent.</para>
//               /// </remarks>
//               public BigDecimal DivideInteger(BigDecimal y)
//               {

//                   // Calculate preferred exponent
//                   //int preferredExp = (int)Math.Max(Math.Min((long)this._exp - y._exp,
//                   //                                            Int32.MaxValue), Int32.MinValue);
//                   int preferredExp = 0;

//                   if (Abs().CompareTo(y.Abs()) < 0)
//                   {
//                       return new BigDecimal(BigInteger.Zero, preferredExp);
//                   }

//                   if (this._coeff.IsZero && !y._coeff.IsZero)
//                       return Rescale(this, preferredExp, RoundingMode.Unnecessary);

//                   // Perform a divide with enough digits to round to a correct
//                   // integer value; then remove any fractional digits

//                   int maxDigits = (int)Math.Min(this.GetPrecision() +
//                                                 (long)Math.Ceiling(10.0 * y.GetPrecision() / 3.0) +
//                                                 Math.Abs((long)this._exp - y._exp) + 2,
//                                                 Int32.MaxValue);

//                   BigDecimal quotient = this.Divide(y, new Context((uint)maxDigits, RoundingMode.Down));
//                   if (y._exp < 0)
//                   {
//                       quotient = Rescale(quotient, 0, RoundingMode.Down).StripZerosToMatchExponent(preferredExp);
//                   }

//                   if (quotient._exp > preferredExp)
//                   {
//                       // pad with zeros if necessary
//                       quotient = Rescale(quotient, preferredExp, RoundingMode.Unnecessary);
//                   }

//                   return quotient;
//               }        




//               /// <summary>
//               /// Returns the value of this instance raised to an integral power.
//               /// </summary>
//               /// <param name="exp">The exponent</param>
//               /// <returns>The exponetiated value</returns>
//               /// <exception cref="System.ArgumentOutOfRangeException">Thrown if the exponent is negative.</exception>
//               public BigDecimal Power(int n)
//               {
//                   if (n < 0 || n > 999999999)
//                       throw new ArithmeticException("Invalid operation");

//                   int exp = CheckExponent((long)_exp * n);
//                   return new BigDecimal(_coeff.Power(n), exp);
//               }

//               /// <summary>
//               /// Returns the value of this instance raised to an integral power.
//               /// </summary>
//               /// <param name="exp">The exponent</param>
//               /// <returns>The exponetiated value</returns>
//               /// <exception cref="System.ArgumentOutOfRangeException">Thrown if the exponent is negative.</exception>
//               /// <remarks><para>Follows the OpenJKD implementation.  This is an implementation of the X3.274-1996 algorithm:</para>
//               /// <list>
//               ///   <item> An ArithmeticException exception is thrown if
//               ///     <list>
//               ///       <item>abs(n) > 999999999</item>
//               ///       <item>c.precision == 0 and code n &lt; 0</item>
//               ///       <item>c.precision > 0 and n has more than c.precision decimal digits</item>
//               ///     </list>
//               ///   </item>
//               ///   <item>if n is zero, ONE is returned even if this is zero, otherwise
//               ///     <list>        
//               ///       <item>if n is positive, the result is calculated via
//               ///             the repeated squaring technique into a single accumulator.
//               ///             The individual multiplications with the accumulator use the
//               ///             same math context settings as in c except for a
//               ///             precision increased to c.precision + elength + 1
//               ///             where elength is the number of decimal digits in n.
//               ///       </item>       
//               ///       <item>if n is negative, the result is calculated as if
//               ///             n were positive; this value is then divided into one
//               ///             using the working precision specified above.
//               ///       </item>
//               ///       <item>The final value from either the positive or negative case
//               ///              is then rounded to the destination precision.
//               ///        </item>
//               ///     </list>
//               ///  </list>
//               /// </remarks>
//               public BigDecimal Power(int n, Context c)
//               {
//                   if (c.Precision == 0)
//                       return Power(n);
//                   if (n < -999999999 || n > 999999999)
//                       throw new ArithmeticException("Invalid operation");
//                   if (n == 0)
//                       return One;                      
//                   BigDecimal lhs = this;
//                   Context workc = c;           
//                   int mag = Math.Abs(n);               
//                   if (c.Precision > 0)
//                   {
//                       int elength = (int)BigInteger.UIntPrecision((uint)mag);
//                       if (elength > c.Precision)        // X3.274 rule
//                           throw new ArithmeticException("Invalid operation");
//                       workc = new Context((uint)(c.Precision + elength + 1),c.RoundingMode);
//                   }

//                   BigDecimal acc = One;           
//                   bool seenbit = false;        
//                   for (int i = 1; ; i++)
//                   {            
//                       mag += mag;                 // shift left 1 bit
//                       if (mag < 0)
//                       {              // top bit is set
//                           seenbit = true;         // OK, we're off
//                           acc = acc.Multiply(lhs, workc); // acc=acc*x
//                       }
//                       if (i == 31)
//                           break;                  // that was the last bit
//                       if (seenbit)
//                           acc = acc.Multiply(acc, workc);   // acc=acc*acc [square]
//                       // else (!seenbit) no point in squaring ONE
//                   }
//                   // if negative n, calculate the reciprocal using working precision
//                   if (n < 0)                          // [hence mc.precision>0]
//                       acc = One.Divide(acc, workc);
//                   // round to final precision and strip zeros
//                   return acc.Round(c);
//               }


//               public BigDecimal Plus()
//               {
//                   return this;
//               }

//               public BigDecimal Plus(Context c)
//               {
//                   if ( c.Precision == 0 )
//                       return this;
//                   return this.Round(c);
//               }

               
//               public BigDecimal Minus()
//               {
//                   return Negate();
//               }

//               public BigDecimal Minus(Context c)
//               {
//                   return Negate(c);
//               }
               

//               #endregion

//               #region Other computed values

//               /// <summary>
//               /// Does this BigDecimal have a zero value?
//               /// </summary>
//               public bool IsZero
//               {
//                   get { return _coeff.IsZero; }
//               }

//               /// <summary>
//               /// Does this BigDecimal represent a positive value?
//               /// </summary>
//               public bool IsPositive
//               {
//                   get { return _coeff.IsPositive; }
//               }

//               /// <summary>
//               /// Does this BigDecimal represent a negative value?
//               /// </summary>
//               public bool IsNegative
//               {
//                   get { return _coeff.IsNegative; }
//               }

//               /// <summary>
//               /// Returns the sign (-1, 0, +1) of this BigDecimal.
//               /// </summary>
//               public int Signum
//               {
//                   get { return _coeff.Signum; }
//               }


//               public BigDecimal MovePointRight(int n)
//               {
//                   int newExp = CheckExponent((long)_exp + n);
//                   BigDecimal d = new(_coeff, newExp);
//                   return d;
//               }

//               public BigDecimal MovePointLeft(int n)
//               {
//                   int newExp = CheckExponent((long)_exp - n);
//                   BigDecimal d = new(_coeff, newExp);
//                   return d;
//               }

               



//               #endregion

//               #region Exponent computations

//               /// <summary>
//               /// Check to see if the result of exponent arithmetic is valid.
//               /// </summary>
//               /// <param name="candidate">The value resulting from exponent arithmetic.</param>
//               /// <param name="isZero">Are we computing an exponent for a zero coefficient?</param>
//               /// <param name="exponent">The exponent to use</param>
//               /// <returns>True if the candidate is valid, false otherwise.</returns>
//               /// <remarks>
//               /// <para>Exponent arithmetic during various operations may result in values
//               /// that are out of range of an Int32.  We can do the computation as a long,
//               /// then use this to make sure the result is okay to use.</para>
//               /// <para>If the exponent is out of range, but the coefficient is zero,
//               /// the exponent in some sense is not that relevant, so we just clamp to 
//               /// the appropriate (pos/neg) extreme value for Int32.  (This handling inspired by 
//               /// the OpenJKD implementation.)</para>
//               /// </remarks>
//               static bool CheckExponent(long candidate, bool isZero, out int exponent)
//               {
//                   exponent = (int)candidate;
//                   if (exponent == candidate)
//                       return true;

//                   // We have underflow/overflow.
//                   // If Zero, use the max value of the appropriate sign.
//                   if (isZero)
//                   {
//                       exponent = candidate > Int32.MaxValue ? Int32.MaxValue : Int32.MinValue;
//                       return true;
//                   }

//                   return false;
//               }

//               /// <summary>
//               /// Reduce exponent to Int32.  Throw error if out of range.
//               /// </summary>
//               /// <param name="candidate">The value resulting from exponent arithmetic.</param>
//               /// <param name="isZero">Are we computing an exponent for a zero coefficient?</param>
//               /// <returns>The exponent to use</returns>
//               static int CheckExponent(long candidate, bool isZero)
//               {
//                   bool result = CheckExponent(candidate, isZero, out int exponent);
//                   if (result)
//                       return exponent;

//                   // Report error condition
//                   if (candidate > Int32.MaxValue)
//                       throw new ArithmeticException("Overflow in scale");
//                   else
//                       throw new ArithmeticException("Underflow in scale");
//               }

//#pragma warning disable IDE0051 // Remove unused private members
//               bool CheckExponent(long candidate, out int exponent)
//#pragma warning restore IDE0051 // Remove unused private members
//               {
//                   return CheckExponent(candidate, _coeff.IsZero, out exponent);
//               }

//               int CheckExponent(long candidate)
//               {
//                   return CheckExponent(candidate, _coeff.IsZero);
//               }
         
//               static BigInteger BIPowerOfTen(int n)
//               {
//                   if ( n < 0 )
//                       throw new ArgumentException("Power of ten must be non-negative");

//                   if (n < _maxCachedPowerOfTen)
//                       return _biPowersOfTen[n];

//                   char[] buf = new char[n + 1];
//                   buf[0] = '1';
//                   for (int i = 1; i <= n; i++)
//                       buf[i] = '0';
//                   return BigInteger.Parse(new String(buf));
//               }

//               static readonly BigInteger[] _biPowersOfTen = new BigInteger[] {
//                   BigInteger.One,
//                   BigInteger.Ten,
//                   BigInteger.Create(100),
//                   BigInteger.Create(1000),
//                   BigInteger.Create(10000),
//                   BigInteger.Create(100000),
//                   BigInteger.Create(1000000),
//                   BigInteger.Create(10000000),
//                   BigInteger.Create(100000000),
//                   BigInteger.Create(1000000000),
//                   BigInteger.Create(10000000000),
//                   BigInteger.Create(100000000000)
//               };
               
//               static readonly int _maxCachedPowerOfTen = _biPowersOfTen.Length;

//               /// <summary>
//               /// Remove insignificant trailing zeros from this BigDecimal until the 
//               /// preferred exponent is reached or no more zeros can be removed.
//               /// </summary>
//               /// <param name="preferredExp"></param>
//               /// <returns></returns>
//               /// <remarks>
//               /// <para>Took this one from OpenJDK implementation, with some minor edits.</para>
//               /// <para>Modifies its argument.  Use only on a new BigDecimal.</para>
//               /// </remarks>
//               private BigDecimal StripZerosToMatchExponent(long preferredExp)
//               {
//                   while (_coeff.Abs().CompareTo(BigInteger.Ten) >= 0 && _exp < preferredExp)
//                   {
//                       if (_coeff.IsOdd)
//                           break;                  // odd number.  cannot end in 0
//                       BigInteger quo = _coeff.DivRem(BigInteger.Ten, out BigInteger rem);
//                       if (!rem.IsZero)
//                           break;   // non-0 remainder
//                       _coeff = quo;
//                       _exp = CheckExponent((long)_exp + 1);// could overflow
//                       if (_precision > 0)  // adjust precision if known
//                           _precision--;
//                   }

//                   return this;
//               }


//               /// <summary>
//               /// Returns a BigDecimal numerically equal to this one, but with 
//               /// any trailing zeros removed.
//               /// </summary>
//               /// <returns></returns>
//               /// <remarks>Ended up needing this in ClojureCLR, grabbed from OpenJDK.</remarks>
//               public BigDecimal StripTrailingZeros()
//               {    
//                   BigDecimal result = new(this._coeff,this._exp);
//                   result.StripZerosToMatchExponent(Int64.MaxValue);
//                   return result;
//               }

//               #endregion

//               #region Rounding/quantize/rescale
           
//           }
//}