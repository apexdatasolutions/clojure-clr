namespace Clojure.Numerics

open System
open System.Text
open System.Numerics
open System.Globalization
open System.Runtime.InteropServices
open System.Runtime.CompilerServices


module private ArithmeticHelpers =

    let getBIPrecision bi =  Math.Ceiling(BigInteger.Log10(bi)) |> uint32         

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
        let i1 = (v.[0] |> uint) ||| (v.[1] |> uint) ||| (v.[2] |> uint) ||| (v.[3] |> uint)
        let i2 = (v.[4] |> uint) ||| (v.[5] |> uint) ||| (((v.[6] &&& 0xFuy) |> uint) <<< 16)
        uint64 i1 ||| ((uint64 i2) <<<32)
        
    /// Extract the exponent from a byte-array representaition of a double.
    let getDoubleBiasedExponent (v:byte[]) = ((uint16 (v.[7] &&& 0x7fuy)) <<< 4) ||| ((uint16 (v.[6] &&& 0xFuy)) >>> 4)

    let biFive = BigInteger(5)

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

type RoundingMode =
    | Up
    | Down
    | Ceiling
    | Floor
    | HalfUp
    | HalfDown
    | HalfEven
    | Unnecessary

[<Struct>]
type Context =
    private { 
        precision : uint32 
        roundingMode : RoundingMode 
    }

    static member Decimal32 = {precision =  7u; roundingMode = HalfEven}
    static member Decimal64 = {precision =  16u; roundingMode = HalfEven}
    static member Decimal128 = {precision =  34u; roundingMode = HalfEven}
    static member Unlimited = {precision =  0u; roundingMode = HalfUp}
    static member Default = {precision =  9ul; roundingMode = HalfUp}
    static member ExtendedDefault precision = {precision=precision; roundingMode = HalfEven}

[<Struct>]
type internal ParserSpan = {Start: int; Length: int}  // shall we go to the trouble of making these uints?

type internal ParseData =
    { wholePart : ParserSpan
      fractionPart : ParserSpan
      exponent: ParserSpan }

type internal ParseResult = ParseResult of ParseData * int

type BigDecimal private (coeff, exp, precision) = 
    let mutable precision : uint = precision

    static member Zero = BigDecimal(BigInteger.Zero,0,1u);
    static member One = BigDecimal(BigInteger.One,0,1u)
    static member Ten =  BigDecimal(new BigInteger(10),0,2u);        

    member private x.GetPrecision() = 
        match precision with    
        | 0u -> precision <- ArithmeticHelpers.getBIPrecision(coeff)
        | _ -> ()
        precision

    member _.Coefficient = coeff
    member _.Exponent = exp
    member x.Precision = x.GetPrecision()
    member private x.RawPrecision = precision

    static member private roundingDivide2 x y mode =
        let (q,r) = BigInteger.DivRem(x,y)  
        let isNeg = q.Sign < 0
        let cmp bi = BigInteger.Abs(bi+bi).CompareTo(y)
        let increment = 
            if r.IsZero then false
            else match mode with
                 | Unnecessary -> raise <| ArithmeticException("Rounding required, but prohibited.")
                 | Ceiling -> not isNeg
                 | Floor -> isNeg
                 | Down-> false
                 | Up -> true
                 | HalfDown -> (cmp r) > 0;
                 | HalfUp -> (cmp r) >= 0
                 | HalfEven -> let c = (cmp r) in (c > 0) || (c = 0 &&  q.IsEven)
        if increment then 
            if q.Sign < 0 || (q.IsZero && x.Sign < 0) then q - BigInteger.One
            else q + BigInteger.One
        else q

    static member private round (v:BigDecimal) (c:Context): BigDecimal = 
        let vp = v.GetPrecision()
        if ( vp <= c.precision ) then v
        else
            let drop = vp - c.precision
            let divisor = ArithmeticHelpers.biPowerOfTen(drop)
            let rounded = BigDecimal.roundingDivide2 v.Coefficient divisor c.roundingMode
            let exp = ArithmeticHelpers.checkExponentE ((int64 v.Exponent)+(int64 drop)) rounded.IsZero
            let result = BigDecimal(rounded,exp,0u)
            if c.precision > 0u then BigDecimal.round result c
            else result

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
        if Double.IsNaN(v) then invalidArg "value" "Nan is not supported in BigDecimal"
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
                elif leftShift < 0
                    then ( coeff <<< leftShift , 0 )
                else ( coeff, 0 )
            BigDecimal(coeff,expToUse,0u)
                                  


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



    // Using ROSC as input to consolidate versions for char arrays, strings, and similar.
    // Using a parsing model simlar to parser combinators, but much simpler because we have a simple linear model:
    //  +/- whole-part . fraction-part E +/- exponent
    // All parts are optional with the following constraints:
    //  (1) there must be a digit in whole-part + fraction-part
    //  (2) if there is an E, there must be a digit in exponent
  
    // my original plan was to pass ReadOnlySpan<char> (ROSC) objects around all over the place, 
    //   but F# has way too many restrictions.
    // My second plan was to pass just (offset, length) around and refer to the ROSC indirectly, 
    //   but that ended up closing over the ROSC, also forbidden
    // I could do a strictly sequential version such as I had in my C# code (before ROSCs existed),
    //   but I wanted to go functional.
    // So I'm going to eat the creation of a char array from the ROSC, 
    //  and still use (offset,length) tuples to define chunks without additional allocations.
    // not happy.
  
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
            let wholeDigitCount = data.wholePart.Length - (if isSign input.[data.wholePart.Start] then 1 else 0)
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












//module BigDecimal