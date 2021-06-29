using System;
using SNBI = System.Numerics.BigInteger;
using CLBI = clojure.lang.BigInteger;
using CLBD = clojure.lang.BigDecimal;
using System.Threading;

namespace PrecisionTest
{
    class Program
    {
        static void Main(string[] args)
        {
            //CreateFromDouble(100.0);

            //for (int i = 5; i < 10; i++)
            //{
            //    String s = new String('9', i);
            //    var snbi = SNBI.Parse(s);
            //    var clbi = CLBI.Parse(s);
            //    var cp = clbi.Precision;
            //    var sl = (UInt32)Math.Ceiling(SNBI.Log10(snbi));
            //    var csl = ComputedPrecision(snbi);
            //    var sLen = snbi.ToString().Length;
            //    Console.WriteLine($"i={i}, Prec={cp}, Log= {sl}, CLog={csl}, Slen={sLen},m Match={cp == csl}");

            //}

            //for (int i = 5; i < 10; i++)
            //{
            //    String s = "1" + new String('0', i);
            //    var snbi = SNBI.Parse(s);
            //    var clbi = CLBI.Parse(s);
            //    var cp = clbi.Precision;
            //    var sl = SNBI.Log10(snbi);
            //    var csl = ComputedPrecision(snbi);
            //    var sLen = snbi.ToString().Length;
            //    Console.WriteLine($"i={i}, Prec={cp}, Log= {sl}, CLog={csl}, Slen={sLen},m Match={cp == csl}");

            //}
            //{

            //    var s = "123456789099999989001469561517021489294165803585201501846313476562500";
            //    var snbi = SNBI.Parse(s);
            //    var clbi = CLBI.Parse(s);
            //    var cp = clbi.Precision;
            //    var sl = SNBI.Log10(snbi);
            //    var csl = ComputedPrecision(snbi);
            //    var sLen = snbi.ToString().Length;
            //    Console.WriteLine($"Prec={cp}, Log= {sl}, CLog={csl}, Slen={sLen},m Match={cp == csl}");



            //}

            //{
            //    var s = "100000000000000000000000000000000000";
            //    var snbi = SNBI.Parse(s);
            //    var clbi = CLBI.Parse(s);
            //    var snbiBits = snbi.ToByteArray();
            //    var clbiBits = clbi.GetMagnitude();
            //    Console.WriteLine($"{snbiBits} {clbiBits}");
            //}


            //(0.1234567891, "0.123456789", c9hu);
            //(0.01234567891, "0.0123456789", c9hu);
            //(0.001234567891, "0.00123456789", c9hu);
            //(0.0001234567891, "0.000123456789", c9hu);
            //(0.00001234567891, "0.0000123456789", c9hu);
            //(0.000001234567891, "0.00000123456789", c9hu);
            //(0.0000001234567891, "1.23456789E-7", c9hu);
            //(0.00000001234567891, "1.23456789E-8", c9hu);
            //(0.000000001234567891, "1.23456789E-9", c9hu);
            //(0.0000000001234567891, "1.23456789E-10", c9hu);
            //(0.00000000001234567891, "1.23456789E-11", c9hu);
            //(0.000000000001234567891, "1.23456789E-12", c9hu);
            //(0.0000000000001234567891, "1.23456789E-13", c9hu);

            //CreateFromDouble(0.1234567891);
            //CreateFromDouble(0.0000001234567891);


            var bd = CLBD.Parse("1");
            //var c = new clojure.lang.BigDecimal.Context(9, CLBD.RoundingMode.HalfUp);
            var c = new clojure.lang.BigDecimal.Context(0, CLBD.RoundingMode.HalfUp);
            var q = bd.Divide(bd, c);

            var s = q.ToScientificString();
            Console.WriteLine(s);

        }


        static bool IsMultipleOfTen(SNBI x)
        {
            return x == 10 * (x / 10);
        }

        static uint ComputedPrecision(SNBI x)
        {
            if (x == SNBI.Zero)
                return 1;

            var log =  (uint)Math.Floor(SNBI.Log10(x< 0?-x:x))+1;
            return log;
        }

        static void CreateFromDouble(double d)
        {
            var cnctxt = new Clojure.Numerics.Context(9, Clojure.Numerics.RoundingMode.HalfUp);
            var clctxt = new clojure.lang.BigDecimal.Context(9, clojure.lang.BigDecimal.RoundingMode.HalfUp);




            var cnd = Clojure.Numerics.BigDecimal.CreateC(d,cnctxt);
            var cld = clojure.lang.BigDecimal.Create(d,clctxt);

            var cndCoeff = cnd.Coefficient;
            var cldCoeff = cld.Coefficient;

            var cndExp = cnd.Exponent;
            var cldExp = cld.Exponent;

            var cndPrec = cnd.Precision;
            var cldPrec = cld.GetPrecision();

            Console.WriteLine($"old = {cldCoeff} {cldExp} {cldPrec} {cld.ToString()}");
            Console.WriteLine($"new = {cndCoeff} {cndExp} {cndPrec} {cnd.ToString()}");

        }

        static void PlayTime()
        {

            var b = SNBI.Parse("123");
            var b1 = SNBI.Parse("234");
            var x = b < b1;
            var p = b1 / b;
            var p1 = b1 << 2;
        }



    }
}
