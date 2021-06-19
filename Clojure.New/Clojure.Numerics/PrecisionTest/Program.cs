using System;
using SNBI = System.Numerics.BigInteger;
using CLBI = clojure.lang.BigInteger;
using System.Net.Security;

namespace PrecisionTest
{
    class Program
    {
        static void Main(string[] args)
        {
            //CreateFromDouble(100.0);

            for (int i = 1; i < 50; i++)
            {
                String s = "-"+new String('9', i);
                var snbi = SNBI.Parse(s);
                var clbi = CLBI.Parse(s);
                var cp = clbi.Precision;
                var sl = (UInt32)Math.Ceiling(SNBI.Log10(snbi));
                var csl = ComputedPrecision(snbi);
                Console.WriteLine($"i={i}, Prec={cp}, Log= {sl}, CLog={csl}, Match={cp == csl}");

            }

            for (int i = 1; i < 10; i++)
            {
                String s = "-1" + new String('0', i);
                var snbi = SNBI.Parse(s);
                var clbi = CLBI.Parse(s);
                var cp = clbi.Precision;
                var sl = SNBI.Log10(snbi);
                var csl = ComputedPrecision(snbi);
                Console.WriteLine($"i={i}, Prec={cp}, Log= {sl}, CLog={csl}, Match={cp == csl}");

            }

        }


        static bool IsMultipleOfTen(SNBI x)
        {
            return x == 10 * (x / 10);
        }

        static uint ComputedPrecision(SNBI x)
        {
            if (x == SNBI.Zero)
                return 1;

            var log =  (uint)Math.Ceiling(SNBI.Log10(x< 0?-x:x));
            return log + (IsMultipleOfTen(x) ? 1u : 0u);
               
        }

        static bool CreateFromDouble(double d)
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


            return true;
        }

    }
}
