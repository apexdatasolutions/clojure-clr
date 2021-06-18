using System;
using SNBI = System.Numerics.BigInteger;
using CLBI = clojure.lang.BigInteger;



namespace PrecisionTest
{
    class Program
    {
        static void Main(string[] args)
        {
            for (int i=1; i<50; i++)
            {
                String s= new('9', i);
                var snbi = SNBI.Parse(s);
                var clbi = CLBI.Parse(s);
                var cp = clbi.Precision;
                var sl = (UInt32)Math.Ceiling(SNBI.Log10(snbi));
                Console.WriteLine($"i={i}, Prec={cp}, Log={sl}, Match={cp==sl}");

            }

            for (int i = 1; i < 10; i++)
            {
                String s = "1" + new String('0', i);
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
            var log =  (uint)Math.Ceiling(SNBI.Log10(x));
            return log + (IsMultipleOfTen(x) ? 1u : 0u);
               
        }

    }
}
