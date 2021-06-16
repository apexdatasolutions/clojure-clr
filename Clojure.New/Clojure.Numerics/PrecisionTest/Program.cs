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
                var sl = Math.Ceiling(SNBI.Log10(snbi));
                Console.WriteLine($"i={i}, Prec={cp}, Log={sl}, Match={cp==sl}");

            }
        }
    }
}
