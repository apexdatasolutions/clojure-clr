using BenchmarkDotNet.Running;
using System;
using SNBI = System.Numerics.BigInteger;
using CLBI = clojure.lang.BigInteger;
using BenchmarkDotNet.Attributes;
using System.Globalization;
using Microsoft.Diagnostics.Runtime;
using Microsoft.Diagnostics.Tracing.Stacks;

namespace BigIntegerBenchmarks
{
    public class AdditionBM
    {
        private static readonly string digits = "999999999999999999999999999999999999999999";
        private static readonly SNBI largesnbi = SNBI.Parse(digits);
        private static readonly CLBI largeclbi = CLBI.Parse(digits);
        private const int numIters = 10;

        [Benchmark(Baseline = true)]
        public SNBI AddSNBI()
        {
            SNBI bi = largesnbi;
            for (int i = 0; i < numIters; i++)
                bi = System.Numerics.BigInteger.Add(bi, bi);
            return bi;
        }

        [Benchmark]
        public CLBI AddCLBI()
        {
            CLBI bi = largeclbi;
            for (int i = 0; i < numIters; i++)
                bi = clojure.lang.BigInteger.Add(bi, bi);
            return bi;
        }
    }

    public class MultiplicationBM
    {
        private static readonly string digits = "999999999999999999999999999999999999999999";
        private static readonly SNBI largesnbi = SNBI.Parse(digits);
        private static readonly CLBI largeclbi = CLBI.Parse(digits);
        private const int numIters = 10;

        [Benchmark(Baseline = true)]
        public SNBI MulSNBI()
        {
            SNBI bi = largesnbi;
            for (int i = 0; i < numIters; i++)
                bi = System.Numerics.BigInteger.Add(bi, bi);
            return bi;
        }

        [Benchmark]
        public CLBI MulCLBI()
        {
            CLBI bi = largeclbi;
            for (int i = 0; i < numIters; i++)
                bi = clojure.lang.BigInteger.Add(bi, bi);
            return bi;
        }
    }

    public class PowBM
    {
        private static readonly string digits = "999999999999999999999999999999999999999999";
        private static readonly SNBI largesnbi = SNBI.Parse(digits);
        private static readonly CLBI largeclbi = CLBI.Parse(digits);
        private const int numIters = 10;

        [Benchmark(Baseline = true)]
        public SNBI PowSNBI()
        {
            return System.Numerics.BigInteger.Pow(largesnbi, 5);
        }

        [Benchmark]
        public CLBI PowCLBI()
        {
            return clojure.lang.BigInteger.Power(largeclbi, 5);
        }
    }


    public class DivModBM
    {
        private static readonly string digits = "999999999999999999999999999999999999999999";
        private static readonly SNBI largesnbi = SNBI.Parse(digits);
        private static readonly CLBI largeclbi = CLBI.Parse(digits);
        private const int numIters = 10;
        private static readonly SNBI snbiDivisor = (SNBI)10;
        private static readonly CLBI clbiDivisor = (CLBI)10;

        [Benchmark(Baseline = true)]
        public SNBI DivModSNBI()
        {
            SNBI bi = largesnbi;
            for (int i = 0; i < numIters; i++)
                bi = System.Numerics.BigInteger.DivRem(bi, snbiDivisor, out SNBI rem);
            return bi;
        }

        [Benchmark]
        public CLBI DivModCLBI()
        {
            CLBI bi = largeclbi;
            for (int i = 0; i < numIters; i++)
                bi = clojure.lang.BigInteger.DivRem(bi, clbiDivisor, out CLBI rem);
            return bi;
        }

    }



    public class DivideBM
    {
        private static readonly string digits = "999999999999999999999999999999999999999999";
        private static readonly SNBI largesnbi = SNBI.Parse(digits);
        private static readonly CLBI largeclbi = CLBI.Parse(digits);
        private const int numIters = 10;
        private static readonly SNBI snbiDivisor = (SNBI)10;
        private static readonly CLBI clbiDivisor = (CLBI)10;

        [Benchmark(Baseline = true)]
        public SNBI DivModSNBI()
        {
            SNBI bi = largesnbi;
           
            for (int i = 0; i < numIters; i++)
                bi = System.Numerics.BigInteger.Divide(bi, snbiDivisor);
            return bi;
        }

        [Benchmark]
        public CLBI DivModCLBI()
        {
            CLBI bi = largeclbi;
            for (int i = 0; i < numIters; i++)
                bi = clojure.lang.BigInteger.Divide(bi, clbiDivisor);
            return bi;
        }

    }



    public class ParseBM
    {
        private static readonly string digits = "999999999999999999999999999999999999999999";
        private const int numIters = 10;
        private static readonly string[] inputs = new string[numIters];

        static ParseBM()
        {
            string s = digits;
            for ( int i=0; i<numIters; i++)
            {
                inputs[i] = s;
                s += digits;
            }
        }

        [Benchmark(Baseline = true)]
        public SNBI ParseSNBI()
        {
            SNBI bi = SNBI.Parse("0");
            foreach (string s in inputs)
                bi = SNBI.Parse(s);
            return bi;
        }

        [Benchmark]
        public CLBI ParseCLBI()
        {
            CLBI bi = CLBI.Parse("0");
            foreach (string s in inputs)
                bi = CLBI.Parse(s);
            return bi;
        }
    }



    public class ToStringBM
    {
        private static readonly string digits = "999999999999999999999999999999999999999999";
        private const int numIters = 10;
        private static readonly string[] inputs = new string[numIters];
        private static readonly SNBI largesnbi = SNBI.Parse(digits);
        private static readonly CLBI largeclbi = CLBI.Parse(digits);

        [Benchmark(Baseline = true)]
        public string ToStringSNBI()
        {
            return largesnbi.ToString();
        }

        [Benchmark]
        public string ToStringCLBI()
        {
            return largeclbi.ToString();
        }
    }

    public class RoundTripBM
    {
        private static readonly string digits = "999999999999999999999999999999999999999999";

        [Benchmark(Baseline = true)]
        public bool RoundTripSNBI()
        {
            bool b = SNBI.Parse(digits).ToString().Equals(digits);
            if (!b) throw new Exception("RoundTripSNBI");
            return b;
        }

        [Benchmark]
        public bool RoundTripCLBI()
        {
            bool b = CLBI.Parse(digits).ToString().Equals(digits);
            if (!b) throw new Exception("RoundTripCLBI");
            return b;
        }
    }




    [HtmlExporter, MarkdownExporter, CsvExporter]
    class Program
    {
        static void Main(string[] args) => BenchmarkSwitcher.FromAssembly(typeof(Program).Assembly).Run(args);
    }

}