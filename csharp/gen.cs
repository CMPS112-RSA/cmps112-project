using System;
using System.Security.Cryptography;
using IntXLib;
class gen {

  private static RNGCryptoServiceProvider rngCsp = new RNGCryptoServiceProvider();
  public static ulong getBin() {
    byte[] r = new byte[1];
    rngCsp.GetBytes(r);

    if(r[0] % 2 == 0) {
      return 1;
    }else {
      return 0;
    }
  }

  public static IntX genRand() {
    int size = 8;
    IntX output = 0;
    for(int i = 0; i < size; i++) {
      ulong bit = (getBin() * (ulong) Math.Pow(2, i));
      output = output + (IntX) bit;
    }
    Console.WriteLine(output);
    return output;
  }

  public static bool isPrime(IntX num) {
    if(num % 2 == 0) {
      return false;
    }
    for(IntX i = num - 1; i > 2; i--) {
      if(num % i == 0) {
        return false;
      }
    }
    return true;
}

public static bool isPrime_new(IntX num) {
  if(num < 2) {
    return false;
  }
  if(num == 2) {
    return false;
  }
  if(num % 2 == 0) {
    return false;
  }
  for(ulong i = 3; i <= Math.Sqrt( (ulong) num); i += 2) {
    if(num % i == 0) {
      return false;
    }
  }
  return true;
}

  public static IntX genRandPrime(IntX index) {
      IntX p = 0;
      while(true) {
        p = genRand();
        Console.WriteLine(p);
        if(isPrime(p)) {
          break;
        }
      }
    return p;
  }

  public static IntX gcd(IntX a, IntX b) {
    if(b == 0) {
      return a;
    }else {
      return gcd(b, a % b);
    }
  }

  public static IntX[] genN() {
    IntX[] output = new IntX[3];
    //Console.WriteLine("Generating p...");
    output[0] = genRandPrime(1);
    //Console.WriteLine("Generating q...");
    output[1] = genRandPrime(1);
    //Console.WriteLine("Generating n...");
    output[2] = output[0] * output[1];

    return output;
  }

  public static IntX genE(IntX[] npq) {
    IntX totient = (npq[0] - 1) * (npq[1] - 1);

    IntX output = 0;
    //Console.WriteLine("Generating e...");
    for(IntX e = 2; e < totient; e++) {
      if(gcd(e, totient) == 1) {
        output = e;
        break;
      }
    }
    return output;
  }

  public static IntX[] Extended_GCD(IntX a, IntX b)
  {
      IntX[] result = new IntX[3];
      if (a < b) //if a less than b, switch them
      {
          IntX temp = a;
          a = b;
          b = temp;
      }
      IntX r = b;
      IntX q = 0;
      IntX x0 = 1;
      IntX y0 = 0;
      IntX x1 = 0;
      IntX y1 = 1;
      IntX x = 0, y = 0;
      while (r > 1)
      {
          r = a % b;
          q = a / b;
          x = x0 - q * x1;
          y = y0 - q * y1;
          x0 = x1;
          y0 = y1;
          x1 = x;
          y1 = y;
          a = b;
          b = r;
      }
      result[0] = r;
      result[1] = x;
      result[2] = y;
      return result;
  }

  public static IntX modInverse(IntX a, IntX m)
  {
    IntX[] g = Extended_GCD(a, m);
    if (g.Length == 0)
        return -1;
    else
    {
        // m is added to handle negative x
        IntX res = (g[1] % m + m) % m;
        return res;
    }
}


  public static IntX genD(IntX e, IntX totient) {
    IntX output = 0;
    //Console.WriteLine("Generating d...");
    for(IntX d = 3; d < totient; d++) {
      if((d*e) % totient == 1) {
        output = d;
        break;
      }
    }
    return output;
  }

  public static void Main() {
    IntX dp = 0;
    while(dp < 32) {
      IntX[] npq = genN();
      IntX e = genE(npq);
      //Console.WriteLine("Generating d...");
      IntX d = modInverse(e, ((npq[0] -1) * (npq[1] - 1)));
      dp = d;
      Console.Write("n=");
      Console.Write(npq[2]);
      Console.Write(", e=");
      Console.Write(e);
      Console.Write(", d=");
      Console.Write(d);
      Console.WriteLine("");
    }
  }
}
