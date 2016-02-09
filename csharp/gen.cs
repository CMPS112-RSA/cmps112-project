using System;
using System.Security.Cryptography;
using IntXLib;
class gen {

  private static RNGCryptoServiceProvider rngCsp = new RNGCryptoServiceProvider();
  public static IntX genRand() {
    byte[] r = new byte[2];
    rngCsp.GetBytes(r);
    IntX output = 1;
    for(int i = 0; i < r.Length; i++) {
      output = output + (i + 1) * r[i];
    }
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

  public static IntX genRandPrime(IntX index) {
    IntX count = 0;
    IntX p = 0;
    while(count < index) {
      p = genRand();
      if(isPrime(p)) {
        count++;
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
    Console.WriteLine("Generating p...");
    output[0] = genRandPrime(1);
    Console.WriteLine("Generating q...");
    output[1] = genRandPrime(1);
    Console.WriteLine("Generating n...");
    output[2] = output[0] * output[1];

    return output;
  }

  public static IntX genE(IntX[] npq) {
    IntX totient = (npq[0] - 1) * (npq[1] - 1);

    IntX output = 0;
    Console.WriteLine("Generating e...");
    for(IntX e = 2; e < totient; e++) {
      if(gcd(e, totient) == 1) {
        output = e;
        break;
      }
    }
    return output;
  }

  public static IntX genD(IntX e, IntX totient) {
    IntX output = 0;
    Console.WriteLine("Generating d...");
    for(IntX d = 3; d < totient; d++) {
      if((d*e) % totient == 1) {
        output = d;
        break;
      }
    }
    return output;
  }

  public static void Main() {
    IntX[] npq = genN();
    Console.WriteLine(npq[2]);
    IntX e = genE(npq);
    Console.WriteLine(e);
    IntX d = genD(e, ((npq[0] -1) * (npq[1] - 1)));
    Console.WriteLine(d);
  }
}
