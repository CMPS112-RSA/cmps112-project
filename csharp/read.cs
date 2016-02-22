using System;
using System.IO;
using IntXLib;
using System.Collections.Generic;

public class read {
  public static readonly string[] options = {"-f", "-o", "-k"};
  public static byte[] readFile(string name) {
    return File.ReadAllBytes(name);
  }

  public static Dictionary <string, string> getopt(string[] args) {
    Dictionary<string, string> parsed = new Dictionary <string, string>();
    for(int i = 0; i < args.Length - 1; i++) {
      for(int opt = 0; i < options.Length; i++) {
        if(args[i].Equals(options[opt])) {
          parsed.Add(options[opt], args[i+1]);
        }
      }
    }
    return parsed;
  }

  public static IntX power(IntX x, IntX n) {
    //IntX i, output = 1;
    //for(i = 0; i < b; i++) {
      //output *= a;
      //output = IntX.Multiply(output, a, MultiplyMode.AutoFht);
    //}
    //return output;
    if(n < 0) {
      x = 1 / x;
      n = n * -1;
    }
    if(n == 0) {
      return 1;
    }
    IntX y = 1;
    while(n > 1) {
      if((n%2) == 0) {
        x = x * x;
        n = n / 2;
      }else {
        y = x * y;
        x = x * x;
        n = (n-1) / 2;
      }
    }
    return x * y;
  }

  public static IntX[] encrypt(byte[] message, IntX key_n, IntX key_e) {
    IntX[] output = new IntX [message.Length];
    for(uint i = 0; i < message.Length; i++) {
      IntX powop = power((IntX) message[i], key_e);
      IntX t = powop % key_n;
      //Console.WriteLine(t);
      output[i] = t;

    }
    return output;
  }

  public static IntX[] decrypt(IntX[] message, IntX key_n, IntX key_d) {
    IntX[] output = new IntX[message.Length];
    Console.WriteLine("Decrypting message...");
    for(uint i = 0; i < message.Length; i++) {
      IntX powop = power(message[i], key_d);
      IntX t = (powop % key_n);
      output[i] = t;
    }
    return output;
  }

  public static void Main() {
      string[] args = Environment.GetCommandLineArgs();
      string filePath = args[1];

      byte[] file = readFile(filePath);
      IntX[] encrypted = encrypt(file, Int64.Parse(args[2]), Int64.Parse(args[3]));
      Console.Write("--------------------------\n");

      //byte[] dec = Convert.FromBase64String(enc);
      IntX[] decrypted = decrypt(encrypted, Int64.Parse(args[2]), Int64.Parse(args[4]));

      for(int i = 0; i < decrypted.Length; i++) {
        Console.Write((char) decrypted[i]);
      }

  }
}
