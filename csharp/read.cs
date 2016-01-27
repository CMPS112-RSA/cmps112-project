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

  public static IntX power(IntX a, IntX b) {
    IntX output = 1;
    for(IntX i = 0; i < b; i++) {
      output = output * a;
    }
    return output;
  }

  public static byte[] encrypt(byte[] message, IntX key_n, IntX key_e) {
    byte[] output = new byte [message.Length];
    for(int i = 0; i < message.Length; i++) {
      IntX powop = power(message[i], key_e);
      byte t = (byte) IntX.Modulo(powop, key_n, DivideMode.AutoNewton);
      output[i] = t;

    }
    return output;
  }

  public static byte[] decrypt(byte[] message, IntX key_d, IntX key_n) {
    byte[] output = new byte[message.Length];
    for(int i = 0; i < message.Length; i++) {
      IntX powop = power(message[i], key_d);
      byte t = (byte) IntX.Modulo(powop, key_n, DivideMode.AutoNewton);
      output[i] = t;
      //Console.WriteLine(output[i]);
    }
    return output;
  }

  public static void Main() {
      string[] args = Environment.GetCommandLineArgs();
      //Dictionary<string, string> paths = getopt(args);
      string filePath = args[1];

      byte[] file = readFile(filePath);
      byte[] encrypted = encrypt(file, 143, 7);

      string enc = Convert.ToBase64String(encrypted);
      Console.WriteLine(enc);
      Console.Write("--------------------------\n");

      byte[] dec = Convert.FromBase64String(enc);
      byte[] decrypted = decrypt(dec, 103, 143);

      for(int i = 0; i < decrypted.Length; i++) {
        Console.Write((char) decrypted[i]);
      }

      Console.WriteLine(dec);

      //string armored = Convert.ToBase64String(key);
      //Console.WriteLine(armored);

  }
}
