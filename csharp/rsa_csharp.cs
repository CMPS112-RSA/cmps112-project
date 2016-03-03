using System;
using System.IO;
using IntXLib;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

public class rsa_csharp {

  //This is the option parser.  Writing one was easier than using provided ones.
  //---------------------------------------------------------------------------
  public static readonly string[] options = {"-i", "-o", "-k"};
  public static readonly string[] s = {"-e", "-d"};
  public static byte[] readFile(string name) {
    return File.ReadAllBytes(name);
  }

  public static Dictionary <string, string> getopt(string[] args) {
    Dictionary<string, string> parsed = new Dictionary <string, string>();

    for(int opt = 0; opt < options.Length; opt++) {
      parsed.Add(options[opt], "");
    }

    for(int opt = 0; opt < s.Length; opt++) {
      parsed.Add(s[opt], "False");
    }

    for(int i = 0; i < args.Length - 1; i++) {
      for(int opt = 0; opt < s.Length; opt++) {
        if(args[i].Equals(s[opt])) {
          parsed[s[opt]] = "True";
        }
      }
    }

    for(int i = 0; i < args.Length - 1; i++) {
      for(int opt = 0; opt < options.Length; opt++) {
        if(args[i].Equals(options[opt]) && i <= args.Length-2) {
          parsed[options[opt]] = args[i+1];
        }
      }
    }
    return parsed;
  }
  //---------------------------------------------------------------------------


  //Fast exponentiation function.  Consulted Wikipedia.
  public static IntX power(IntX x, IntX n) {
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

  //Core encryption function
  public static IntX[] encrypt(byte[] message, IntX key_n, IntX key_e) {
    IntX[] output = new IntX [message.Length];
    for(uint i = 0; i < message.Length; i++) {
      IntX powop = power((IntX) message[i], key_e);
      IntX t = powop % key_n;
      output[i] = t;

    }
    return output;
  }

  //Core decryption function
  public static IntX[] decrypt(IntX[] message, IntX key_n, IntX key_d) {
    IntX[] output = new IntX[message.Length];
    Console.WriteLine("Decrypting message...");
    for(uint i = 0; i < message.Length; i++) {
      IntX powop = power(message[i], key_d);
      IntX t = (powop % key_n);
      //IntX t = fastPower(message[i], key_d, key_n);
      output[i] = t;
    }
    return output;
  }

  //Writes encrypted data to file.
  public static void writeToFile(IntX[] encrypted, string path, IntX key_n, IntX key_d ) {
    string[] output = new string[encrypted.Length+2];
    output[0] = key_n.ToString();
    output[1] = key_d.ToString();
    int count = 2;
    for(int i = 0; i < encrypted.Length; i++) {
      output[count] = encrypted[i].ToString();
      count++;
    }
    //System.IO.File.WriteAllText(path, key_n.ToString());
    //System.IO.File.WriteAllText(path, key_d.ToString());
    System.IO.File.WriteAllLines(path, output);
  }

  //Writes decrypted data to file.
  public static void writeDecToFile(string path, IntX[] file) {
    byte[] output = new byte[file.Length];
    for(int b = 0; b < output.Length; b++) {
      output[b] = (byte) file[b];
    }

    BinaryWriter Writer = new BinaryWriter(File.OpenWrite(path));
    Writer.Write(output);
    Writer.Flush();
    Writer.Close();

    //File.WriteAllBytes(path, output);
  }

  //Converts string to an IntX bigInteger
  public static IntX stringToInt(string str) {
    int power = 0;
    IntX sum = 0;
    for(int i = str.Length - 1; i >= 0; i--) {
      sum = sum + ((str[i] - '0') * (int) (Math.Pow(10, power)));
      power++;
    }
    return sum;
  }

  //Reads in a file and decrypts its contents.
  public static IntX[] decryptfromFile(string path, IntX key_e) {
    int lineCount = File.ReadLines(path).Count();
    IntX[] fromFile = new IntX[lineCount];
    int count = 0;

    foreach (string line in File.ReadLines(path)) {
      fromFile[count] = stringToInt(line);
      count++;
    }

    IntX key_n = fromFile[0];
    IntX[] output = new IntX[fromFile.Length - 2];
    int outindex = 0;
    for(int i = 2; i < fromFile.Length; i++) {
      output[outindex] = fromFile[i];
      outindex++;
    }



    IntX[] dec = decrypt(output, key_n, key_e);
    return dec;
  }

  //Reads a key from a file.
  public static IntX[] readKey(string path) {
    int lineCount = File.ReadLines(path).Count();
    IntX[] fromFile = new IntX[lineCount];
    int count = 0;

    foreach (string line in File.ReadLines(path)) {
      fromFile[count] = stringToInt(line);
      count++;
    }
    return fromFile;
  }

  public static void Main() {
      //Get and parse command line arguments.
      string[] args = Environment.GetCommandLineArgs();
      Dictionary<string, string> parsed = getopt(args);
      //Set input and output paths.
      string filePath = parsed["-i"];
      string outfile = parsed["-o"];

      //Read key.
      IntX[] key = readKey(parsed["-k"]);

      //Encrypt contents of input file
      if(parsed["-e"].Equals("True")){
        Console.WriteLine("Encrypting...");
        byte[] file = readFile(filePath);
        IntX[] encrypted = encrypt(file, key[0], key[1]);
        writeToFile(encrypted, outfile, key[0], key[1]);

      //Decrypt contents of input file
      }else if(parsed["-d"].Equals("True")) {
        Console.WriteLine("Decrypting...");
        IntX[] decfile = decryptfromFile(filePath, key[1]);
        writeDecToFile(outfile, decfile);
      }

  }
}
