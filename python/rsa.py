import sys
import getopt
import os.path


# Read each char and store as byte in array
def read_into_buffer(filename):
    buf = bytearray(os.path.getsize(filename))
    with open(filename, 'rb') as f:
         f.readinto(buf)
    return buf


# Perform arithmetic on byte to "encrypt"
def encrypt(byte_array):
    array = []
    for byte in byte_array:
        array.append(modExp(int(byte), e, n))
    return array


# Perform arithmetic on int value to decrypt
def decrypt(encrypted_bytes):
    array = []
    for byte in encrypted_bytes:
        #array.append((int(byte) ** d) % n)
        array.append(modExp(int(byte), d, n))
    return array


# Extract key values of file
def keyValues(file):
    fr = open(os.getcwd()+"/"+file, "r")
    values = fr.read().split("\n")
    for i, val in enumerate(values):
        try:
            values[i] = int(val)
        except:
            ValueError
    if len(values) > 2:
        values.pop()
    return values


# We define a function f(x,e,m) whose return value is x^e % m
# Code taken from http://www.math.umn.edu/~garrett/crypto/Code/FastPow_Python.html
def modExp(x,e,m):
    X = x
    E = e
    Y = 1
    while E > 0:
        if E % 2 == 0:
            X = (X * X) % m
            E = E/2
        else:
            Y = (X * Y) % m
            E = E - 1
    return Y


def usage():
    print("python rsa.py -e -d -i in -o out -k key")
    print("python rsa.py --encrypt --decrypt --input in --output out --key key")


# Allows user to encrypt and decrypt a file using a public or private key
def main():

    global n
    global e
    global d

    decrypt_opt = False
    encrypt_opt = False
    key = []
    in_file = ""
    out_file = ""

    if not len(sys.argv[1:]):
        usage()
        sys.exit(0)

    try:
        opts, args = getopt.getopt(sys.argv[1:], "dek:i:o:", ["decrypt", "encrypt", "key", "input", "output"])

    except getopt.GetoptError as error:
        print(error)
        sys.exit(0)

    # Check for flags
    for opt, arg in opts:
        if opt in ("-d", "--decrypt"):
            decrypt_opt = True
        elif opt in ("-e", "--encrypt"):
            encrypt_opt = True
        elif opt in ("-k", "--key"):
            key = arg
        elif opt in ("-i", "--input"):
            in_file = arg
        elif opt in ("-o", "--output"):
            out_file = arg
        else:
            assert False, usage()

    # Check for valid set of options
    if not in_file=="" and not out_file=="" and len(keyValues(key)) == 2 and encrypt_opt != decrypt_opt:
        if encrypt_opt:

            # Grab values from public key
            n = keyValues(key)[0]
            e = keyValues(key)[1]

            # Encrypt bute values
            encrypted = encrypt(read_into_buffer(in_file))

            # Open output file for writing
            fw = open(os.getcwd()+"/"+out_file, 'w')

            # Create encrypted file using n value and encrypted data
            fw.write(str(n)+"\n")
            for i, val in enumerate(encrypted):
                if (i == len(encrypted)-1):
                    fw.write(str(val))
                else:
                    fw.write(str(val)+"\n")
            fw.close()

        if decrypt_opt:

            # Grab values from public key
            n = keyValues(key)[0]
            d = keyValues(key)[1]

            # Read encrypted file and store values into an array
            vals = open(os.getcwd()+"/"+in_file).read().split("\n")

            # Decrypt vals
            decrypted  = bytearray(decrypt(vals))

            # Open output file for writing
            fw = open(os.getcwd()+"/"+out_file, 'w')
            fw.write("".join(map(chr, decrypted)))
            fw.close()
    else:
        usage()
        sys.exit(0)

main()