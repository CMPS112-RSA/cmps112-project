import sys
import getopt
import os.path


def read_into_buffer(filename):
    buf = bytearray(os.path.getsize(filename))
    with open(filename, 'rb') as f:
         f.readinto(buf)
    return buf


def encrypt(byte_array):
    array = []
    for byte in byte_array:
        array.append((int(byte) ** e) % n)
    return array


def decrypt(encrypted_bytes):
    array = []
    for byte in encrypted_bytes:
        array.append((int(byte) ** d) % n)
    return array


def keyValues(file):
    fr = open(os.getcwd()+"/"+file, "r")
    values = fr.read().split("\n")
    for i, val in enumerate(values):
        values[i] = int(val)
    return values


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
        print ("Specify arguments")
        sys.exit(0)

    try:
        opts, args = getopt.getopt(sys.argv[1:], "dek:i:o:", ["decrypt", "encrypt", "key", "input", "output"])

    except getopt.GetoptError as error:
        print(error)
        sys.exit(0)

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
            assert False, "Incorrect Opt"



    # n = 143
    # e = 7    # pub
    # d = 103  # priv



    print(in_file)
    print(out_file)
    print(keyValues(key))
    print(len(keyValues(key)))
    print(encrypt_opt)
    print(decrypt_opt)
    if not in_file=="" and not out_file=="" and len(keyValues(key)) == 2 and encrypt_opt != decrypt_opt:
        if encrypt_opt:

            n = keyValues(key)[0]
            e = keyValues(key)[1]

            encrypted = encrypt(read_into_buffer(in_file))
            fw = open(os.getcwd()+"/"+out_file, 'w')

            fw.write(str(n)+"\n")
            for i, val in enumerate(encrypted):
                if (i == len(encrypted)-1):
                    fw.write(str(val))
                else:
                    fw.write(str(val)+"\n")



            fw.close()

        if decrypt_opt:

            n = keyValues(key)[0]
            d = keyValues(key)[1]

            vals = open(os.getcwd()+"/"+in_file).read().split("\n")
            print(vals)
            decrypted  = bytearray(decrypt(vals))
            fw = open(os.getcwd()+"/"+out_file, 'w')
            # for line in decrypted:
            #     fw.write(str(line)+" - ")
            fw.write("".join(map(chr, decrypted)))
            fw.close()
    else:
        print("Invalid Set of arguments")
        sys.exit(0)

main()


#
# bytes = read_into_buffer('/Users/ajanakos/cmps112/cmps112-project/python/input')
#
# print (bytearray(encrypt(bytes)))
#
# print (bytearray(decrypt(encrypt(bytes))))

