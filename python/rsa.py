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
        array.append((byte ** e) % n)
    return array

def decrypt(encrypted_bytes):
    array = []
    for byte in encrypted_bytes:
        array.append((byte ** d) % n)
    return array

def main():
    # global decrypt_opt
    # global encrypt_opt
    # global string
    # global key
    # global in_file
    # global out_file
    global n
    global e
    global d

    decrypt_opt = False
    encrypt_opt = False
    in_file = ""
    out_file = ""

    if not len(sys.argv[1:]):
        print ("Specify arguments")
        sys.exit(0)

    try:
        opts, args = getopt.getopt(sys.argv[1:], "dek:i:s:o:", ["decrypt", "encrypt", "key", "input", "string", "output"])

    except getopt.GetoptError as error:
        print(error)
        sys.exit(0)

    for opt, arg in opts:
        if opt in ("-d", "--decrypt"):
            decrypt_opt = True
        elif opt in ("-e", "--encrypt"):
            print('test')
            encrypt_opt = True
        elif opt in ("-k", "--key"):
            key = arg
        elif opt in ("-i", "--input"):
            in_file = arg
        elif opt in ("-s", "--string"):
            string = arg
        elif opt in ("-o", "--output"):
            out_file = arg
        else:
            assert False, "Incorrect Opt"


    # encrpyt
    # c = M^e mod n

    # decrpyt
    # M = c^d mod n

    n = 143
    e = 7
    d = 103

    if not in_file=="" and not out_file=="":
        if encrypt_opt:
            encrypted = bytearray(encrypt(read_into_buffer(in_file)))
            fw = open(os.getcwd()+"/"+out_file, 'w')
            fw.write(encrypted)
            fw.close()
        if decrypt_opt:
            decrypted  = bytearray(decrypt(read_into_buffer(in_file)))
            fw = open(os.getcwd()+"/"+out_file, 'w')
            fw.write(decrypted)
            fw.close()
main()


#
# bytes = read_into_buffer('/Users/ajanakos/cmps112/cmps112-project/python/input')
#
# print (bytearray(encrypt(bytes)))
#
# print (bytearray(decrypt(encrypt(bytes))))

