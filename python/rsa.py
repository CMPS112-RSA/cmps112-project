import sys
import getopt
import os.path


# def main():
#     global decrypt
#     global encrypt
#     global string
#     global key
#     global in_file
#     global out_file
#
#     if not len(sys.argv[1:]):
#         print "Specify arguments"
#         sys.exit(0)
#
#     try:
#         opts, args = getopt.getopt(sys.argv[1:], "dek:f:s:o:", ["decrypt", "encrypt", "key", "file", "string", "output"])
#
#     except getopt.GetoptError as error:
#         print str(error)
#         sys.exit(0)
#
#     for opt, arg in opts:
#         if opt in ("-d", "--decrypt"):
#             decrypt = True
#         elif opt in ("-e", "--encrypt"):
#             encrypt = True
#         elif opt in ("-k", "--key"):
#             key = arg
#         elif opt in ("-f", "--file"):
#             in_file = open(arg, 'r')
#         elif opt in ("-s", "--string"):
#             string = arg
#         elif opt in ("-o", "--output"):
#             out_file = arg
#         else:
#             assert False, "Incorrect Opt"
#
# main()



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
        array.append((byte ** 103) % n)
    return array

# encrpyt
# c = M^e mod n

# decrpyt
# M = c^d mod n

n = 143
e = 7
d = 103

bytes = read_into_buffer('/Users/ajanakos/PycharmProjects/RSA/input')

print (bytearray(encrypt(bytes)))

print (bytearray(decrypt(encrypt(bytes))))

