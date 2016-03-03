#!/bin/bash

print_header() {
  echo "++++++++++++++++++++++++++++++++++"
  echo $1
  echo "++++++++++++++++++++++++++++++++++"
}


encrypt_driver() {
  #INPUT: language, binary_path
  print_header $1
  LANG=$1
  FLAGS=" -e -i gencpp.ml -o test."$LANG".encrypt -k csharp/key.pub"
  time $2 $FLAGS
}

decrypt_driver() {
  #INPUT: language, binary_path
  print_header $1
  LANG=$1
  FLAGS=" -d -i test."$LANG".encrypt -o test."$LANG".decrypt -k csharp/key.priv"
  time $2 $FLAGS
}

keygen_driver() {
  print_header $1
  LANG=$1
  time $2 key.priv.$LANG key.pub.$LANG
}

print_header "Encryption tests"

encrypt_driver "csharp" "mono csharp/rsa_csharp.exe"
encrypt_driver "c" "./c/build/msg-crypt/rsa-c"
print_header "Haskell"
time ./haskell/rsa-haskell encrypt -i gencpp.ml -o test.haskell.encrypt -k csharp/key.pub
print_header "Python"
cd python && time python3 rsa.py -e -i gencpp.ml -o test.python.encrypt -k key.pub
cd ..


print_header "Decryption tests"
decrypt_driver "csharp" "mono csharp/rsa_csharp.exe"
decrypt_driver "c" "./c/build/msg-crypt/rsa-c"
print_header "Haskell"
time ./haskell/rsa-haskell decrypt -i test.haskell.encrypt -o test.haskell.decrypt -k csharp/key.priv
print_header "Python"
cd python && time python3 rsa.py -d -i test.python.encrypt -o test.python.decrypt -k key.priv
cd ..

print_header "Key generation tests - 32 bit"
keygen_driver "csharp" "mono csharp/keygen_csharp.exe"
keygen_driver "c" "c/build/keygen/rsa-keygen-c"
keygen_driver "Haskell" "haskell/rsa-keygen-haskell"
keygen_driver "Python" "python3 python/keygen.py"

#time  mono csharp/rsa_csharp.exe -d -i test.csharp.encrypt -o test.csharp.decrypt -k csharp/key.priv
