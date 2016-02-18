A simple implementation of the RSA algorithm using Haskell.

rsa-keygen-haskell generates a valid public and private RSA key.

rsa-haskell can encrypt and decrpt files using those keys or other keys provided.
### To run file:
    rsa-keygen-haskell [public] [private]
    rsa-haskell [-ed encrypt/decrypt] [-k key] [-f file] [-o output]
### To compile:
    rsa-keygen-haskell make keygen
    rsa-haskell make rosettacode
### To remove all files generated:
    make clean
### To recompile and test:
    rsa-keygen-haskell make test-keygen
    rsa-haskell make test-rsa
