all: rsa keygen

rsa:
	ghc -O2 -XDeriveDataTypeable rsa-haskell.hs -rtsopts

keygen:
	ghc -O2 rsa-keygen-haskell.hs -rtsopts

test-keygen: keygen
	./rsa-keygen-haskell private public
	cat public
	cat private

test-rsa: rsa test-keygen
	./rsa-haskell encrypt -k public -i README.md -o encryptedTestFile.enc
	./rsa-haskell decrypt -k private -i encryptedTestFile.enc -o encryptedTestFile.md
	cat encryptedTestFile.md

clean:
	rm *.exe *.hi *.o *.txt private public rsa-haskell rsa-keygen-haskell encryptedTestFile.*
