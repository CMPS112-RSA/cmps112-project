rsa:
	ghc -XDeriveDataTypeable rsa-haskell.hs

keygen:
	ghc rsa-keygen-haskell.hs

test-keygen: clean keygen
	rsa-keygen-haskell private public
	cat public
	cat private

test-rsa: clean rsa test-keygen
	rsa-haskell -e -k public -f README.md -o encryptedTestFile.enc
	rsa-haskell -e -k private -f encryptedTestFile.enc -o encryptedTestFile.md
	cat encryptedTestFile.md

clean:
	rm *.exe *.hi *.o *.txt private public rsa-haskell rsa-keygen-haskell encryptedTestFile.*
