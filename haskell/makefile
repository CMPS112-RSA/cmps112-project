rsa:
	ghc rsa-haskell.hs

keygen:
	ghc rsa-keygen-haskell.hs

test-keygen: clean keygen
	rsa-keygen-haskell.exe public private

clean:
	rm *.exe *.hi *.o *.txt private public
