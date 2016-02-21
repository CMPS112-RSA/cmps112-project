-- Ryan Coley
-- rsa-keygen-haskell.hs
-- Generates RSA keys
-- rsa-keygen-haskell [private] [public]

import System.Random
import Data.Word
import System.Environment

divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = 1:[ y | y <- [3,5..(div x 2)], mod x y == 0] ++ [x]

isPrime :: Integer -> Bool
isPrime x | mod x 2 == 0 = False
          | otherwise = divisors x == [1,x]

genRandom :: StdGen -> Int
genRandom seed = (read (show prime) :: Int) where prime = (randoms seed :: [Word16])!!0

primeNums :: [Integer]
primeNums = [ x | x <- [3..1000000], isPrime x]

genPrime :: StdGen -> Integer
genPrime seed = primeNums!!select where select = genRandom seed

genE :: Integer -> Integer
genE totient = [ e | e <- [2..totient], (gcd totient e) == 1]!!0


{- Got this code from http://rosettacode.org/wiki/Modular_inverse#Haskell and modified it to fit my needs -}
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
            in (t, s - q * t, g)

modInv :: Integer -> Integer -> Integer
modInv a m = let (i, _, g) = gcdExt a m in
             if g == 1 then (mkPos i) else -1
             where mkPos x = if x < 0 then x + m else x
{- End -}

genD :: Integer -> Integer -> Integer
genD totient public = modInv public totient

main = do
         args <- getArgs
         if (length args) /= 2 then error "USAGE: rsa-keygen-haskell private public" else putStrLn "Making the Key..."
         seed <- newStdGen
         let p = genPrime seed
         seed <- newStdGen
         let q = genPrime seed
         putStrLn "Generating N..."
         let n = p * q
         writeFile (args!!0) (show n)
         appendFile (args!!0) "\n"
         writeFile (args!!1) (show n)
         appendFile (args!!1) "\n"
         let t = (p-1) * (q-1)
         putStrLn "Generating E..."
         let e = genE t
         appendFile (args!!1) (show e)
         putStrLn "Generating D..."
         let d = genD t e
         appendFile (args!!0) (show d)
         putStrLn "Finished making the Key..."
