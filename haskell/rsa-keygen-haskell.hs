-- Ryan Coley
-- rsa-keygen-haskell.hs

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

gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
            in (t, s - q * t, g)

modInv :: Integer -> Integer -> Integer
modInv a m = let (i, _, g) = gcdExt a m in
             if g == 1 then (mkPos i) else -1
             where mkPos x = if x < 0 then x + m else x

genD :: Integer -> Integer -> Integer
genD totient public = modInv public totient

--seed :: IO StdGen
--seed = getStdGen

main = do
         args <- getArgs
         seed <- newStdGen
         let p = genPrime seed
         seed <- newStdGen
         let q = genPrime seed
         print "Generating N..."
         let n = p * q
         writeFile (args!!0) (show n)
         appendFile (args!!0) "\n"
         writeFile (args!!1) (show n)
         appendFile (args!!1) "\n"
         let t = (p-1) * (q-1)
         print "Generating E..."
         let e = genE t
         appendFile (args!!0) (show e)
         print "Generating D..."
         let d = genD t e
         appendFile (args!!1) (show d)
