-- Ryan Coley
-- rsa-haskell.hs
-- Encrypts and Decrypts files using RSA
-- rsa-haskell [encrypt/decrypt] [-k key] [-i input] [-o output]

--imports
import qualified Data.Binary as B
import qualified System.IO as SIO
import qualified Data.List.Split as DLS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Control.Monad
import Data.ByteString.Char8
import System.Console.CmdArgs

--read process the key
readKey :: String -> (Integer,Integer)
readKey key = (keyN,keyPow) where keyN = stringToInteger splitted!!0
                                  keyPow = stringToInteger splitted!!1
                                  splitted = DLS.splitOn "\n" key

{-Borrowed code http://rosettacode.org/wiki/Modular_exponentiation#Haskell-}
{-quickly a^b mod n without overflow issues-}
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r
{-End-}

-- Command Line Argument Parsing
data RSA = Encrypt {key :: String, input :: String, output :: String}
         | Decrypt {key :: String, input :: String, output :: String}
           deriving (Show, Data, Typeable)

encrypt :: RSA
encrypt = Encrypt {
   key = def &= typFile &= help "RSA Public Key",
   input = def &= typFile &= help "File to encrypt",
   output = def &= typFile &= help "Output encrypted file"
} &= help "Encrypt Files"

decrypt :: RSA
decrypt = Decrypt {
   key = def &= typFile &= help "RSA Private Key",
   input = def &= typFile &= help "File to decrypt",
   output = def &= typFile &= help "Output decrypted file"
} &= help "Decrypt Files"

--process command
optionHandler :: RSA -> IO ()
optionHandler opts@Encrypt{key = key, input = input, output = output}  = do
   --command line error checking
   when (key == "") $ error "Key must be specified"
   when (input == "") $ error "File must be specified"
   when (output == "") $ error "Output must be specified"

   --get the key
   keyFile <- SIO.readFile key
   let (keyN,keyE) = readKey keyFile

   --get the input file
   inputFile <- BL.readFile input

   --encrypt
   Prelude.mapM (\x -> Prelude.appendFile output ((show (mod ((toInteger x)^keyE) keyN) ++ "\n"))) (BL.unpack inputFile)

   SIO.putStrLn "Encryption Finished"

--process command
optionHandler opts@Decrypt{key = key, input = input, output = output}  = do
   --command line error checking
   when (key == "") $ error "Key must be specified"
   when (input == "") $ error "File must be specified"
   when (output == "") $ error "Output must be specified"

   --get the key
   keyFile <- SIO.readFile key
   let (keyN, keyD) = readKey keyFile

   --get the input file
   inputFile <- SIO.readFile input
   let splitted = (Prelude.init (DLS.splitOn "\n" inputFile))

   --decrypt
   Prelude.mapM (\x -> BL.appendFile output (BL.singleton (read (show (powm (read x :: Integer) keyD keyN 1)) :: B.Word8))) splitted

   SIO.putStrLn "Decryption Finished"

--process the command line args and perform functions
main = do
         opts <- cmdArgs ( modes [encrypt,decrypt]
                                 &= help "Basic RSA implementation"
                                 &= summary "rsa-haskell v0.1")
         optionHandler opts
