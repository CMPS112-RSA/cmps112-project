-- Ryan Coley
-- rsa-haskell.hs
--Encrypts and Decrypts files using RSA
--rsa-haskell [encrypt/decrypt] [-k key] [-f file] [-o output]

--imports
import qualified System.IO as SIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as B
import Data.Char
import System.Environment
import Data.ByteString.Char8
import qualified Data.List.Split as DLS
import System.Console.CmdArgs
import Control.Monad

--type casting functions
{-convertWord8ToInteger :: [B.Word8] -> [Integer]
convertWord8ToInteger char = Prelude.map (\x -> (read (show x) :: Integer)) char-}

convertIntegerToWord8 :: [Integer] -> [B.Word8]
convertIntegerToWord8 int = Prelude.map (\x -> (read (show x) :: B.Word8)) int

integerToString :: [Integer] -> [String]
integerToString x = Prelude.map (\y -> show y) x

stringToInteger :: [String] -> [Integer]
stringToInteger x = Prelude.map (\y -> read y :: Integer) x

--encrypting functions
{-encryptCharacter :: Integer -> Integer -> Integer -> Integer
encryptCharacter char keyN keyE = mod powop keyN where powop = char ^ keyE

encryptMsg :: [B.Word8] -> Integer -> Integer -> [Integer]
encryptMsg msg keyN keyE = Prelude.map (\x -> encryptCharacter x keyN keyE) (convertWord8ToInteger msg)-}

decryptCharacter :: Integer -> Integer -> Integer -> Integer
decryptCharacter char keyD keyN = mod powop keyN where powop = char ^ keyD

decryptMsg :: [Integer] -> Integer -> Integer -> [B.Word8]
decryptMsg msg keyD keyN = convertIntegerToWord8 (Prelude.map (\x -> decryptCharacter x keyD keyN) msg)

--other functions
readKey :: String -> (Integer,Integer)
readKey key = (keyN,keyPow) where keyN = stringToInteger splitted!!0
                                  keyPow = stringToInteger splitted!!1
                                  splitted = DLS.splitOn "\n" $ key

-- Command Line Argument Parsing
data RSA = Encrypt {key :: String, file :: String, output :: String}
         | Decrypt {key :: String, file :: String, output :: String}
           deriving (Show, Data, Typeable)

encrypt :: RSA
encrypt = Encrypt {
    key = def &= typFile &= help "RSA Public Key",
    file = def &= typFile &= help "File to encrypt",
    output = def &= typFile &= help "Output encrypted file"
} &= help "Encrypt Files"

decrypt :: RSA
decrypt = Decrypt {
    key = def &= typFile &= help "RSA Private Key",
    file = def &= typFile &= help "File to decrypt",
    output = def &= typFile &= help "Output decrypted file"
} &= help "Decrypt Files"

optionHandler :: RSA -> IO ()
optionHandler opts@Encrypt{key = key, file = file, output=output}  = do
   when (key == "") $ error "Key must be specified"
   when (file == "") $ error "File must be specified"
   when (output == "") $ error "Output must be specified"
   keyFile <- SIO.readFile key
   let (keyN,keyE) = readKey keyFile
   contents <- BL.readFile file
   Prelude.mapM (\x -> Prelude.appendFile output ((show (mod ((toInteger x)^keyE) keyN) ++ "\n"))) (BL.unpack contents)
   SIO.putStrLn "Encryption Finished"

optionHandler opts@Decrypt{key = key, file = file, output=output}  = do
   when (key == "") $ error "Key must be specified"
   when (file == "") $ error "File must be specified"
   when (output == "") $ error "Output must be specified"
   keyFile <- SIO.readFile key
   let (keyN, keyD) = readKey keyFile
   encryptedFileResults <- SIO.readFile file
   let splitted = (Prelude.init (DLS.splitOn "\n" encryptedFileResults)) in BL.writeFile output (BL.pack (decryptMsg (stringToInteger splitted) keyD keyN))
   SIO.putStrLn "Decryption Finished"

main = do
         opts <- cmdArgs ( modes [encrypt,decrypt]
                                 &= help "Basic RSA implementation"
                                 &= summary "rsa-haskell v0.1")
         optionHandler opts
