-- Ryan Coley
-- rsa-haskell.hs
--Encrypts and Decrypts files using RSA
--rsa-haskell [-ed encrypt/decrypt] [-k key] [-f file] [-o output]

import qualified System.IO as SIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as B
import Data.Char
import System.Environment
import Data.ByteString.Char8
import qualified Data.List.Split as DLS
import System.Console.CmdArgs

encryptCharacter :: Integer -> Integer -> Integer -> Integer
encryptCharacter char keyN keyE = mod powop keyN where powop = char ^ keyE

decryptCharacter :: Integer -> Integer -> Integer -> Integer
decryptCharacter char keyD keyN = mod powop keyN where powop = char ^ keyD

convertWord8ToInteger :: [B.Word8] -> [Integer]
convertWord8ToInteger char = Prelude.map (\x -> (read (show x) :: Integer)) char

convertIntegerToWord8 :: [Integer] -> [B.Word8]
convertIntegerToWord8 int = Prelude.map (\x -> (read (show x) :: B.Word8)) int

encryptMsg :: [B.Word8] -> Integer -> Integer -> [Integer]
encryptMsg msg keyN keyE = Prelude.map (\x -> encryptCharacter x keyN keyE) (convertWord8ToInteger msg)

decryptMsg :: [Integer] -> Integer -> Integer -> [B.Word8]
decryptMsg msg keyD keyN = convertIntegerToWord8 (Prelude.map (\x -> decryptCharacter x keyD keyN) msg)

integerToString :: [Integer] -> [String]
integerToString x = Prelude.map (\y -> show y) x

stringToInteger :: [String] -> [Integer]
stringToInteger x = Prelude.map (\y -> read y :: Integer) x

data RSA = Encrypt {key :: String, file :: String, output :: String}
         | Decrypt {key :: String, file :: String, output :: String}
           deriving (Show, Data, Typeable)

encrypt = Encrypt {
    key = def &= typFile &= help "RSA Public Key",
    file = def &= typFile &= help "File to encrypt",
    output = def &= opt "encrypted_file.enc" &= typFile &= help "Output encrypted file"
} &= help "Encrypt Files"

decrypt = Decrypt {
    key = def &= typFile &= help "RSA Private Key",
    file = def &= typFile &= help "File to decrypt",
    output = def &= typFile &= help "Output decrypted file"
} &= help "Decrypt Files"

main = print =<< cmdArgs ( modes [encrypt,decrypt]
                                 &= help "Basic RSA implementation"
                                 &= summary "rsa-haskell v0.1")
{-do
          args <- getArgs
          contents <- BL.readFile (args!!0)
          Prelude.mapM (\x -> Prelude.appendFile "print3DArray.txt" (x ++ "\n")) (integerToString (encryptMsg (BL.unpack contents) 5917 5027))
          encryptedFileResults <- Prelude.readFile "print3DArray.txt"
          let splitted = (Prelude.init (DLS.splitOn "\n" encryptedFileResults)) in BL.writeFile "print3DArray_result.class" (BL.pack (decryptMsg (stringToInteger splitted) 1163 5917))-}
