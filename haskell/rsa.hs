-- Ryan Coley
-- rsa.hs

import qualified System.IO as SIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as B
import Data.Char
import System.Environment
import Data.ByteString.Char8
--import Data.ByteString.Base64.Lazy (encode, decode)

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

correctDecrypt :: [B.Word8] -> [B.Word8] -> Bool
correctDecrypt [] [] = True
correctDecrypt _ [] = False
correctDecrypt [] _ = False
correctDecrypt (x:xs) (y:ys) = if x /= y then False else correctDecrypt xs ys

--to get file contents BL.readFile (args!!0)

main = do
          args <- getArgs
          --mapM SIO.putStrLn args
          contents <- BL.readFile (args!!0)
          --SIO.putStr "Encrypted\n"
          --print (encryptMsg (BL.unpack contents) 143 7)
          --SIO.putStr "\n\n\nDecrypted\n"
          --print (decryptMsg (encryptMsg (BL.unpack contents) 143 7) 103 143)
          --SIO.putStr "\n\n\nOriginal\n"
          --print (BL.unpack contents)
          --if (correctDecrypt (BL.unpack contents) (decryptMsg (encryptMsg (BL.unpack contents) 143 7) 103 143)) then SIO.putStrLn "True" else SIO.putStrLn "False"
          print ((decryptMsg (encryptMsg (BL.unpack contents) 5917 5027) 1163 5917)!!0)
