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

convertStrToInteger :: [B.Word8] -> [Integer]
convertStrToInteger str = Prelude.map (\x -> (read (show x) :: Integer)) str

encryptMsg :: [B.Word8] -> Integer -> Integer -> [Integer]
encryptMsg msg keyN keyE = Prelude.map (\x -> encryptCharacter x keyN keyE) (convertStrToInteger msg)


--to get file contents BL.readFile (args!!0)

main = do
          args <- getArgs
          mapM SIO.putStrLn args
          contents <- BL.readFile (args!!0)
          BL.putStr contents
          print "------------------------------------------"
          print (encryptMsg (BL.unpack contents) 6261533 65537)
          print (mod (2876799 ^ 6261533) 62534171)
