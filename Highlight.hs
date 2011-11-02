import Data.Maybe
import Control.Applicative
import System.Environment
import Text.HTML.TagStream
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.Console.ANSI

color :: Color -> ByteString -> ByteString
color c s = S.concat [ S.pack $ setSGRCode [SetColor Foreground Dull c]
                     , s
                     , S.pack $ setSGRCode [SetColor Foreground Dull White]
                     ]

main = do
    args <- getArgs
    (Right tokens) <- decode <$> maybe S.getContents S.readFile (listToMaybe args)
    S.putStr $ encodeHL (color Red) tokens
