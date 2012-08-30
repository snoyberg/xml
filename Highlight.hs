import System.IO
import System.Environment
import System.Console.ANSI

import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

import Data.Conduit.Blaze (builderToByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import qualified Text.HTML.TagStream.ByteString as S

color :: Color -> ByteString -> ByteString
color c s = S.concat [ S.pack $ setSGRCode [SetColor Foreground Dull c]
                     , s
                     , S.pack $ setSGRCode [SetColor Foreground Dull White]
                     ]

main :: IO ()
main = do
    args <- getArgs
    filename <- maybe (fail "Highlight file") return (listToMaybe args)
    C.runResourceT $
        CB.sourceFile filename
        C.$= S.tokenStream
        C.$= CL.map (S.showToken (color Red))
        C.$= builderToByteString
        C.$$ CB.sinkHandle stdout
