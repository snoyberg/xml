import Data.Maybe
import System.IO
import System.Environment
import Text.HTML.TagStream
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.Console.ANSI
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Blaze.ByteString.Builder (Builder)
import Data.Conduit.Blaze (builderToByteString)

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
        C.$= tokenStream
        C.$= CL.map (showToken (color Red))
        C.$= builderToByteString
        C.$$ CB.sinkHandle stdout
