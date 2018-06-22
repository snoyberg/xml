import System.IO
import System.Environment
import System.Console.ANSI

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import Data.Text.Lazy.Internal (defaultChunkSize)
import Data.Text.Lazy.Builder (toLazyTextWith)

import qualified Data.Conduit.Blaze as B
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import qualified Text.HTML.TagStream.Text as T

color :: Color -> Text -> Text
color c s = T.concat [ T.pack $ setSGRCode [SetColor Foreground Dull c]
                     , s
                     , T.pack $ setSGRCode [SetColor Foreground Dull White]
                     ]

main :: IO ()
main = do
    args <- getArgs
    filename <- maybe (fail "Highlight file") return (listToMaybe args)
    C.runResourceT $
        CB.sourceFile filename
        C.$= CL.map (decodeUtf8With lenientDecode)
        C.$= T.tokenStream
        C.$= CL.map (B.fromLazyByteString . L.encodeUtf8 . (toLazyTextWith defaultChunkSize) . T.showToken (color Red))
        C.$= B.builderToByteString
        C.$$ CB.sinkHandle stdout
