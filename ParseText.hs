import Data.Maybe
import System.Environment
import qualified Text.HTML.TagStream.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as CL

main :: IO ()
main = do
    args <- getArgs
    filename <- maybe (fail "pass file path") return (listToMaybe args)
    _ <- C.runResourceT $
           C.sourceFile filename
           C.$= CL.map (decodeUtf8With lenientDecode)
           C.$= T.tokenStream
           C.$$ CL.consume
    return ()
