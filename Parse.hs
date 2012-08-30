import Data.Maybe
import System.Environment
import qualified Text.HTML.TagStream.ByteString as S
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as CL

main :: IO ()
main = do
    args <- getArgs
    filename <- maybe (fail "pass file path") return (listToMaybe args)
    _ <- C.runResourceT $
           C.sourceFile filename
           C.$= S.tokenStream
           C.$$ CL.consume
    return ()
