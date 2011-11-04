import Data.Maybe
import System.IO
import System.Environment
import Text.HTML.TagStream
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as E
import qualified Data.Enumerator.List as EL

main :: IO ()
main = do
    args <- getArgs
    filename <- maybe (fail "pass file path") return (listToMaybe args)
    E.run_ $ E.enumFile filename
             E.$= tokenStream
             E.$$ EL.consume
    return ()
