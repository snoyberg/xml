import Data.Maybe
import System.Environment
import Text.HTML.TagStream
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as E
import qualified Data.Enumerator.List as EL

main :: IO ()
main = do
    args <- getArgs
    filename <- maybe (fail "pass file path") return (listToMaybe args)
    _ <- E.run_ $ E.enumFile filename
                E.$= tokenStream
                E.$$ EL.consume
    return ()
