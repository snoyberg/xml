import System.Environment
import Control.Applicative
import Text.HTML.TagSoup
import qualified Data.ByteString.Char8 as S

main :: IO ()
main = do
    [filename] <- getArgs
    renderTags . parseTags <$> S.readFile filename >>= S.putStrLn
