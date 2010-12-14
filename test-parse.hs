import Text.XML.Enumerator.Parse
import qualified Data.ByteString as S
import Data.Enumerator
import qualified Data.Enumerator as E
import Control.Monad.IO.Class
import Blaze.ByteString.Builder.Enumerator
import Data.Enumerator.IO
import System.IO (withBinaryFile, IOMode (WriteMode))
import Text.XML.Enumerator.Render
import System.Environment (getArgs)

main :: IO ()
main = do
    [fn] <- getArgs
    x <- S.readFile fn
    run_ $ enumList 1 [x] $$ joinI $ detectUtf $$ joinI $ parseBytes $$ iterPrint
    withBinaryFile "test8.xml" WriteMode $ \h ->
        run_ $ enumList 1 [x] $$ joinI $ detectUtf $$ joinI $ parseBytes
            $$ joinI $ renderBuilder
            $$ joinI $ builderToByteString
            $$ iterHandle h
  where
    iterPrint = do
        x <- E.head
        case x of
            Nothing -> return ()
            Just y -> liftIO (print y) >> iterPrint
