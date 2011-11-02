{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import System.IO (openBinaryFile, IOMode(..))
import qualified Data.Enumerator.Binary as E
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Text.HTML.TagStream

withUrl :: Monad m => (ByteString -> ByteString) -> E.Enumeratee Token Token m b
withUrl f = EL.map filter'
  where filter' :: Token -> Token
        filter' (TagOpen name as)  = TagOpen name (map filter'' as)
        filter' t = t
        filter'' :: Attr -> Attr
        filter'' (name, value)
            | name=="href" || name=="src" = (name, f value)
            | otherwise = (name, value)

changeHost :: ByteString -> ByteString
changeHost = id

main :: IO ()
main = do
    [filename] <- getArgs
    h <- openBinaryFile filename ReadMode
    let bufSize = 2
        enum = E.enumHandle bufSize h
    tokens <- E.run_ $ ((enum E.$= tokenStream) E.$= withUrl changeHost) E.$$ EL.consume
    S.putStrLn $ S.concat $ map showToken tokens
