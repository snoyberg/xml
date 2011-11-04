{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import System.Environment (getArgs)
import System.IO (openBinaryFile, IOMode(..), stdout)
import qualified Data.Enumerator.Binary as E
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Data.Attoparsec.Char8
import Text.HTML.TagStream
import Blaze.ByteString.Builder.Enumerator (builderToByteString)

type Protocol = ByteString
type Domain = ByteString
type Path = ByteString
url :: Parser (Protocol, Domain, Path)
url = (,,) <$> (string "http://" <|> string "https://")
           <*> takeTill (=='/')
           <*> takeByteString

changeUrl :: ByteString -> ByteString
changeUrl s = either (const s) changeDomain $ parseOnly url s
  where
    changeDomain (prop, domain, path) =
      S.concat [ prop
               , domain
               , ".proxy.com"
               , path
               ]

withUrl :: Monad m => (ByteString -> ByteString) -> E.Enumeratee Token Token m b
withUrl f = EL.map filter'
  where filter' :: Token -> Token
        filter' (TagOpen name as close)  = TagOpen name (map filter'' as) close
        filter' t = t
        filter'' :: Attr -> Attr
        filter'' (name, value)
            | name=="href" || name=="src" = (name, f value)
            | otherwise = (name, value)

main :: IO ()
main = do
    [filename] <- getArgs
    E.run_ $ E.enumFile filename
             E.$= tokenStream
             E.$= withUrl changeUrl
             E.$= EL.map (showToken id)
             E.$= builderToByteString
             E.$$ E.iterHandle stdout
