{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans.Resource
import Data.Conduit (Consumer, Sink, ($$))
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Text.XML.Stream.Parse

data Person = Person Int Text
    deriving Show

parsePerson :: MonadThrow m => Consumer Event m (Maybe Person)
parsePerson = tagName "person" (requireAttr "age") $ \age -> do
    name <- content
    return $ Person (read $ unpack age) name

parsePeople :: MonadThrow m => Sink Event m (Maybe [Person])
parsePeople = tagNoAttr "people" $ many parsePerson

main :: IO ()
main = do
    people <- runResourceT $
            parseFile def "people.xml" $$ force "people required" parsePeople
    print people

