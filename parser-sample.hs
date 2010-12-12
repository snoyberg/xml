{-# LANGUAGE OverloadedStrings #-}
import Text.XML.Enumerator.Parse
import Data.Text.Lazy (Text, unpack)

data Person = Person { age :: Int, name :: Text }
    deriving Show

parsePerson = tag' "person" (requireAttr "age") $ \age -> do
    name <- content'
    return $ Person (read $ unpack age) name

parsePeople = tag'' "people" $ many parsePerson

main = parseFile_ "people.xml" (const Nothing) $ force "people required" parsePeople
