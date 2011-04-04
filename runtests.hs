{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                (guard)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Char                    (chr,ord)
import           Data.String                  (fromString)
import           Data.Text                    (toLower)
import           Data.XML.Types
import           Test.HUnit                   hiding (Test)
import           Test.Hspec
import           Test.Hspec.HUnit
import           Text.XML.Enumerator.Parse    (decodeEntities)
import qualified Control.Exception            as C
import qualified Data.ByteString.Lazy.Char8   as L
import qualified Data.Map                     as Map
import qualified Text.XML.Enumerator.Document as D
import qualified Text.XML.Enumerator.Parse    as P

import Text.XML.Enumerator.Parse (decodeEntities)
import qualified Text.XML.Enumerator.Parse as P
import qualified Text.XML.Enumerator.Render as R
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)
import qualified Data.Enumerator as E
import Data.Enumerator(($$))
import qualified Data.Enumerator.List as EL
import Data.Monoid
import Data.Text(Text)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad
import Control.Applicative((<$>), (<*>))

main :: IO ()
main = hspec $ describe "XML parsing and rendering"
    [ it "is idempotent to parse and render a document" documentParseRender
    , it "has valid parser combinators" combinators
    , it "has working choose function" testChoose
    , it "has working many function" testMany
    , it "has working orE" testOrE
    ]

documentParseRender =
    mapM_ go docs
  where
    go x = x @=? D.parseLBS_ (D.renderLBS x) decodeEntities
    docs =
        [ Document (Prologue [] Nothing [])
                   (Element "foo" [] [])
                   []
        , D.parseLBS_
            "<?xml version=\"1.0\"?>\n<!DOCTYPE foo>\n<foo/>"
            decodeEntities
        , D.parseLBS_
            "<?xml version=\"1.0\"?>\n<!DOCTYPE foo>\n<foo><nested>&ignore;</nested></foo>"
            decodeEntities
        , D.parseLBS_
            "<foo><![CDATA[this is some<CDATA content>]]></foo>"
            decodeEntities
        , D.parseLBS_
            "<foo bar='baz&amp;bin'/>"
            decodeEntities
        , D.parseLBS_
            "<foo><?instr this is a processing instruction?></foo>"
            decodeEntities
        , D.parseLBS_
            "<foo><!-- this is a comment --></foo>"
            decodeEntities
        ]

combinators = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagName "hello" (P.requireAttr "world") $ \world -> do
        liftIO $ world @?= "true"
        P.force "need child1" $ P.tagNoAttr "{mynamespace}child1" $ return ()
        P.force "need child2" $ P.tagNoAttr "child2" $ return ()
        P.force "need child3" $ P.tagNoAttr "child3" $ do
            x <- P.contentMaybe
            liftIO $ x @?= Just "combine <all> &content"
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello world='true'>"
        , "<?this should be ignored?>"
        , "<child1 xmlns='mynamespace'/>"
        , "<!-- this should be ignored -->"
        , "<child2>   </child2>"
        , "<child3>combine &lt;all&gt; <![CDATA[&content]]></child3>\n"
        , "</hello>"
        ]

testChoose = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.tagNoAttr "failure" $ return 1
            , P.tagNoAttr "success" $ return 2
            ]
        liftIO $ x @?= Just 2
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "</hello>"
        ]

testMany = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.many $ P.tagNoAttr "success" $ return ()
        liftIO $ length x @?= 5
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "<success/>"
        , "<success/>"
        , "<success/>"
        , "<success/>"
        , "</hello>"
        ]

testOrE = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.tagNoAttr "failure" (return 1) `P.orE`
             P.tagNoAttr "success" (return 2)
        liftIO $ x @?= Just 2
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "</hello>"
        ]
