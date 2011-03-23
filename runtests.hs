{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit hiding (Test)

import Data.XML.Types
import qualified Text.XML.Enumerator.Document as D
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
main = hspec $ fmap concat $ sequence [t0, t1]

t0 = describe "XML parsing and rendering"
    [ it "is idempotent to parse and render a document" documentParseRender
    , it "has valid parser combinators" combinators
    , it "has working ignoreSiblings function" testIgnoreSiblings
    , it "has working IgnoreElem function" testIgnoreElem
    , it "has working skipTill function" testSkipTill
    , it "has working choose function" testChoose
    , it "has working many function" testMany
    ]
    
t1 = fmap concat $ sequence [
        describe "Checking dependency of chunk size for iteratees" [
                  it "doesn't depend for ignoreSiblings" testSib
                , it "doesn't depend for ignoreElem" testElem
                , it "doesn't depend for skipTill" testSkipTill
                , it "doesn't depend for tag" testTag
            ],
        describe "Process nested iteratees. (Launch missiles?..)" [
                  it "can run nested iteratees for siblings. E.g. to render text" testProcSib
                , it "can run nested iteratees for next element" testProcElem
            ]
    ]
    where
        testSib = testI P.ignoreSiblings $ drop 2 testData
        testElem = testI P.ignoreElem $ drop 2 testData
        testSkipTill = testI (P.skipTill $ P.tagNoAttr "E2" P.ignoreSiblings) $ drop 1 testData
        testTag = testI (P.tagNoAttr "root" $ P.skipTill $ P.tagNoAttr "E2" P.contentMaybe) testData
        testProcSib = join $ fmap (@?=("<E0><E1><E11/><E12/></E1><E3/></E0><E2/>", [EventEndElement "root"])) $ 
                                resI (P.processSiblings renderTextI) (drop 1 testData) 1
        testProcElem = join $ fmap (@?=(Just "<E0><E1><E11/><E12/></E1><E3/></E0>", dropWhile (/=EventBeginElement "E2" mempty) testData)) $ 
                                resI (P.processElem renderTextI) (drop 1 testData) 1
        renderTextI = E.joinI $ R.renderText $$ E.foldl' mappend mempty
        
        testData = [
                  EventBeginElement "root" mempty
                , EventBeginElement "E0" mempty
                , EventBeginElement "E1" mempty
                , EventBeginElement "E11" mempty
                , EventEndElement "E11"
                , EventBeginElement "E12" mempty
                , EventEndElement "E12"
                , EventEndElement "E1"
                , EventBeginElement "E3" mempty
                , EventEndElement "E3"
                , EventEndElement "E0"
                , EventBeginElement "E2" mempty
                , EventEndElement "E2"
                , EventEndElement "root"
            ]
        resI i xs n = E.run_ $ E.enumList n xs $$ (i >>= \r-> EL.consume >>= \c -> return (r,c))
        cmpI i xs n = liftM2 (==) (resI i xs n) (resI i xs $ n+1)
        testI :: (Eq t, Eq a) => E.Iteratee a IO t -> [a] -> IO ()
        testI i xs = assert $ fmap and $ mapM (cmpI i xs) [1..20] 


documentParseRender =
    mapM_ go docs
  where
    go x = x @=? D.parseLBS_ (D.renderLBS x) decodeEntities
    docs =
        [ Document (Prologue [] Nothing [])
                   (Element "foo" Map.empty [])
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

testIgnoreSiblings = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        P.ignoreSiblings
        return ()
        
testIgnoreElem  = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        P.ignoreElem
        P.ignoreElem
        return ()

testSkipTill = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        P.skipTill (P.tagNoAttr "ignore" P.ignoreSiblings)
        return ()
--  where
input = L.concat
    [ "<?xml version='1.0'?>\n"
    , "<!DOCTYPE foo []>\n"
    , "<hello>"
    , "<success/>"
    , "<ignore>"
    , "<nested>"
    , "<fail/>"
    , "</nested>"
    , "</ignore>\n"
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
