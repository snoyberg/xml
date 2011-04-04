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
main = hspec $ fmap concat $ sequence [t0, t1]

t0 = describe "XML parsing and rendering"
    [ it "is idempotent to parse and render a document" documentParseRender
    , it "has valid parser combinators" combinators
    , it "has working ignoreSiblings function" testIgnoreSiblings
    , it "has working IgnoreElem function" testIgnoreElem
    , it "has working skipTill function" testSkipTill
    , it "has working choose function" testChoose
    , it "has working many function" testMany
    , it "has working orE" testOrE
    , it "has working chooseSplit" testChooseSplit
    , it "has working permute" testPermute
    , it "has working permuteFallback" testPermuteFallback
    , it "has working tags" testTags
    , it "has working tagsPermute" testTagsPermute
    , it "has working tagsPermuteRepetition" testTagsPermuteRepetition
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
        renderTextI = E.joinI $ R.renderText $$ EL.fold mappend mempty
        
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

testChooseSplit = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.chooseSplit (\t-> P.tagNoAttr t (return t)) ["a", "b", "c"]
        liftIO $ x @?= Just ("b",["a","c"])
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<b/>"
        , "</hello>"
        ]

testPermute 
    = do
        let frame input = P.parseLBS_ input decodeEntities $ do
                            P.force "need hello" $ P.tagNoAttr "hello" $ 
                             P.permute (\t -> P.tagNoAttr t (return t)) ["a", "b"]
        frame input1 >>= \result1 -> result1 @?= Just ["a", "b"]
        frame input2 >>= \result2 -> result2 @?= Just ["b", "a"]
        frame input3 >>= \result3 -> result3 @?= Nothing
        C.try (frame input4) >>= \result4 -> case result4 of
                                               Left (P.XmlException { 
                                                            P.xmlBadInput = Just (EventBeginElement 
                                                                                    Name { 
                                                                                      nameLocalName = "c"
                                                                                    , nameNamespace = Nothing
                                                                                    , namePrefix = Nothing 
                                                                                    }
                                                                                    _) 
                                                            }) -> return () -- right type of error
                                               Left  _ -> assertFailure "wrong error"
                                               Right _ -> assertFailure "erroneous document requires an error"
  where
    input1 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "<b/>"
        , "</hello>"
        ]
    input2 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<b/>"
        , "<a/>"
        , "</hello>"
        ]
    input3 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "</hello>"
        ]
    input4 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "<c/>"
        , "</hello>"
        ]

testPermuteFallback
    = do
        let frame input = P.parseLBS_ input decodeEntities $ do
                            P.force "need hello" $ P.tagNoAttr "hello" $ 
                             P.permuteFallback (fmap return `fmap` P.contentMaybe) 
                                               (\t -> P.tagNoAttr t (return $ nameLocalName t)) 
                                               ["a", "b"]
        frame input1 >>= \result1 -> result1 @?= Just ["a", "t", "b"]
        frame input2 >>= \result2 -> result2 @?= Just ["t", "b", "a"]
        frame input3 >>= \result3 -> result3 @?= Nothing
        C.try (frame input4) >>= \result4 -> case result4 of
                                               Left (P.XmlException { 
                                                            P.xmlBadInput = Just (EventBeginElement 
                                                                                    Name { 
                                                                                      nameLocalName = "c"
                                                                                    , nameNamespace = Nothing
                                                                                    , namePrefix = Nothing 
                                                                                    }
                                                                                    _) 
                                                            }) -> return () -- right type of error
                                               Left  _ -> assertFailure "wrong error"
                                               Right _ -> assertFailure "erroneous document requires an error"
  where
    input1 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "t"
        , "<b/>"
        , "</hello>"
        ]
    input2 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "t"
        , "<b/>"
        , "<a/>"
        , "</hello>"
        ]
    input3 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "</hello>"
        ]
    input4 = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "<c/>"
        , "</hello>"
        ]

testTags = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.tags (\state name -> do 
                       let n = nameLocalName name
                       guard (n == fromString [chr $ ord 'a' + state]) 
                       Just (return (), \_ -> return $ Just (state + 1, Just n)))
                    (const $ return Nothing)
                    0
        liftIO $ x @?= (5, ["a", "b", "c", "d", "e"])
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<a/>"
        , "<b/>"
        , "<c/>"
        , "<d/>"
        , "<e/>"
        , "</hello>"
        ]

testTagsPermute = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        let p c = (return (), \_ -> return (Just c))
        x <- P.tagsPermute (toLower . nameLocalName) 
                           (Map.fromList $ map (\c -> (c, p c)) 
                                   ["a", "b", "c", "d", "e"])
                           (return Nothing)
        liftIO $ x @?= Just ["d", "b", "e", "a", "c"]
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<d/>"
        , "<b/>"
        , "<E/>"
        , "<a/>"
        , "<C/>"
        , "</hello>"
        ]

testTagsPermuteRepetition = P.parseLBS_ input decodeEntities $ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        let p r c = (r, return (), \_ -> return (Just ()))
        x <- P.tagsPermuteRepetition (toLower . nameLocalName) 
                                     (Map.fromList $ map (\c -> (c, p P.repeatOnce c)) ["a", "b", "c", "d", "e"] ++
                                                     map (\c -> (c, p P.repeatMany c)) ["r"])
                                     (return Nothing)
        liftIO $ fmap (map fst) x @?= Just ["d", "r", "b", "e", "r", "a", "c"]
  where
    input = L.concat
        [ "<?xml version='1.0'?>\n"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<d/>"
        , "<r/>"
        , "<b/>"
        , "<E/>"
        , "<r/>"
        , "<a/>"
        , "<C/>"
        , "</hello>"
        ]
