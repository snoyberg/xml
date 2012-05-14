{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class       (liftIO)
import           Data.XML.Types
import           Test.HUnit                   hiding (Test)
import           Test.Hspec.Monadic
import qualified Data.ByteString.Char8        as S
import qualified Data.ByteString.Lazy.Char8   as L
import qualified Text.XML.Unresolved          as D
import qualified Text.XML.Stream.Parse        as P
import qualified Text.XML                     as Res
import qualified Text.XML.Cursor              as Cu
import           Text.XML.Stream.Parse        (def)

import Text.XML.Cursor ((&/), (&//), (&.//), ($|), ($/), ($//), ($.//))
import Data.Text(Text)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Set as Set
import Control.Exception (toException)
import Test.Hspec.HUnit ()

import qualified Data.Conduit as C

main :: IO ()
main = hspecX $ do
    describe "XML parsing and rendering" $ do
        it "is idempotent to parse and render a document" documentParseRender
        it "has valid parser combinators" combinators
        it "has working choose function" testChoose
        it "has working many function" testMany
        it "has working orE" testOrE
        it "is idempotent to parse and pretty render a document" documentParsePrettyRender
        it "ignores the BOM" parseIgnoreBOM
        it "strips duplicated attributes" stripDuplicateAttributes
        it "displays comments" testRenderComments
    describe "XML Cursors" $ do
        it "has correct parent" cursorParent
        it "has correct ancestor" cursorAncestor
        it "has correct orSelf" cursorOrSelf
        it "has correct preceding" cursorPreceding
        it "has correct following" cursorFollowing
        it "has correct precedingSibling" cursorPrecedingSib
        it "has correct followingSibling" cursorFollowingSib
        it "has correct descendant" cursorDescendant
        it "has correct check" cursorCheck
        it "has correct check with lists" cursorPredicate
        it "has correct checkNode" cursorCheckNode
        it "has correct checkElement" cursorCheckElement
        it "has correct checkName" cursorCheckName
        it "has correct anyElement" cursorAnyElement
        it "has correct element" cursorElement
        it "has correct laxElement" cursorLaxElement
        it "has correct content" cursorContent
        it "has correct attribute" cursorAttribute
        it "has correct laxAttribute" cursorLaxAttribute
        it "has correct &* and $* operators" cursorDeep
        it "has correct force" cursorForce
        it "has correct forceM" cursorForceM
        it "has correct hasAttribute" cursorHasAttribute
        it "has correct attributeIs" cursorAttributeIs
    describe "resolved" $ do
        it "identifies unresolved entities" resolvedIdentifies
        it "decodeHtmlEntities" testHtmlEntities
        it "works for resolvable entities" resolvedAllGood
        it "merges adjacent content nodes" resolvedMergeContent
        it "understands inline entity declarations" resolvedInline
    describe "pretty" $ do
        it "works" casePretty

documentParseRender :: IO ()
documentParseRender =
    mapM_ go docs
  where
    go x = x @=? D.parseLBS_ def (D.renderLBS def x)
    docs =
        [ Document (Prologue [] Nothing [])
                   (Element "foo" [] [])
                   []
        , D.parseLBS_ def
            "<?xml version=\"1.0\"?><!DOCTYPE foo>\n<foo/>"
        , D.parseLBS_ def
            "<?xml version=\"1.0\"?><!DOCTYPE foo>\n<foo><nested>&ignore;</nested></foo>"
        , D.parseLBS_ def
            "<foo><![CDATA[this is some<CDATA content>]]></foo>"
        , D.parseLBS_ def
            "<foo bar='baz&amp;bin'/>"
        , D.parseLBS_ def
            "<foo><?instr this is a processing instruction?></foo>"
        , D.parseLBS_ def
            "<foo><!-- this is a comment --></foo>"
        ]

documentParsePrettyRender :: IO ()
documentParsePrettyRender =
    L.unpack (D.renderLBS def { D.rsPretty = True } (D.parseLBS_ def doc)) @?= L.unpack doc
  where
    doc = L.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<foo>"
        , "    <?bar bar?>"
        , "    text"
        , "    <?bin bin?>"
        , "</foo>"
        ]

combinators :: Assertion
combinators = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagName "hello" (P.requireAttr "world") $ \world -> do
        liftIO $ world @?= "true"
        P.force "need child1" $ P.tagNoAttr "{mynamespace}child1" $ return ()
        P.force "need child2" $ P.tagNoAttr "child2" $ return ()
        P.force "need child3" $ P.tagNoAttr "child3" $ do
            x <- P.contentMaybe
            liftIO $ x @?= Just "combine <all> &content"
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello world='true'>"
        , "<?this should be ignored?>"
        , "<child1 xmlns='mynamespace'/>"
        , "<!-- this should be ignored -->"
        , "<child2>   </child2>"
        , "<child3>combine &lt;all&gt; <![CDATA[&content]]></child3>\n"
        , "</hello>"
        ]

testChoose :: Assertion
testChoose = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.tagNoAttr "failure" $ return 1
            , P.tagNoAttr "success" $ return 2
            ]
        liftIO $ x @?= Just (2 :: Int)
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "</hello>"
        ]

testMany :: Assertion
testMany = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.many $ P.tagNoAttr "success" $ return ()
        liftIO $ length x @?= 5
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "<success/>"
        , "<success/>"
        , "<success/>"
        , "<success/>"
        , "</hello>"
        ]

testOrE :: IO ()
testOrE = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.tagNoAttr "failure" (return 1) `P.orE`
             P.tagNoAttr "success" (return 2)
        liftIO $ x @?= Just (2 :: Int)
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "</hello>"
        ]


name :: [Cu.Cursor] -> [Text]
name [] = []
name (c:cs) = ($ name cs) $ case Cu.node c of
                              Res.NodeElement e -> ((Res.nameLocalName $ Res.elementName e) :)
                              _ -> id

cursor :: Cu.Cursor
cursor =
    Cu.fromDocument $ Res.parseLBS_ def input
  where
    input = L.concat
        [ "<foo attr=\"x\">"
        ,    "<bar1/>"
        ,    "<bar2>"
        ,       "<baz1/>"
        ,       "<baz2 attr=\"y\"/>"
        ,       "<baz3>a</baz3>"
        ,    "</bar2>"
        ,    "<bar3>"
        ,       "<bin1/>"
        ,       "b"
        ,       "<bin2/>"
        ,       "<bin3/>"
        ,    "</bar3>"
        ,    "<Bar1 xmlns=\"http://example.com\" Attr=\"q\"/>"
        , "</foo>"
        ]

bar2, baz2, bar3, bin2 :: Cu.Cursor
bar2 = Cu.child cursor !! 1
baz2 = Cu.child bar2 !! 1

bar3 = Cu.child cursor !! 2
bin2 = Cu.child bar3 !! 1

cursorParent, cursorAncestor, cursorOrSelf, cursorPreceding, cursorFollowing,
    cursorPrecedingSib, cursorFollowingSib, cursorDescendant, cursorCheck,
    cursorPredicate, cursorCheckNode, cursorCheckElement, cursorCheckName,
    cursorAnyElement, cursorElement, cursorLaxElement, cursorContent,
    cursorAttribute, cursorLaxAttribute, cursorHasAttribute,
    cursorAttributeIs, cursorDeep, cursorForce, cursorForceM,
    resolvedIdentifies, resolvedAllGood, resolvedMergeContent,
    testHtmlEntities
    :: Assertion
cursorParent = name (Cu.parent bar2) @?= ["foo"]
cursorAncestor = name (Cu.ancestor baz2) @?= ["bar2", "foo"]
cursorOrSelf = name (Cu.orSelf Cu.ancestor baz2) @?= ["baz2", "bar2", "foo"]
cursorPreceding = do
  name (Cu.preceding baz2) @?= ["baz1", "bar1"]
  name (Cu.preceding bin2) @?= ["bin1", "baz3", "baz2", "baz1", "bar2", "bar1"]
cursorFollowing = do
  name (Cu.following baz2) @?= ["baz3", "bar3", "bin1", "bin2", "bin3", "Bar1"]
  name (Cu.following bar2) @?= ["bar3", "bin1", "bin2", "bin3", "Bar1"]
cursorPrecedingSib = name (Cu.precedingSibling baz2) @?= ["baz1"]
cursorFollowingSib = name (Cu.followingSibling baz2) @?= ["baz3"]
cursorDescendant = (name $ Cu.descendant cursor) @?= T.words "bar1 bar2 baz1 baz2 baz3 bar3 bin1 bin2 bin3 Bar1"
cursorCheck = null (cursor $.// Cu.check (const False)) @?= True
cursorPredicate = (name $ cursor $.// Cu.check Cu.descendant) @?= T.words "foo bar2 baz3 bar3"
cursorCheckNode = (name $ cursor $// Cu.checkNode f) @?= T.words "bar1 bar2 bar3"
    where f (Res.NodeElement e) = "bar" `T.isPrefixOf` Res.nameLocalName (Res.elementName e)
          f _               = False
cursorCheckElement = (name $ cursor $// Cu.checkElement f) @?= T.words "bar1 bar2 bar3"
    where f e = "bar" `T.isPrefixOf` Res.nameLocalName (Res.elementName e)
cursorCheckName = (name $ cursor $// Cu.checkName f) @?= T.words "bar1 bar2 bar3"
    where f n = "bar" `T.isPrefixOf` nameLocalName n
cursorAnyElement = (name $ cursor $// Cu.anyElement) @?= T.words "bar1 bar2 baz1 baz2 baz3 bar3 bin1 bin2 bin3 Bar1"
cursorElement = (name $ cursor $// Cu.element "bar1") @?= ["bar1"]
cursorLaxElement = (name $ cursor $// Cu.laxElement "bar1") @?= ["bar1", "Bar1"]
cursorContent = do
  Cu.content cursor @?= []
  (cursor $.// Cu.content) @?= ["a", "b"]
cursorAttribute = Cu.attribute "attr" cursor @?= ["x"]
cursorLaxAttribute = (cursor $.// Cu.laxAttribute "Attr") @?= ["x", "y", "q"]

cursorHasAttribute = (length $ cursor $.// Cu.hasAttribute "attr") @?= 2
cursorAttributeIs = (length $ cursor $.// Cu.attributeIs "attr" "y") @?= 1

cursorDeep = do
  (Cu.element "foo" &/ Cu.element "bar2" &// Cu.attribute "attr") cursor @?= ["y"]
  (return &.// Cu.attribute "attr") cursor @?= ["x", "y"]
  (cursor $.// Cu.attribute "attr") @?= ["x", "y"]
  (cursor $/ Cu.element "bar2" &// Cu.attribute "attr") @?= ["y"]
  (cursor $/ Cu.element "bar2" &/ Cu.element "baz2" >=> Cu.attribute "attr") @?= ["y"]
  null (cursor $| Cu.element "foo") @?= False
cursorForce = do
  Cu.force () [] @?= (Nothing :: Maybe Integer)
  Cu.force () [1] @?= Just (1 :: Int)
  Cu.force () [1,2] @?= Just (1 :: Int)
cursorForceM = do
  Cu.forceM () [] @?= (Nothing :: Maybe Integer)
  Cu.forceM () [Just 1, Nothing] @?= Just (1 :: Int)
  Cu.forceM () [Nothing, Just (1 :: Int)] @?= Nothing

showEq :: (Show a, Show b) => Either a b -> Either a b -> Assertion
showEq x y = show x @=? show y

resolvedIdentifies =
    Left (toException $ Res.UnresolvedEntityException $ Set.fromList ["foo", "bar", "baz"]) `showEq`
    Res.parseLBS def
    "<root attr='&bar;'>&foo; --- &baz; &foo;</root>"

testHtmlEntities =
    Res.parseLBS_ def
        { P.psDecodeEntities = P.decodeHtmlEntities
        } xml1 @=? Res.parseLBS_ def xml2
  where
    xml1 = "<root>&nbsp;</root>"
    xml2 = "<root>&#160;</root>"

resolvedAllGood =
    D.parseLBS_ def xml @=?
    Res.toXMLDocument (Res.parseLBS_ def xml)
  where
    xml = "<foo><bar/><baz/></foo>"

resolvedMergeContent =
    Res.documentRoot (Res.parseLBS_ def xml) @=?
    Res.Element "foo" [] [Res.NodeContent "bar&baz"]
  where
    xml = "<foo>bar&amp;baz</foo>"

parseIgnoreBOM :: Assertion
parseIgnoreBOM = do
    either (const $ Left (1 :: Int)) Right (Res.parseText Res.def "\xfeef<foo/>") @?=
        either (const $ Left (2 :: Int)) Right (Res.parseText Res.def "<foo/>")

stripDuplicateAttributes :: Assertion
stripDuplicateAttributes = do
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo bar=\"baz\"/>" @=?
        D.renderLBS def (Document (Prologue [] Nothing []) (Element "foo" [("bar", [ContentText "baz"]), ("bar", [ContentText "bin"])] []) [])
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo x:bar=\"baz\" xmlns:x=\"namespace\"/>" @=?
        D.renderLBS def (Document (Prologue [] Nothing []) (Element "foo"
            [ ("x:bar", [ContentText "baz"])
            , (Name "bar" (Just "namespace") (Just "x"), [ContentText "bin"])
            ] []) [])

testRenderComments :: Assertion
testRenderComments =do
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo><!--comment--></foo>"
        @=? D.renderLBS def (Document (Prologue [] Nothing [])
            (Element "foo" [] [NodeComment "comment"]) [])

resolvedInline :: Assertion
resolvedInline = do
    Res.Document _ root _ <- return $ Res.parseLBS_ Res.def "<!DOCTYPE foo [<!ENTITY bar \"baz\">]><foo>&bar;</foo>"
    root @?= Res.Element "foo" [] [Res.NodeContent "baz"]
    Res.Document _ root2 _ <- return $ Res.parseLBS_ Res.def "<!DOCTYPE foo [<!ENTITY bar \"baz\">]><foo bar='&bar;'/>"
    root2 @?= Res.Element "foo" [("bar", "baz")] []

casePretty :: Assertion
casePretty = do
    let pretty = S.unlines
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<!DOCTYPE foo>"
            , "<foo bar=\"bar\" baz=\"baz\">"
            , "    <foo"
            , "      bar=\"bar\""
            , "      baz=\"baz\""
            , "      bin=\"bin\">"
            , "        Hello World"
            , "    </foo>"
            , "    <foo/>"
            , "    <?foo bar?>"
            , "    <!-- foo bar baz bin -->"
            , "    <bar>"
            , "        bar content"
            , "    </bar>"
            , "</foo>"
            ]
        doctype = Res.Doctype "foo" Nothing
        doc = Res.Document (Res.Prologue [] (Just doctype) []) root []
        root = Res.Element "foo" [("bar", "bar"), ("baz", "baz")]
                [ Res.NodeElement $ Res.Element "foo" [("bar", "bar"), ("baz", "baz"), ("bin", "bin")]
                    [ Res.NodeContent "  Hello World\n\n"
                    , Res.NodeContent "  "
                    ]
                , Res.NodeElement $ Res.Element "foo" [] []
                , Res.NodeInstruction $ Res.Instruction "foo" "bar"
                , Res.NodeComment "foo bar\n\r\nbaz    \tbin "
                , Res.NodeElement $ Res.Element "bar" [] [Res.NodeContent "bar content"]
                ]
    pretty @=? S.concat (L.toChunks $ Res.renderLBS def { D.rsPretty = True } doc)
