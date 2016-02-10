{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception            (Exception)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Typeable                (Typeable)
import           Data.XML.Types
import           Test.HUnit                   hiding (Test)
import           Test.Hspec
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
import Control.Applicative ((<$>))
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import qualified Data.Set as Set
import Control.Exception (toException)

import qualified Data.Conduit as C
import qualified Control.Monad.Trans.Resource as C
import qualified Data.Conduit.List as CL
import qualified Data.Map as Map
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.String (renderMarkup)

main :: IO ()
main = hspec $ do
    describe "XML parsing and rendering" $ do
        it "is idempotent to parse and render a document" documentParseRender
        it "has valid parser combinators" combinators
        context "has working choose function" testChoose
        it "has working many function" testMany
        it "has working many' function" testMany'
        it "has working manyYield function" testManyYield
        it "has working orE" testOrE
        it "is idempotent to parse and pretty render a document" documentParsePrettyRender
        it "ignores the BOM" parseIgnoreBOM
        it "strips duplicated attributes" stripDuplicateAttributes
        it "displays comments" testRenderComments
        it "conduit parser" testConduitParser
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
    describe "top level namespaces" $ do
        it "works" caseTopLevelNamespace
        it "works with prefix" caseTopLevelNamespacePrefix
        it "handles conflicts" caseTLNConflict
    describe "blaze-html instances" $ do
        it "works" caseBlazeHtml
    describe "attribute reordering" $ do
        it "works" caseAttrReorder
    describe "ordering attributes explicitly" $ do
        it "works" caseOrderAttrs
    it "parsing CDATA" caseParseCdata
    it "retains namespaces when asked" caseRetainNamespaces
    it "handles iso-8859-1" caseIso8859_1
    it "renders CDATA when asked" caseRenderCDATA 
    it "escapes CDATA closing tag in CDATA" caseEscapesCDATA

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

testChoose :: Spec
testChoose = do
    it "can choose between elements"
        testChooseEitherElem
    it "can choose between elements and text, returning text"
        testChooseElemOrTextIsText
    it "can choose between elements and text, returning elements"
        testChooseElemOrTextIsElem
    it "can choose between text and elements, returning text"
        testChooseTextOrElemIsText
    it "can choose between text and elements, returning elements"
        testChooseTextOrElemIsElem
    it "can choose between text and elements, when the text is encoded"
        testChooseElemOrTextIsEncoded
    it "can choose between text and elements, when the text is whitespace"
        testChooseElemOrTextIsWhiteSpace
    it "can choose betwen text and elements, when the whitespace is both literal and encoded"
        testChooseElemOrTextIsChunkedText
    it "can choose between text and elements, when the text is chunked the other way"
        testChooseElemOrTextIsChunkedText2

testChooseElemOrTextIsText :: Assertion
testChooseElemOrTextIsText = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.tagNoAttr "failure" $ return "boom"
            , P.contentMaybe
            ]
        liftIO $ x @?= Just " something "
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , " something "
        , "</hello>"
        ]

testChooseElemOrTextIsEncoded :: Assertion
testChooseElemOrTextIsEncoded = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.tagNoAttr "failure" $ return "boom"
            , P.contentMaybe
            ]
        liftIO $ x @?= Just "\160something\160"
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "&#160;something&#160;"
        , "</hello>"
        ]

testChooseElemOrTextIsWhiteSpace :: Assertion
testChooseElemOrTextIsWhiteSpace = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.tagNoAttr "failure" $ return "boom"
            , P.contentMaybe
            ]
        liftIO $ x @?= Just "\x20\x20\x20"
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>   </hello>"
        ]

testChooseElemOrTextIsChunkedText :: Assertion
testChooseElemOrTextIsChunkedText = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.tagNoAttr "failure" $ return "boom"
            , P.contentMaybe
            ]
        liftIO $ x @?= Just "\x20\x20\x20"
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello> &#x20; </hello>"
        ]

testChooseElemOrTextIsChunkedText2 :: Assertion
testChooseElemOrTextIsChunkedText2 = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.tagNoAttr "failure" $ return "boom"
            , P.contentMaybe
            ]
        liftIO $ x @?= Just "\x20\x20\x20"
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>&#x20; &#x20;</hello>"
        ]

testChooseElemOrTextIsElem :: Assertion
testChooseElemOrTextIsElem = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.tagNoAttr "success" $ return "success"
            , P.contentMaybe
            ]
        liftIO $ x @?= Just "success"
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "</hello>"
        ]

testChooseTextOrElemIsText :: Assertion
testChooseTextOrElemIsText = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.contentMaybe
            , P.tagNoAttr "failure" $ return "boom"
            ]
        liftIO $ x @?= Just " something "
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , " something "
        , "</hello>"
        ]

testChooseTextOrElemIsElem :: Assertion
testChooseTextOrElemIsElem = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.contentMaybe
            , P.tagNoAttr "success" $ return "success"
            ]
        liftIO $ x @?= Just "success"
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "</hello>"
        ]

testChooseEitherElem :: Assertion
testChooseEitherElem = C.runResourceT $ P.parseLBS def input C.$$ do
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

testManyYield :: Assertion
testManyYield = do
    -- Basically the same as testMany, but consume the streamed result
    result <- C.runResourceT $
        P.parseLBS def input C.$$ helloParser
        C.$= CL.consume
    length result @?= 5
  where
    helloParser = void $ P.tagNoAttr "hello" $ P.manyYield successParser
    successParser = P.tagNoAttr "success" $ return ()
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

testMany' :: Assertion
testMany' = C.runResourceT $ P.parseLBS def input C.$$ do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.many' $ P.tagNoAttr "success" $ return ()
        liftIO $ length x @?= 5
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "<success/>"
        , "<success/>"
        , "<foobar/>"
        , "<success/>"
        , "<foo><bar attr=\"1\">some content</bar></foo>"
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

testConduitParser :: Assertion
testConduitParser = C.runResourceT $ do
    x <- P.parseLBS def input
        C.$= (P.force "need hello" $ P.tagNoAttr "hello" f)
        C.$$ CL.consume
    liftIO $ x @?= [1, 1, 1]
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<item/>"
        , "<item/>"
        , "<item/>"
        , "</hello>"
        ]
    f :: C.MonadThrow m => C.Conduit Event m Int
    f = do
        ma <- P.tagNoAttr "item" (return 1)
        maybe (return ()) (\a -> C.yield a >> f) ma


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
  Cu.force DummyEx [] @?= (Nothing :: Maybe Integer)
  Cu.force DummyEx [1] @?= Just (1 :: Int)
  Cu.force DummyEx [1,2] @?= Just (1 :: Int)
cursorForceM = do
  Cu.forceM DummyEx [] @?= (Nothing :: Maybe Integer)
  Cu.forceM DummyEx [Just 1, Nothing] @?= Just (1 :: Int)
  Cu.forceM DummyEx [Nothing, Just (1 :: Int)] @?= Nothing

data DummyEx = DummyEx
    deriving (Show, Typeable)
instance Exception DummyEx

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
    Res.Element "foo" Map.empty [Res.NodeContent "bar&baz"]
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
    root @?= Res.Element "foo" Map.empty [Res.NodeContent "baz"]
    Res.Document _ root2 _ <- return $ Res.parseLBS_ Res.def "<!DOCTYPE foo [<!ENTITY bar \"baz\">]><foo bar='&bar;'/>"
    root2 @?= Res.Element "foo" (Map.singleton "bar" "baz") []

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
        root = Res.Element "foo" (Map.fromList [("bar", "bar"), ("baz", "baz")])
                [ Res.NodeElement $ Res.Element "foo" (Map.fromList [("bar", "bar"), ("baz", "baz"), ("bin", "bin")])
                    [ Res.NodeContent "  Hello World\n\n"
                    , Res.NodeContent "  "
                    ]
                , Res.NodeElement $ Res.Element "foo" Map.empty []
                , Res.NodeInstruction $ Res.Instruction "foo" "bar"
                , Res.NodeComment "foo bar\n\r\nbaz    \tbin "
                , Res.NodeElement $ Res.Element "bar" Map.empty [Res.NodeContent "bar content"]
                ]
    pretty @=? S.concat (L.toChunks $ Res.renderLBS def { D.rsPretty = True } doc)

caseTopLevelNamespace :: Assertion
caseTopLevelNamespace = do
    let lbs = S.concat
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<foo xmlns:bar=\"baz\">"
            , "<subfoo bar:bin=\"\"/>"
            , "</foo>"
            ]
        rs = def { D.rsNamespaces = [("bar", "baz")] }
        doc = Res.Document (Res.Prologue [] Nothing [])
                (Res.Element "foo" Map.empty
                    [ Res.NodeElement
                        $ Res.Element "subfoo" (Map.singleton "{baz}bin" "") []
                    ])
                []
    lbs @=? S.concat (L.toChunks $ Res.renderLBS rs doc)

caseTopLevelNamespacePrefix :: Assertion
caseTopLevelNamespacePrefix = do
    let lbs = S.concat
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<foo xmlns:bar=\"baz\">"
            , "<subfoo bar:bin=\"\"/>"
            , "</foo>"
            ]
        rs = def { D.rsNamespaces = [("bar", "baz")] }
        doc = Res.Document (Res.Prologue [] Nothing [])
                (Res.Element "foo" Map.empty
                    [ Res.NodeElement
                        $ Res.Element "subfoo" (Map.fromList [(Name "bin" (Just "baz") (Just "bar"), "")]) []
                    ])
                []
    lbs @=? S.concat (L.toChunks $ Res.renderLBS rs doc)

caseTLNConflict :: Assertion
caseTLNConflict = do
    let lbs = S.concat
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<foo xmlns:bar=\"something\" bar:x=\"y\">"
            , "<subfoo xmlns:bar_=\"baz\" bar_:bin=\"\"/>"
            , "</foo>"
            ]
        rs = def { D.rsNamespaces = [("bar", "baz")] }
        doc = Res.Document (Res.Prologue [] Nothing [])
                (Res.Element "foo" (Map.fromList [(Name "x" (Just "something") (Just "bar"), "y")])
                    [ Res.NodeElement
                        $ Res.Element "subfoo" (Map.fromList [(Name "bin" (Just "baz") (Just "bar"), "")]) []
                    ])
                []
    lbs @=? S.concat (L.toChunks $ Res.renderLBS rs doc)

caseBlazeHtml :: Assertion
caseBlazeHtml =
    expected @=? str
  where
    str = renderMarkup $ toMarkup $ Res.Document (Res.Prologue [] Nothing []) root []
    root :: Res.Element
    root = Res.Element "html" Map.empty
        [ Res.NodeElement $ Res.Element "head" Map.empty
            [ Res.NodeElement $ Res.Element "title" Map.empty [Res.NodeContent "Test"]
            , Res.NodeElement $ Res.Element "script" Map.empty
                [Res.NodeContent "if (5 < 6 || 8 > 9) alert('Hello World!');"]
            , Res.NodeElement $ Res.Element "{http://www.snoyman.com/xml2html}ie-cond" (Map.singleton "cond" "lt IE 7")
                [Res.NodeElement $ Res.Element "link" (Map.singleton "href" "ie6.css") []]
            , Res.NodeElement $ Res.Element "style" Map.empty
                [Res.NodeContent "body > h1 { color: red }"]
            ]
        , Res.NodeElement $ Res.Element "body" Map.empty
            [ Res.NodeElement $ Res.Element "h1" Map.empty [Res.NodeContent "Hello World!"]
            ]
        ]
    expected :: String
    expected = concat
        [ "<!DOCTYPE HTML>\n"
        , "<html><head><title>Test</title><script>if (5 < 6 || 8 > 9) alert('Hello World!');</script>"
        , "<!--[if lt IE 7]><link href=\"ie6.css\" /><![endif]-->"
        , "<style>body > h1 { color: red }</style>"
        , "</head>"
        , "<body><h1>Hello World!</h1></body></html>"
        ]

caseAttrReorder :: Assertion
caseAttrReorder = do
    let lbs = S.concat
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<foo c=\"c\" b=\"b\" a=\"a\">"
            , "<bar a=\"a\" b=\"b\" c=\"c\"/>"
            , "</foo>"
            ]
        rs = def { Res.rsAttrOrder = \name m ->
                        case name of
                            "foo" -> reverse $ Map.toAscList m
                            _ -> Map.toAscList m
                 }
        attrs = Map.fromList [("a", "a"), ("b", "b"), ("c", "c")]
        doc = Res.Document (Res.Prologue [] Nothing [])
                (Res.Element "foo" attrs
                    [ Res.NodeElement
                        $ Res.Element "bar" attrs []
                    ])
                []
    lbs @=? S.concat (L.toChunks $ Res.renderLBS rs doc)

caseOrderAttrs :: Assertion
caseOrderAttrs = do
    let lbs = S.concat
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<foo c=\"c\" b=\"b\" a=\"a\">"
            , "<bar a=\"a\" b=\"b\" c=\"c\"/>"
            , "</foo>"
            ]
        rs = def { Res.rsAttrOrder = Res.orderAttrs
                     [("foo", ["c", "b"])]
                 }
        attrs = Map.fromList [("a", "a"), ("b", "b"), ("c", "c")]
        doc = Res.Document (Res.Prologue [] Nothing [])
                (Res.Element "foo" attrs
                    [ Res.NodeElement
                        $ Res.Element "bar" attrs []
                    ])
                []
    lbs @=? S.concat (L.toChunks $ Res.renderLBS rs doc)

caseParseCdata :: Assertion
caseParseCdata = do
    let lbs = "<a><![CDATA[www.google.com]]></a>"
        doc = Res.Document (Res.Prologue [] Nothing [])
                (Res.Element "a" Map.empty
                    [ Res.NodeContent "www.google.com"
                    ])
                []
    Res.parseLBS_ def lbs @?= doc

caseRetainNamespaces :: Assertion
caseRetainNamespaces = do
    let lbs = "<foo xmlns:bar='baz'><bar:bin/><bin3 xmlns='bin4'></bin3></foo>"
        doc = Res.parseLBS_ def { Res.psRetainNamespaces = True } lbs
    doc `shouldBe` Res.Document
        (Res.Prologue [] Nothing [])
        (Res.Element
            "foo"
            (Map.singleton "xmlns:bar" "baz")
            [ Res.NodeElement $ Res.Element
                "{baz}bin"
                Map.empty
                []
            , Res.NodeElement $ Res.Element
                "{bin4}bin3"
                (Map.singleton "xmlns" "bin4")
                []
            ])
        []

caseIso8859_1 :: Assertion
caseIso8859_1 = do
    let lbs = "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?><foo>\232</foo>"
        doc = Res.parseLBS_ def lbs
    doc `shouldBe` Res.Document
        (Res.Prologue [] Nothing [])
        (Res.Element
            "foo"
            Map.empty
            [Res.NodeContent "\232"])
        []

caseRenderCDATA :: Assertion
caseRenderCDATA = do
    let doc = Res.Document (Res.Prologue [] Nothing [])
                (Res.Element "a" Map.empty
                    [ Res.NodeContent "www.google.com"
                    ])
                []
        withoutCDATA = Res.renderLBS def doc
        withCDATA = Res.renderLBS (def { Res.rsUseCDATA = const True }) doc
    withCDATA `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><a><![CDATA[www.google.com]]></a>"
    withoutCDATA `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><a>www.google.com</a>"

caseEscapesCDATA :: Assertion
caseEscapesCDATA = do
    let doc = Res.Document (Res.Prologue [] Nothing [])
                (Res.Element "a" Map.empty
                    [ Res.NodeContent "]]>"
                    ])
                []
        result = Res.renderLBS (def { Res.rsUseCDATA = const True }) doc
    result `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><a><![CDATA[]]]]><![CDATA[>]]></a>"
