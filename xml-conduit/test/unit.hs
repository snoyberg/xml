{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Control.Exception            (Exception, toException,
                                               fromException)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString.Char8        as S
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Typeable                (Typeable)
import           Data.XML.Types
import           Test.Hspec
import           Test.HUnit                   hiding (Test)
import qualified Text.XML                     as Res
import qualified Text.XML.Cursor              as Cu
import           Text.XML.Stream.Parse        (def)
import qualified Text.XML.Stream.Parse        as P
import qualified Text.XML.Unresolved          as D

import           Control.Monad
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.XML.Cursor              (($.//), ($/), ($//), ($|),
                                               (&.//), (&/), (&//))

import qualified Control.Monad.Trans.Resource as C
import           Data.Conduit                 ((.|), runConduit,
                                               runConduitRes, ConduitT)
import           Data.Conduit.Attoparsec      (ParseError(..))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.List            as CL
import qualified Data.Map                     as Map
import           Text.Blaze                   (toMarkup)
import           Text.Blaze.Renderer.String   (renderMarkup)

main :: IO ()
main = hspec $ do
    describe "XML parsing and rendering" $ do
        it "is idempotent to parse and render a document" documentParseRender
        it "has valid parser combinators" combinators
        context "has working choose function" testChoose
        it "has working many function" testMany
        it "has working many' function" testMany'
        it "has working manyYield function" testManyYield
        it "has working takeContent function" testTakeContent
        it "has working takeTree function" testTakeTree
        it "has working takeAnyTreeContent function" testTakeAnyTreeContent
        it "has working orE" testOrE
        it "is idempotent to parse and pretty render a document" documentParsePrettyRender
        it "ignores the BOM" parseIgnoreBOM
        it "strips duplicated attributes" stripDuplicateAttributes
        it "displays comments" testRenderComments
        it "conduit parser" testConduitParser
        it "can omit the XML declaration" omitXMLDeclaration
        it "doesn't hang on malformed entity declarations" malformedEntityDeclaration
        it "escapes <>'\"& as necessary" caseEscapesAsNecessary
        it "preserves the order of attributes" casePreservesAttrOrder
        context "correctly parses hexadecimal entities" hexEntityParsing
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
        it "understands complex inline with markup" resolvedInlineComplex
        it "can expand inline entities recursively" resolvedInlineRecursive
        it "doesn't explode with an inline entity loop" resolvedInlineLoop
        it "doesn't explode with the billion laughs attack" billionLaughs
        it "allows entity expansion size limit to be adjusted" thousandLaughs
        it "ignores parameter entity declarations" parameterEntity
        it "doesn't break on [] in doctype comments" doctypeComment
        it "skips element declarations in doctype" doctypeElements
        it "skips processing instructions in doctype" doctypePI
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
combinators = runConduitRes $ P.parseLBS def input .| do
    P.force "need hello" $ P.tag' "hello" (P.requireAttr "world") $ \world -> do
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
    it "can choose between text and elements, when the text is encoded, NBSP"
        testChooseElemOrTextIsEncodedNBSP
    it "can choose between elements and text, when the text is whitespace"
        testChooseElemOrTextIsWhiteSpace
    it "can choose between text and elements, when the text is whitespace"
        testChooseTextOrElemIsWhiteSpace
    it "can choose between text and elements, when the whitespace is both literal and encoded"
        testChooseElemOrTextIsChunkedText
    it "can choose between text and elements, when the text is chunked the other way"
        testChooseElemOrTextIsChunkedText2

testChooseElemOrTextIsText :: Assertion
testChooseElemOrTextIsText = runConduitRes $ P.parseLBS def input .| do
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
testChooseElemOrTextIsEncoded = runConduitRes $ P.parseLBS def input .| do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.tagNoAttr "failure" $ return "boom"
            , P.contentMaybe
            ]
        liftIO $ x @?= Just "\x20something\x20"
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "&#x20;something&#x20;"
        , "</hello>"
        ]

testChooseElemOrTextIsEncodedNBSP :: Assertion
testChooseElemOrTextIsEncodedNBSP = runConduitRes $ P.parseLBS def input .| do
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
testChooseElemOrTextIsWhiteSpace = runConduitRes $ P.parseLBS def input .| do
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

testChooseTextOrElemIsWhiteSpace :: Assertion
testChooseTextOrElemIsWhiteSpace = runConduitRes $ P.parseLBS def input .| do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.choose
            [ P.contentMaybe
            , P.tagNoAttr "failure" $ return "boom"
            ]
        liftIO $ x @?= Just "\x20\x20\x20"
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>   </hello>"
        ]

testChooseElemOrTextIsChunkedText :: Assertion
testChooseElemOrTextIsChunkedText = runConduitRes $ P.parseLBS def input .| do
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
testChooseElemOrTextIsChunkedText2 = runConduitRes $ P.parseLBS def input .| do
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
testChooseElemOrTextIsElem = runConduitRes $ P.parseLBS def input .| do
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
testChooseTextOrElemIsText = runConduitRes $ P.parseLBS def input .| do
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
testChooseTextOrElemIsElem = runConduitRes $ P.parseLBS def input .| do
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
testChooseEitherElem = runConduitRes $ P.parseLBS def input .| do
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
    result <- runConduitRes $
        P.parseLBS def input .| helloParser
        .| CL.consume
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

testTakeContent :: Assertion
testTakeContent = do
    result <- runConduitRes $ P.parseLBS def input .| rootParser
    result @?= Just
      [ EventContent (ContentText "Hello world !")
      ]
  where
    rootParser = P.tagNoAttr "root" $ void (P.takeContent >> P.takeContent) .| CL.consume
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<root>"
        , "Hello world !"
        , "</root>"
        ]

testTakeTree :: Assertion
testTakeTree = do
    result <- runConduitRes $ P.parseLBS def input .| rootParser
    result @?=
      [ EventBeginDocument
      , EventBeginDoctype "foo" Nothing
      , EventEndDoctype
      , EventBeginElement "a" []
      , EventBeginElement "em" []
      , EventContent (ContentText "Hello world !")
      , EventEndElement "em"
      , EventEndElement "a"
      ]
  where
    rootParser = void (P.takeTree "a" P.ignoreAttrs) .| CL.consume
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<a>"
        , "<em>Hello world !</em>"
        , "</a>"
        , "<b>"
        , "</b>"
        ]

testTakeAnyTreeContent :: Assertion
testTakeAnyTreeContent = do
    result <- runConduitRes $ P.parseLBS def input .| rootParser
    result @?= Just
      [ EventBeginElement "b" []
      , EventContent (ContentText "Hello ")
      , EventBeginElement "em" []
      , EventContent (ContentText "world")
      , EventEndElement "em"
      , EventContent (ContentText " !")
      , EventEndElement "b"
      ]
  where
    rootParser = P.tagNoAttr "root" $ (P.takeAnyTreeContent >> void P.ignoreAnyTreeContent) .| CL.consume
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<root>"
        , "<b>Hello <em>world</em> !</b> Welcome !"
        , "</root>"
        ]


testMany :: Assertion
testMany = runConduitRes $ P.parseLBS def input .| do
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
testMany' = runConduitRes $ P.parseLBS def input .| do
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
testOrE = runConduitRes $ runConduit $ P.parseLBS def input .| do
    P.force "need hello" $ P.tagNoAttr "hello" $ do
        x <- P.tagNoAttr "failure" (return 1) `P.orE`
             P.tagNoAttr "success" (return 2)
        y <- P.tag' "success" (P.requireAttr "failure") (const $ return 1) `P.orE`
             P.tag' "success" (P.requireAttr "success") (const $ return 2)
        liftIO $ x @?= Just (2 :: Int)
        liftIO $ y @?= Just (2 :: Int)
  where
    input = L.concat
        [ "<?xml version='1.0'?>"
        , "<!DOCTYPE foo []>\n"
        , "<hello>"
        , "<success/>"
        , "<success success=\"0\"/>"
        , "</hello>"
        ]

testConduitParser :: Assertion
testConduitParser = do
    x <-   runConduitRes
         $ P.parseLBS def input
        .| (P.force "need hello" $ P.tagNoAttr "hello" f)
        .| CL.consume
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
    f :: C.MonadThrow m => ConduitT Event Int m ()
    f = do
        ma <- P.tagNoAttr "item" (return 1)
        maybe (return ()) (\a -> C.yield a >> f) ma

omitXMLDeclaration :: Assertion
omitXMLDeclaration = Res.renderLBS settings input @?= spec
  where
    settings = def { Res.rsXMLDeclaration = False }
    input = Res.Document (Prologue [] Nothing [])
              (Res.Element "foo" Map.empty [Res.NodeContent "bar"])
              []
    spec = "<foo>bar</foo>"

malformedEntityDeclaration :: Assertion
malformedEntityDeclaration = do -- missing > after bim
    assertBool "raises ParseError" $
      case Res.parseLBS Res.def "<!DOCTYPE foo [<!ENTITY bim \"Hello\"]><foo></foo>" of
        Left e -> case fromException e of
                    Just (ParseError ["DOCTYPE"] _ _) -> True
                    _ -> False
        _ -> False

caseEscapesAsNecessary :: Assertion
caseEscapesAsNecessary = do
    let doc = Res.Document (Res.Prologue [] Nothing [])
                (Res.Element "a" (Map.fromList [("attr", "'<&val>'")])
                    [Res.NodeContent "'\"<&test]]>\"'"])
                []
        result = Res.renderLBS def doc
    result `shouldBe` "<?xml version=\"1.0\" encoding=\"UTF-8\"?><a attr=\"'&lt;&amp;val>'\">'\"&lt;&amp;test]]&gt;\"'</a>"

casePreservesAttrOrder :: Assertion
casePreservesAttrOrder = do
    let doc = Document (Prologue [] Nothing [])
                (Element "doc" [] [
                  NodeElement (Element "el" [("attr1", [ContentText "1"]), ("attr2", [ContentText "2"])] []),
                  NodeElement (Element "el" [("attr2", [ContentText "2"]), ("attr1", [ContentText "1"])] [])
                ])
                []
        rendered = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><doc><el attr1=\"1\" attr2=\"2\"/><el attr2=\"2\" attr1=\"1\"/></doc>"
        renderResult = D.renderLBS def doc
        parseResult = D.parseLBS def rendered
    renderResult `shouldBe` rendered
    parseResult `shouldSatisfy` either (const False) (doc==)

hexEntityParsing :: Spec
hexEntityParsing = do
  it "rejects leading 0x" $
    go "<foo>&#x0xff;</foo>" @?= Nothing
  it "rejects leading 0X" $
    go "<foo>&#x0Xff;</foo>" @?= Nothing
  it "accepts lowercase hex digits" $
    go "<foo>&#xff;</foo>" @?= Just (spec "\xff")
  it "accepts uppercase hex digits" $
    go "<foo>&#xFF;</foo>" @?= Just (spec "\xff")
  --Note: this must be rejected, because, according to the XML spec, a
  --legal EntityRef's entity matches Name, which can't start with a
  --hash.
  it "rejects trailing junk" $
    go "<foo>&#xffhello;</foo>" @?= Nothing
  --Some of these next tests are XML 1.0 specific (i.e., they would
  --differ for XML 1.1), but approximately no-one uses XML 1.1.
  it "rejects illegal character #x0" $
    go "<foo>&#x0;</foo>" @?= Nothing
  it "rejects illegal character #xFFFE" $
    go "<foo>&#xFFFE;</foo>" @?= Nothing
  it "rejects illegal character #xFFFF" $
    go "<foo>&#xFFFF;</foo>" @?= Nothing
  it "rejects illegal character #xD900" $
    go "<foo>&#xD900;</foo>" @?= Nothing
  it "rejects illegal character #xC" $
    go "<foo>&#xC;</foo>" @?= Nothing
  it "rejects illegal character #x1F" $
    go "<foo>&#x1F;</foo>" @?= Nothing
  it "accepts astral plane character" $
    go "<foo>&#x1006ff;</foo>" @?= Just (spec "\x1006ff")
  it "accepts custom character references" $
    go' customSettings "<foo>&#xC;</foo>" @?= Just (spec "\xff")
  where
    spec content = Document (Prologue [] Nothing [])
                    (Element "foo" [] [NodeContent (ContentText content)])
                    []

    go = either (const Nothing) Just . D.parseLBS def
    go' settings = either (const Nothing) Just . D.parseLBS settings
    customSettings = def { P.psDecodeIllegalCharacters = customDecoder }
    customDecoder 12 = Just '\xff'
    customDecoder _  = Nothing

name :: [Cu.Cursor] -> [Text]
name [] = []
name (c:cs) = ($ name cs) $ case Cu.node c of
                              Res.NodeElement e -> ((Res.nameLocalName $ Res.elementName e) :)
                              _                 -> id

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
          f _                   = False
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

resolvedInlineComplex :: Assertion
resolvedInlineComplex = do
    Res.Document _ root _ <- return $ Res.parseLBS_ Res.def "<!DOCTYPE foo [<!ENTITY bar \"<p>baz &bim;</p>\"><!ENTITY bim \"Hello\">]><foo>&bar;</foo>"
    root @?= Res.Element "foo" Map.empty [Res.NodeElement (Res.Element "p" Map.empty [Res.NodeContent "baz Hello"])]
    Res.Document _ root2 _ <- return $ Res.parseLBS_ Res.def "<!DOCTYPE foo [<!ENTITY bar \"<p>baz</p>\">]><foo class=\"&bar;\"/>"
    root2 @?= Res.Element "foo" (Map.fromList [("class","baz")]) []


resolvedInlineRecursive :: Assertion
resolvedInlineRecursive = do
    Res.Document _ root _ <- return $ Res.parseLBS_ Res.def
      "<!DOCTYPE foo [<!ENTITY bim \"baz\"><!ENTITY bar \"&bim;&#73;&amp;\">]><foo>&bar;</foo>"
    root @?= Res.Element "foo" Map.empty [Res.NodeContent "bazI&"]

resolvedInlineLoop :: Assertion
resolvedInlineLoop = do
    res <- return $ Res.parseLBS Res.def
           "<!DOCTYPE foo [<!ENTITY bim \"&bim;\">]><foo>&bim;</foo>"
    Left (toException $ Res.UnresolvedEntityException (Set.fromList ["bim"]))
      `showEq` res
    res2 <- return $ Res.parseLBS Res.def
           "<!DOCTYPE foo [<!ENTITY bim \"&bim;\">]><foo class=\"&bim;\"/>"
    Left (toException $ Res.UnresolvedEntityException (Set.fromList ["bim"]))
      `showEq` res2

billionLaughs :: Assertion
billionLaughs = do
    res <- return $ Res.parseLBS Res.def
      "<?xml version=\"1.0\"?><!DOCTYPE lolz [<!ENTITY lol \"lol\"><!ELEMENT lolz (#PCDATA)><!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\"><!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\"><!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\"><!ENTITY lol4 \"&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;\"><!ENTITY lol5 \"&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;\"><!ENTITY lol6 \"&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;\"><!ENTITY lol7 \"&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;\"><!ENTITY lol8 \"&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;\"><!ENTITY lol9 \"&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;\">]><lolz>&lol9;</lolz>"
    Left (toException $ Res.UnresolvedEntityException (Set.fromList ["lol9"]))
      `showEq` res

thousandLaughs :: Assertion
thousandLaughs = do
    res <- return $ Res.parseLBS Res.def{ P.psEntityExpansionSizeLimit = 2999 }
      "<?xml version=\"1.0\"?><!DOCTYPE lolz [<!ENTITY lol \"lol\"><!ELEMENT lolz (#PCDATA)><!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\"><!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\"><!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\">]><lolz>&lol3;</lolz>"
    Left (toException $ Res.UnresolvedEntityException (Set.fromList ["lol3"]))
      `showEq` res
    -- Raise the entity expansion limit and it should work:
    Right (Res.Document {Res.documentRoot = Res.Element{ Res.elementNodes = [Res.NodeContent t] }}) <- return $ Res.parseLBS Res.def{ P.psEntityExpansionSizeLimit = 3001 } "<?xml version=\"1.0\"?><!DOCTYPE lolz [<!ENTITY lol \"lol\"><!ELEMENT lolz (#PCDATA)><!ENTITY lol1 \"&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;\"><!ENTITY lol2 \"&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;\"><!ENTITY lol3 \"&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;\">]><lolz>&lol3;</lolz>"
    t @?= T.replicate 1000 "lol"

parameterEntity :: Assertion
parameterEntity = do
    let res = Res.parseLBS Res.def "<!DOCTYPE foo [<!ENTITY % bim \"Hello\">]><foo>&bim;</foo>"
    Left (toException $ Res.UnresolvedEntityException (Set.fromList ["bim"]))
      `showEq` res

doctypeComment :: Assertion
doctypeComment = do
    Res.Document _ root _ <- return $ Res.parseLBS_
       Res.def "<!DOCTYPE foo [<!-- [comment] --> <!ENTITY bar \"baz\">]><foo>&bar;</foo>"
    root @?= Res.Element "foo" Map.empty [Res.NodeContent "baz"]

doctypeElements :: Assertion
doctypeElements = do
    Res.Document _ root _ <- return $ Res.parseLBS_
       Res.def "<!DOCTYPE foo [<!ELEMENT assessment (#PCDATA)>\n<!ELEMENT textbooks(author,title)>\n<!ATTLIST assessment assessment_type (exam | assignment) #IMPLIED>\n<!ENTITY bar \"baz\">]><foo>&bar;</foo>"
    root @?= Res.Element "foo" Map.empty [Res.NodeContent "baz"]

doctypePI :: Assertion
doctypePI = do
    Res.Document _ root _ <- return $ Res.parseLBS_
       Res.def "<!DOCTYPE foo [<?foobar \"[baz]\"?><!ENTITY bar \"baz\">]><foo>&bar;</foo>"
    root @?= Res.Element "foo" Map.empty [Res.NodeContent "baz"]

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
        rs = def { Res.rsAttrOrder = \name' m ->
                        case name' of
                            "foo" -> reverse $ Map.toAscList m
                            _     -> Map.toAscList m
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