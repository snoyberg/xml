{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit
import Text.Hamlet.XML (xml)
import Text.XML.XPath (xp)
import Text.XML.Enumerator.Cursor (fromNode, ($/), (&/), element, node)
import qualified Text.XML.Enumerator.Resolved as X
import Test.HUnit

cursor = fromNode $ X.NodeElement $ X.Element "html" [] [xml|
<head>
    <title>Foo bar baz
    <link rel=stylesheet href=foo.css>
    <link rel=next href=bar.html>
<body>
    <h1>Title
    <div>
        <h2>Test
|]

titleC = head $ cursor $/ element "head" &/ element "title"
titleE = X.Element "title" [] [X.NodeContent "Foo bar baz"]

main = hspecX $ describe "XPath" $ do
    it "parses text, root" $ ["Foo bar baz"] @=?
        [xp|/head/title//text()|] titleC
    it "parses text, context" $ ["Foo bar baz"] @=?
        [xp|head//text()|] cursor
    it "parses elements" $ [titleE] @=?
        ([xp|descendant::title|] cursor)
    it "initial double slash" $ [X.Element "h2" [] [X.NodeContent "Test"]] @=?
        ([xp|//h2|] cursor)
    it "attributes" $ ["foo.css"] @=?
        [xp|/head/link[@rel = 'stylesheet']/@href|] cursor
