{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.Hamlet.XML
import qualified Text.XML as X
import Test.HUnit
import Test.Hspec
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
  describe "xml-hamlet" $ do
    it "handles plain tags" $ [xml|
<foo>
        <baz>
|] @?=
        [ X.NodeElement $ X.Element "foo" Map.empty
            [ X.NodeElement $ X.Element "baz" Map.empty []
            ]
        ]
    it "handles raw text" $ [xml|
<foo>
        <baz>bin
|] @?=
        [ X.NodeElement $ X.Element "foo" Map.empty
            [ X.NodeElement $ X.Element "baz" Map.empty
                [ X.NodeContent "bin"
                ]
            ]
        ]
    it "handles variables" $ [xml|
<foo>
        <baz>#{bin}
|] @?=
        [ X.NodeElement $ X.Element "foo" Map.empty
            [ X.NodeElement $ X.Element "baz" Map.empty
                [ X.NodeContent "bin"
                ]
            ]
        ]
    it "handles embed" $ [xml|
<foo>
        <baz>^{nodes}
|] @?=
        [ X.NodeElement $ X.Element "foo" Map.empty
            [ X.NodeElement $ X.Element "baz" Map.empty nodes
            ]
        ]
    it "handles attributes" $ [xml|
<foo>
    <bar here=there>
        <baz>
|] @?=
        [ X.NodeElement $ X.Element "foo" Map.empty
            [ X.NodeElement $ X.Element "bar" (Map.singleton "here" "there")
                [ X.NodeElement $ X.Element "baz" Map.empty []
                ]
            ]
        ]
    it "handles attributes" $ [xml|
<foo>
    <bar here=there>
        <baz :False:false=false :True:true=#{true} *{attrs}>
|] @?=
        [ X.NodeElement $ X.Element "foo" Map.empty
            [ X.NodeElement $ X.Element "bar" (Map.singleton "here" "there")
                [ X.NodeElement $ X.Element "baz" (Map.fromList (("true", "true") : attrs)) []
                ]
            ]
        ]
    it "handles forall" $ [xml|
$forall x <- xs
    <word>#{x}
    |] @?=
        [ X.NodeElement $ X.Element "word" Map.empty [X.NodeContent "foo"]
        , X.NodeElement $ X.Element "word" Map.empty [X.NodeContent "bar"]
        , X.NodeElement $ X.Element "word" Map.empty [X.NodeContent "baz"]
        ]
    it "handles with" $ [xml|
$with ys <- xs
    $forall x <- ys
        <word>#{x}
    |] @?=
        [ X.NodeElement $ X.Element "word" Map.empty [X.NodeContent "foo"]
        , X.NodeElement $ X.Element "word" Map.empty [X.NodeContent "bar"]
        , X.NodeElement $ X.Element "word" Map.empty [X.NodeContent "baz"]
        ]
    it "handles maybe" $ [xml|
$maybe _x <- Just five
    <one>
$nothing
    <two>
$maybe _x <- Nothing
    <three>
$nothing
    <four>
    |] @?=
        [ X.NodeElement $ X.Element "one" Map.empty []
        , X.NodeElement $ X.Element "four" Map.empty []
        ]
    it "handles conditionals" $ [xml|
$if True
    <one>
$else
    <two>

$if False
    <three>
$elseif True
    <four>

$if False
    <five>
$elseif False
    <six>
$else
    <seven>
|] @?=
        [ X.NodeElement $ X.Element "one" Map.empty []
        , X.NodeElement $ X.Element "four" Map.empty []
        , X.NodeElement $ X.Element "seven" Map.empty []
        ]
    it "case on Maybe" $
      let nothing  = Nothing
          justTrue = Just True
      in [xml|
$case nothing
    $of Just _val
    $of Nothing
        <one>
$case justTrue
    $of Just val
        $if val
            <two>
    $of Nothing
$case (Just $ not False)
    $of Nothing
    $of Just val
        $if val
            <three>
$case Nothing
    $of Just _val
    $of _
        <four>
|] @?=
        [ X.NodeElement $ X.Element "one" Map.empty []
        , X.NodeElement $ X.Element "two" Map.empty []
        , X.NodeElement $ X.Element "three" Map.empty []
        , X.NodeElement $ X.Element "four" Map.empty []
        ]
    it "recognizes clark notation" $ [xml|
<{foo}bar {baz}bin="x">
|] @?= [X.NodeElement $ X.Element "{foo}bar" (Map.singleton "{baz}bin" "x") []]
    it "recognizes clark with URLs" $ [xml|
<{http://www.example.com/foo/bar}baz>
|] @?= [X.NodeElement $ X.Element "{http://www.example.com/foo/bar}baz" Map.empty []]
    it "allow embedding comments" $[xml|^{comment}|] @?= comment
    it "multiline tags" $ [xml|
<foo bar=baz
     bin=bin>content
|] @?= [xml|<foo bar=baz bin=bin>content|]
    it "short circuiting of attributes" $ [xml|<foo :False:x=#{undefined}>|] @?= [xml|<foo>|]
    it "Hash in attribute value" $ [xml|<a href=#>|] @?= [xml|<a href="#">|]
  where
    bin = "bin"
    nodes = [X.NodeInstruction $ X.Instruction "ifoo" "ibar"]
    true = "true"
    attrs = [("one","a"), ("two","b")]
    xs = ["foo", "bar", "baz"]
    comment = [X.NodeComment "somecomment"]

five :: Int
five = 5
