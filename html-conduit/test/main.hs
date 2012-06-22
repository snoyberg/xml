{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (Test)
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Data.ByteString.Lazy.Char8 ()
import qualified Text.HTML.DOM as H
import qualified Text.XML as X

main :: IO ()
main = hspec $ do
    describe "parses" $ do
        it "well-formed document" $
            X.parseLBS_ X.def "<foo><bar>baz</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz</bar></foo>"
        it "adds missing close tags" $
            X.parseLBS_ X.def "<foo><bar>baz</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz</foo>"
        it "void tags" $
            X.parseLBS_ X.def "<foo><bar><img/>foo</bar></foo>" @=?
            H.parseLBS        "<foo><bar><img>foo</foo>"
        it "xml entities" $
            X.parseLBS_ X.def "<foo><bar>baz&gt;</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz&gt;</foo>"
        it "html entities" $
            X.parseLBS_ X.def "<foo><bar>baz&#160;</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz&nbsp;</foo>"
        it "decimal entities" $
            X.parseLBS_ X.def "<foo><bar>baz&#160;</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz&#160;</foo>"
        it "hex entities" $
            X.parseLBS_ X.def "<foo><bar>baz&#x160;</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz&#x160;</foo>"
        it "invalid entities" $
            X.parseLBS_ X.def "<foo><bar>baz&amp;foobar;</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz&foobar;</foo>"
