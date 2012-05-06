{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (Test)
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Data.ByteString.Lazy.Char8 ()
import qualified Text.HTML.DOM as H
import qualified Text.XML as X

main :: IO ()
main = hspecX $ do
    describe "parses" $ do
        it "well-formed document" $
            X.parseLBS_ X.def "<foo><bar>baz</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz</bar></foo>"
        it "adds missing close tags" $
            X.parseLBS_ X.def "<foo><bar>baz</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz</foo>"
            {- Need to fix in tagstream-conduit
        it "entities" $
            X.parseLBS_ X.def "<foo><bar>baz&#160;</bar></foo>" @=?
            H.parseLBS        "<foo><bar>baz&nbsp;</foo>"
            -}
        it "void tags" $
            X.parseLBS_ X.def "<foo><bar><img/>foo</bar></foo>" @=?
            H.parseLBS        "<foo><bar><img>foo</foo>"
