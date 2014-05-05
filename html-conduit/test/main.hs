{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (Test)
import Test.Hspec
import Data.ByteString.Lazy.Char8 ()
import qualified Text.HTML.DOM as H
import qualified Text.XML as X
import qualified Data.Map as Map

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
        it "multiple root elements" $
            X.parseLBS_ X.def "<html><foo><bar>baz&amp;foobar;</bar></foo><foo/></html>" @=?
            H.parseLBS        "<foo><bar>baz&foobar;</foo><foo>"
        it "doesn't strip whitespace" $
            X.parseLBS_ X.def "<foo>  hello</foo>" @=?
            H.parseLBS        "<foo>  hello</foo>"
    describe "HTML parsing" $ do
        it "XHTML" $
            let html = "<html><head><title>foo</title></head><body><p>Hello World</p></body></html>"
                doc = X.Document (X.Prologue [] Nothing []) root []
                root = X.Element "html" Map.empty
                    [ X.NodeElement $ X.Element "head" Map.empty
                        [ X.NodeElement $ X.Element "title" Map.empty
                            [X.NodeContent "foo"]
                        ]
                    , X.NodeElement $ X.Element "body" Map.empty
                        [ X.NodeElement $ X.Element "p" Map.empty
                            [X.NodeContent "Hello World"]
                        ]
                    ]
             in H.parseLBS html @?= doc
        it "XHTML with doctype and <?xml #30" $ do
            let html = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n<html><head><title>foo</title></head><body><p>Hello World</p></body></html>"
                doc = X.Document (X.Prologue [] Nothing []) root []
                root = X.Element "html" Map.empty
                    [ X.NodeElement $ X.Element "head" Map.empty
                        [ X.NodeElement $ X.Element "title" Map.empty
                            [X.NodeContent "foo"]
                        ]
                    , X.NodeElement $ X.Element "body" Map.empty
                        [ X.NodeElement $ X.Element "p" Map.empty
                            [X.NodeContent "Hello World"]
                        ]
                    ]
             in H.parseLBS html @?= doc
        it "HTML" $
            let html = "<html><head><title>foo</title></head><body><br><p>Hello World</p></body></html>"
                doc = X.Document (X.Prologue [] Nothing []) root []
                root = X.Element "html" Map.empty
                    [ X.NodeElement $ X.Element "head" Map.empty
                        [ X.NodeElement $ X.Element "title" Map.empty
                            [X.NodeContent "foo"]
                        ]
                    , X.NodeElement $ X.Element "body" Map.empty
                        [ X.NodeElement $ X.Element "br" Map.empty []
                        , X.NodeElement $ X.Element "p" Map.empty
                            [X.NodeContent "Hello World"]
                        ]
                    ]
             in H.parseLBS html @?= doc
