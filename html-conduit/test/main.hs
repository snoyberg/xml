{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit hiding (Test)
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.ByteString.Lazy.Char8 ()
import qualified Text.HTML.DOM as H
import qualified Text.XML as X
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Exception (evaluate)
import Control.DeepSeq (($!!))
import Control.Monad (void)

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
        it "split code-points" $
            X.parseLBS_ X.def "<foo>&#xa0;</foo>" @=?
            H.parseBSChunks ["<foo>\xc2", "\xa0</foo>"]
        it "latin1 codes" $
            X.parseText_ X.def "<foo>\232</foo>" @=?
            H.parseSTChunks ["<foo>\232</foo>"]
        it "latin1 codes strict vs lazy" $
            H.parseLT "<foo>\232</foo>" @=?
            H.parseSTChunks ["<foo>\232</foo>"]
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

    it "doesn't double unescape" $
        let html = "<p>Hello &amp;gt; World</p>"
            doc = X.Document (X.Prologue [] Nothing []) root []
            root = X.Element "p" Map.empty
                [ X.NodeContent "Hello &gt; World"
                ]
         in H.parseLBS html @?= doc

    it "handles entities in attributes" $
        let html = "<br title=\"Mac &amp; Cheese\">"
            doc = X.Document (X.Prologue [] Nothing []) root []
            root = X.Element "br" (Map.singleton "title" "Mac & Cheese") []
         in H.parseLBS html @?= doc

    it "doesn't double escape entities in attributes" $
        let html = "<br title=\"Mac &amp;amp; Cheese\">"
            doc = X.Document (X.Prologue [] Nothing []) root []
            root = X.Element "br" (Map.singleton "title" "Mac &amp; Cheese") []
         in H.parseLBS html @?= doc

    describe "script tags" $ do
      it "ignores funny characters" $
        let html = "<script>hello <> world</script>"
            doc = X.Document (X.Prologue [] Nothing []) root []
            root = X.Element "script" Map.empty [X.NodeContent "hello <> world"]
         in H.parseLBS html @?= doc

      {-

       Would be nice... doesn't work with tagstream-conduit original
       code. Not even sure if the HTML5 parser spec discusses this
       case.

      it "ignores </script> inside string" $
        let html = "<script>hello \"</script>\" world</script>"
            doc = X.Document (X.Prologue [] Nothing []) root []
            root = X.Element "script" Map.empty [X.NodeContent "hello \"</script>\" world"]
         in H.parseLBS html @?= doc

      -}

      it "unterminated" $
        let html = "<script>hello > world"
            doc = X.Document (X.Prologue [] Nothing []) root []
            root = X.Element "script" Map.empty [X.NodeContent "hello > world"]
         in H.parseLBS html @?= doc

      it "entities" $
        let html = "<script>hello &amp; world"
            doc = X.Document (X.Prologue [] Nothing []) root []
            root = X.Element "script" Map.empty [X.NodeContent "hello &amp; world"]
         in H.parseLBS html @?= doc

    prop "parses all random input" $ \strs -> void $ evaluate $!! H.parseSTChunks $ map T.pack strs

    describe "#128 entities cut off" $ do
      it "reported issue" $ do
        let html = "<a href=\"https://example.com?a=b&#038;c=d\">link</a>"
            doc = X.Document (X.Prologue [] Nothing []) root []
            root = X.Element
              "a"
              (Map.singleton "href" "https://example.com?a=b&c=d")
              [X.NodeContent "link"]
         in H.parseLBS html @?= doc

      it "from test suite" $ do
        let html = "<a class=\"u-url\" href=\"https://secure.gravatar.com/avatar/947b5f3f323da0ef785b6f02d9c265d6?s=96&#038;d=blank&#038;r=g\">link</a>"
            doc = X.Document (X.Prologue [] Nothing []) root []
            root = X.Element
              "a"
              (Map.fromList
                  [ ("href", "https://secure.gravatar.com/avatar/947b5f3f323da0ef785b6f02d9c265d6?s=96&d=blank&r=g")
                  , ("class", "u-url")
                  ])
              [X.NodeContent "link"]
         in H.parseLBS html @?= doc
