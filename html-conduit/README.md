This package uses tagstream-conduit for its parser. It automatically balances
mismatched tags, so that there shouldn't be any parse failures. It does not
handle a full HTML document rendering, such as adding missing html and head
tags. Note that, since version 1.3.1, it uses an inlined copy of
tagstream-conduit with entity decoding bugfixes applied.

Simple usage example:

```haskell
#!/usr/bin/env stack
{- stack --install-ghc --resolver lts-6.23 runghc
   --package http-conduit --package html-conduit
-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO        as T
import           Network.HTTP.Simple (httpSink)
import           Text.HTML.DOM       (sinkDoc)
import           Text.XML.Cursor     (attributeIs, content, element,
                                      fromDocument, ($//), (&/), (&//))

main :: IO ()
main = do
    doc <- httpSink "http://www.yesodweb.com/book" $ const sinkDoc
    let cursor = fromDocument doc
    T.putStrLn "Chapters in the Yesod book:\n"
    mapM_ T.putStrLn
      $ cursor
      $// attributeIs "class" "main-listing"
      &// element "li"
      &/ element "a"
      &/ content
```
