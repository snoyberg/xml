{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import Text.Blaze (toHtml)
import Text.Blaze.Renderer.String (renderHtml)
import Text.XML
import Text.Hamlet.XML
import Text.XML.Xml2Html ()

main :: IO ()
main = putStr $ renderHtml $ toHtml $ Document (Prologue [] Nothing []) root []

root :: Element
root = Element "html" [] [xml|
<head>
    <title>Test
    <script>if (5 < 6 || 8 > 9) alert("Hello World!");
    <{http://www.snoyman.com/xml2html}ie-cond cond="lt IE 7">
        <link href="ie6.css">
    <style>body > h1 { color: red }
<body>
    <h1>Hello World!
|]
