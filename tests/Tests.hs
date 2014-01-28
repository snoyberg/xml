{-# LANGUAGE FlexibleInstances, OverloadedStrings, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Applicative

import           Data.Monoid (Monoid(..),(<>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.HUnit hiding (Test)
import           Test.QuickCheck

import           Text.HTML.TagStream
import qualified Text.HTML.TagStream.ByteString as S
import qualified Text.HTML.TagStream.Text as T

main :: IO ()
main = hspec $ do
    describe "[ByteString] Property" $ do
         prop "Text nodes can't be empty" propTextNotEmpty
         prop "Parse results can't empty" propResultNotEmpty
    describe "[Text] Property" $ do
         prop "Text nodes can't be empty" propTextNotEmptyText
         prop "Parse results can't empty" propResultNotEmptyText
    describe "[ByteString]One pass parse" onePassTests
    describe "[ByteString]Streamline parse" streamlineTests
    describe "[Text]One pass parse" onePassTestsText
    describe "[Text]Streamline parse" streamlineTestsText

propTextNotEmpty :: ByteString -> Bool
propTextNotEmpty = either (const False) text_not_empty . S.decode
  where text_not_empty = all not_empty
        not_empty (Text s) = S.length s > 0
        not_empty _ = True

propResultNotEmpty :: ByteString -> Bool
propResultNotEmpty s = either (const False) not_empty . S.decode $ s
  where not_empty tokens = (S.null s && null tokens)
                        || (not (S.null s) && not (null tokens))

propTextNotEmptyText :: Text -> Bool
propTextNotEmptyText = either (const False) text_not_empty . T.decode
  where text_not_empty = all not_empty
        not_empty (Text s) = not (T.null s)
        not_empty _ = True

propResultNotEmptyText :: Text -> Bool
propResultNotEmptyText s = either (const False) not_empty . T.decode $ s
  where not_empty tokens = (T.null s && null tokens)
                        || (not (T.null s) && not (null tokens))

encodeTokenUtf8 :: Token' Text -> Token' ByteString
encodeTokenUtf8 = fmap T.encodeUtf8

onePassTests :: Spec
onePassTests = mapM_ one testcases
  where
    one (T.encodeUtf8 -> str, map encodeTokenUtf8 -> tokens) =
      it (S.unpack str) $ do
        result <- combineText <$> assertDecodeBS str
        assertEqual "one-pass parse result incorrect" tokens result

onePassTestsText :: Spec
onePassTestsText = mapM_ one testcases
  where
    one (str, tokens) = it (T.unpack str) $ do
        result <- combineText <$> assertDecodeText str
        assertEqual "one-pass parse result incorrect" tokens result

streamlineTests :: Spec
streamlineTests = mapM_ one testcases
  where
    isIncomplete (Incomplete _) = True
    isIncomplete _ = False
    one (T.encodeUtf8 -> str, map encodeTokenUtf8 -> tokens) =
      it (S.unpack str) $ do
        -- streamline parse result don't contain the trailing Incomplete token.
        let tokens' = reverse . dropWhile isIncomplete  . reverse $ tokens
        result <- combineText <$> C.runResourceT (
                      CL.sourceList (map S.singleton (S.unpack str))
                      C.$= S.tokenStream
                      C.$$ CL.consume )
        assertEqual "streamline parse result incorrect" tokens' result

streamlineTestsText :: Spec
streamlineTestsText = mapM_ one testcases
  where
    isIncomplete (Incomplete _) = True
    isIncomplete _ = False
    one (T.encodeUtf8 -> str, tokens) =
      it (S.unpack str) $ do
        -- streamline parse result don't contain the trailing Incomplete token.
        let tokens' = reverse . dropWhile isIncomplete  . reverse $ tokens
        result <- combineText <$> C.runResourceT (
                      CL.sourceList (map S.singleton (S.unpack str))
                      C.$= T.tokenStreamBS
                      C.$$ CL.consume )
        assertEqual "streamline parse result incorrect" tokens' result

testcases :: [(Text, [Token' Text])]
testcases =
  -- attributes {{{
  [ ( "<span readonly title=foo class=\"foo bar\" style='display:none;'>"
    , [TagOpen "span" [("readonly", ""), ("title", "foo"), ("class", "foo bar"), ("style", "display:none;")] False]
    )
  , ( "<span a = b = c = d>"
    , [TagOpen "span" [("a", "b"), ("=", ""), ("c", "d")] False]
    )
  , ( "<span a = b = c>"
    , [TagOpen "span" [("a", "b"), ("=", ""), ("c", "")] False]
    )
  , ( "<span /foo=bar>"
    , [TagOpen "span" [("/foo", "bar")] False]
    )
  -- }}}
  -- quoted string and escaping {{{
  , ( "<span \"<p>xx \\\"'\\\\</p>\"=\"<p>xx \\\"'\\\\</p>\">"
    , [TagOpen "span" [("<p>xx \"'\\</p>", "<p>xx \"'\\</p>")] False]
    )
  , ( "<span '<p>xx \\\"\\'\\\\</p>'='<p>xx \\\"\\'\\\\</p>'>"
    , [TagOpen "span" [("<p>xx \"'\\</p>", "<p>xx \"'\\</p>")] False]
    )
  -- }}}
  -- attribute and tag end {{{
  , ( "<br/>"
    , [TagOpen "br" [] True]
    )
  , ( "<img src=http://foo.bar.com/foo.jpg />"
    , [TagOpen "img" [("src", "http://foo.bar.com/foo.jpg")] True]
    )
  , ( "<span foo>"
    , [TagOpen "span" [("foo", "")] False]
    )
  , ( "<span foo/>"
    , [TagOpen "span" [("foo", "")] True]
    )
  , ( "<span foo=/>"
    , [TagOpen "span" [("foo", "/")] False]
    )
  -- }}}
  -- normal tag {{{
  , ( "<p>text</p>"
    , [TagOpen "p" [] False, Text "text", TagClose "p"]
    )
  , ( "<>"
    , [TagOpen "" [] False]
    )
  , ( "<a\ttitle\n=\r\"foo bar\" alt=\n/\r\t>"
    , [TagOpen "a" [("title", "foo bar"), ("alt", "/")] False]
    )
  -- }}}
  -- comment tag {{{
  , ( "<!--foo-->"
    , [Comment "foo"] )
  , ( "<!--f--oo->-->"
    , [Comment "f--oo->"] )
  , ( "<!--foo-->bar-->"
    , [Comment "foo", Text "bar-->"]
    )
  -- }}}
  -- special tag {{{
  , ( "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\">"
    , [Special "DOCTYPE" "html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\""]
    )
  , ( "<!DOCTYPE html>"
    , [Special "DOCTYPE" "html"]
    )
  -- }}}
  -- close tag {{{
  , ( "</\r\t\nbr>"
    , [TagClose "\r\t\nbr"]
    )
  , ( "</br/>"
    , [TagClose "br/"]
    )
  , ( "</>"
    , [TagClose ""]
    )
  -- }}}
  -- incomplete test {{{
  -- }}}
  -- script tag TODO{{{
  , ( "<script></script>"
    , [TagOpen "script" [] False, TagClose "script"]
    )
  , ( "<script>var x=\"</script>"
    , [TagOpen "script" [] False, Text "var x=\"", TagClose "script"]
    )
  --, ( "<script>var x=\"</script>\";</script>"
  --  , [TagOpen "script" [] False, Text "var x=\"</script>\";", TagClose "script"]
  --  )
  , ( "<script>// '\r\n</script>"
    , [TagOpen "script" [] False, Text "// '\r\n", TagClose "script"]
    )
  -- }}}
  -- issue 10 https://github.com/yihuang/tagstream-conduit/issues/10
  , ( "<foo>  hello</foo>"
    , [TagOpen "foo" [] False, Text "  hello", TagClose "foo"]
    )

  -- Text entity decoding
  , text "" ""
  , text "&" "&"
  , text "& hello" " hello"
  , text "&amp" "&amp"
  , text "&amp;" "&"
  , text "&quot;" "\""
  , text "&unknown;" ""
  , text "foo &bar; mu" "foo  mu"
  , text "&lt;p&gt;" "<p>"
  , text "&#60;" "<"
  , text "a&#97;a" "aaa"
  , text "foo &" "foo &"
  , text "foo &amp" "foo &amp"
  , text "foo &amp;" "foo &"
  ]
  where text b a = ("<p>" <> b <> "</p>"
                   ,concat [[TagOpen "p" [] False],[Text a | not (T.null a)],[TagClose "p"]])

testChar :: Gen Char
testChar = growingElements "<>/=\"' \t\r\nabcde\\"
testString :: Gen String
testString = listOf testChar
testBS :: Gen ByteString
testBS = S.pack <$> testString
testText :: Gen Text
testText = T.pack <$> testString

instance Arbitrary ByteString where
    arbitrary = testBS
instance Arbitrary Text where
    arbitrary = testText

assertEither :: Either String a -> Assertion
assertEither = either (assertFailure . ("Left:"++)) (const $ return ())

assertDecodeBS :: ByteString -> IO [S.Token]
assertDecodeBS s = do
    let result = S.decode s
    assertEither result
    let (Right tokens) = result
    return tokens

assertDecodeText :: Text -> IO [T.Token]
assertDecodeText s = do
    let result = T.decode s
    assertEither result
    let (Right tokens) = result
    return tokens

combineText :: Monoid s => [Token' s] -> [Token' s]
combineText = go
  where go [] = []
        go (Text t1 : Text t2 : xs) = go $ Text (mappend t1 t2) : xs
        go (x:xs) = x : go xs
