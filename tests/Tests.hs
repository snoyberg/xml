{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Applicative
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck (prop)
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Text.HTML.TagStream

main :: IO ()
main = hspecX $ do
    describe "Property" $ do
         prop "Text nodes can't be empty" propTextNotEmpty
         prop "Parse results can't empty" propResultNotEmpty
    describe "One pass parse" onePassTests
    describe "Streamline parse" streamlineTests

propTextNotEmpty :: ByteString -> Bool
propTextNotEmpty = either (const False) text_not_empty . decode
  where text_not_empty = all not_empty
        not_empty (Text s) = S.length s > 0
        not_empty _ = True

propResultNotEmpty :: ByteString -> Bool
propResultNotEmpty s = either (const False) not_empty . decode $ s
  where not_empty tokens = (S.null s && null tokens)
                        || (not (S.null s) && not (null tokens))

onePassTests :: Specs
onePassTests = mapM_ one testcases
  where
    one (str, tokens) = it (S.unpack str) $ do
        result <- combineText <$> assertDecode str
        assertEqual "one-pass parse result incorrect" tokens result

streamlineTests :: Specs
streamlineTests = mapM_ one testcases
  where
    isIncomplete (Incomplete _) = True
    isIncomplete _ = False
    one (str, tokens) = it (S.unpack str) $ do
        -- streamline parse result don't contain the trailing Incomplete token.
        let tokens' = reverse . dropWhile isIncomplete  . reverse $ tokens
        result <- combineText <$> C.runResourceT (
                      CL.sourceList (map S.singleton (S.unpack str))
                      C.$= tokenStream
                      C.$$ CL.consume )
        assertEqual "streamline parse result incorrect" tokens' result

testcases :: [(ByteString, [Token])]
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
  ]

testChar :: Gen Char
testChar = growingElements "<>/=\"' \t\r\nabcde\\"
testString :: Gen String
testString = listOf testChar
testBS :: Gen ByteString
testBS = S.pack <$> testString

instance Arbitrary ByteString where
    arbitrary = testBS

assertEither :: Either String a -> Assertion
assertEither = either (assertFailure . ("Left:"++)) (const $ return ())

assertDecode :: ByteString -> IO [Token]
assertDecode s = do
    let result = decode s
    assertEither result
    let (Right tokens) = result
    return tokens

combineText :: [Token] -> [Token]
combineText [] = []
combineText (Text t1 : Text t2 : xs) = combineText $ Text (S.append t1 t2) : xs
combineText (x:xs) = x : combineText xs
