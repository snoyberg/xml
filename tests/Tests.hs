{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Main where

--import Debug.Trace
import Control.Applicative
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Text.HTML.TagStream

main :: IO ()
main = defaultMain tests

atLeast :: Arbitrary a => Int -> Gen [a]
atLeast 0 = arbitrary
atLeast n = (:) <$> arbitrary <*> atLeast (n-1)

testChar :: Gen Char
testChar = growingElements "<>=\"' \tabcde\\"
testString :: Gen String
testString = listOf testChar
testBS :: Gen ByteString
testBS = S.pack <$> testString

instance Arbitrary ByteString where
    arbitrary = testBS

instance Arbitrary (Token' ByteString) where
    arbitrary = oneof [ TagOpen <$> arbitrary <*> arbitrary <*> arbitrary
                      , TagClose <$> arbitrary
                      , Text <$> S.pack <$> atLeast 1
                      ]

tests :: [Test]
tests = [ testGroup "Property"
            [ testProperty "revertiable" prop_text_non_empty
            ]
        , testGroup "Special cases"
            [ testCase "special cases" testSpecialCases
            --, testCase "parse real world file" testRealworldFiles
            ]
        ]

prop_revertiable1 :: ByteString -> Bool
prop_revertiable1 = either (const False) prop_revertiable . decode

prop_revertiable :: [Token] -> Bool
prop_revertiable tokens = either (const False) (==tokens) . decode . encode $ tokens

prop_text_non_empty :: ByteString -> Bool
prop_text_non_empty = either (const False) text_non_empty . decode
  where text_non_empty = all non_empty
        non_empty (Text s) = S.length s > 0
        non_empty _ = True

assertEither :: Either String a -> Assertion
assertEither = either (assertFailure . ("Left:"++)) (const $ return ())

assertDecode :: ByteString -> IO [Token]
assertDecode s = do
    let result = decode s
    assertEither result
    let (Right tokens) = result
    return tokens

testSpecialCases :: Assertion
testSpecialCases = mapM_ testOne testcases
  where
    testOne (str, tokens) =
      --trace (show' str tokens) $
        assertDecode str >>= assertEqual "parse result incorrect" tokens
    --show' str tokens = S.unpack $ S.concat [str, "\n", S.pack (show tokens)]
    testcases =
      -- normal
      [( "<a readonly title=xxx href=\"f<o/>o\" class=\"foo bar\" style='display:none; color:red'>bar</a>",
         [TagOpen "a" [("readonly", ""), ("title", "xxx"), ("href", "f<o/>o"), ("class", "foo bar"), ("style", "display:none; color:red")] False,
          Text "bar",
          TagClose "a"] )
      -- escape
      ,( "<a href=\"f\\\"oo\" >",
         [TagOpen "a" [("href", "f\"oo")] False] )
      ,( "<a href='f\\'o\"o' >",
         [TagOpen "a" [("href", "f'o\"o")] False] )
      ,( "<a href=\"f\\\\\\\"oo\" >",
         [TagOpen "a" [("href", "f\\\"oo")] False] )
      -- attribute
      ,( "<a href=fo\"o >",
         [TagOpen "a" [("href", "fo\"o")] False] )
      ,( "<a href=\"f\noo\" >",
         [TagOpen "a" [("href", "f\noo")] False] )
      ,( "<a href=>",
         [TagOpen "a" [("href", "")] False] )
      ,( "<a href=http://www.douban.com/>",
         [TagOpen "a" [("href", "http://www.douban.com/")] False] )
      ,( "<a href>",
         [TagOpen "a" [("href", "")] False] )
      ,( "<a href src=/>",
         [TagOpen "a" [("href", ""), ("src", "/")] False] )
      -- self close
      ,( "<br />",
         [TagOpen "br" [] True] )
      ,( "<a alt=foo />",
         [TagOpen "a" [("alt", "foo")] True] )
      ,( "</br/>",
         [TagClose "br/"] )
      -- blanks
      ,( "<a\tsrc\t\r\nhref\n=\n\"\nfo\t\no\n\" title\r\n\t=>",
         [TagOpen "a" [("src", ""), ("href", "\nfo\t\no\n"), ("title", "")] False] )
      ,( "< asafasd>",
         [Text "< asafasd>"] )
      ,( "<a href=\"http://",
         [Text "<a href=\"http://"] )
      ,( "<\n/br/>",
         [Text "<\n/br/>"] )
      ,( "</\ndiv>",
         [TagClose "\ndiv"] )
      ,( "<>",
         [Text "<>"] )
      ,( "</>",
         [TagClose ""] )
      -- comments
      ,( "<!--foo-->",
         [Comment "foo"] )
      ,( "<!--f--oo->-->",
         [Comment "f--oo->"] )
      ,( "<!--fo--o->",
         [Text "<!--fo--o->"] )
      ,( "<!--fo--o->",
         [Text "<!--fo--o->"] )
      -- special tags
      ,( "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\">",
         [Special "DOCTYPE" "html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\""] )
      ,( "<!DOCTYPE/>",
         [Special "DOCTYPE/" ""] )
      -- script link tags
      ,( "<script > var x=\"<a href=xx />\";</script>",
         [TagOpen "script" [] False
         ,Text " var x=\""
         ,Text "<a href=xx />\";"
         ,TagClose "script"
         ] )
      ,( "<script></script>",
         [TagOpen "script" [] False
         ,TagClose "script"
         ] )
      ,( "<script>",
         [TagOpen "script" [] False] )
      ,( "<script src='http://xx.js' >",
         [TagOpen "script" [("src", "http://xx.js")] False] )
      ]

testRealworldFiles :: Assertion
testRealworldFiles = mapM_ testFile files
  where
    testFile file = do
        result <- decode <$> S.readFile file
        assertEither result
        assertEqual "not equal" result $ encode <$> result >>= decode
    files = [ "qq.html"
            ]
