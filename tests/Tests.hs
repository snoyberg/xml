{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Main where

import Debug.Trace
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
            [-- testProperty "revertiable" prop_revertiable1
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
      trace (show' str tokens) $
        assertDecode str >>= assertEqual "parse result incorrect" tokens
    show' str tokens = S.unpack $ S.concat [str, "\n", S.pack (show tokens)]
    testcases =
      [( "<a readonly title=xxx href=\"f<o/>o\" class=\"foo bar\">bar</a>",
         [TagOpen "a" [("readonly", ""), ("title", "xxx"), ("href", "f<o/>o"), ("class", "foo bar")] False,
          Text "bar",
          TagClose "a"] )
      ,( "<a href=fo\"o >",
         [TagOpen "a" [("href", "fo\"o")] False] )
      ,( "<a href=\"f\\\"oo\" >",
         [TagOpen "a" [("href", "f\"oo")] False] )
      ,( "<a href=\"f\\\\\\\"oo\" >",
         [TagOpen "a" [("href", "f\\\"oo")] False] )
      ,( "<a href=\"f\noo\" >",
         [TagOpen "a" [("href", "f\noo")] False] )
      ,( "<a href=>",
         [TagOpen "a" [("href", "")] False] )
      ,( "<a href=http://www.douban.com/>",
         [TagOpen "a" [("href", "http://www.douban.com/")] False] )
      ,( "<a alt=foo />",
         [TagOpen "a" [("alt", "foo")] True] )
      ,( "<a href>",
         [TagOpen "a" [("href", "")] False] )
      ,( "<a href src=/>",
         [TagOpen "a" [("href", ""), ("src", "/")] False] )
      ,( "<a\tsrc\t\r\nhref\n=\n\"\nfo\t\no\n\" title\r\n\t=>",
         [TagOpen "a" [("src", ""), ("href", "\nfo\t\no\n"), ("title", "")] False] )
      ,( "<br />",
         [TagOpen "br" [] True] )
      ,( "</br/>",
         [TagClose "br/"] )
      ,( "<\n/br/>",
         [Text "<\n/br/>"] )
      ,( "< asafasd>",
         [Text "< asafasd>"] )
      ,( "<a href=\"http://",
         [Text "<a href=\"http://"] )
      ,( "<>",
         [Text "<>"] )
      ,( "</>",
         [TagClose ""] )
      ,( "</\ndiv>",
         [TagClose "\ndiv"] )
      ,( "<!--foo-->",
         [Comment "foo"] )
      ,( "<!--f--oo->-->",
         [Comment "f--oo->"] )
      ,( "<!--fo--o->",
         [Text "<!--fo--o->"] )
      ,( "<!--fo--o->",
         [Text "<!--fo--o->"] )
      ,( "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\">",
         [Special "DOCTYPE" "html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\""] )
      ,( "<!DOCTYPE/>",
         [Special "DOCTYPE/" ""] )
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
