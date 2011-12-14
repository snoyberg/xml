import qualified Text.XML.Enumerator.Document as D

main = D.readFile_ "test8.xml" >>= D.writeFile "test8.out.xml"
