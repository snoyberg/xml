{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import Data.DTD.Parse.Unresolved (skipWS, textDecl, dtdComponent)
import Data.Attoparsec.Text.Lazy
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Char (isSpace)

main :: IO ()
main = do
    args <- getArgs
    toFail <-
        case args of
            [a] -> return a
            _ -> error "Usage: runghc lazy-bug.hs <yes/no, whether to show the bug>"
    let str = concat $ replicate 1 element
    let text =
            if toFail == "yes"
                then TL.fromChunks $ map T.singleton str
                else TL.fromChunks [T.pack str]
    case parse (skipWS >> optional textDecl >> many1 (skipWS >> dtdComponent)) text of
        Done t _ | TL.all isSpace t -> putStrLn "Success"
        Done t _ ->
            case parse (skipWS >> dtdComponent) t of
                Done _ _ -> putStrLn "This should never happen"
                Fail _ s c -> error $ show ("inner", s, c)
        Fail t s c -> error $ show ("outer", s, c, t)

element = "<!ELEMENT conbody ((dl | parml | fig | syntaxdiagram | imagemap | image | lines | lq | note | hazardstatement | object | ol| p | pre | codeblock | msgblock | screen | simpletable | sl | table | ul | data | data-about | draft-comment | foreign | unknown | required-cleanup)*, (section | example | conbodydiv)* ) >\n"
