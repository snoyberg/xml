{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Text.Hamlet.XMLParse
    ( Result (..)
    , Content (..)
    , Doc (..)
    , parseDoc
    , Binding (..)
    , DataConstr (..)
    , Module (..)
    )
    where

import Text.Shakespeare.Base
import Control.Applicative ((<$>), Applicative (..))
import Control.Monad
import Data.Char (isUpper)
import Data.Data
import Text.ParserCombinators.Parsec hiding (Line)

data Result v = Error String | Ok v
    deriving (Show, Eq, Read, Data, Typeable)
instance Monad Result where
    return = Ok
    Error s >>= _ = Error s
    Ok v >>= f = f v
    fail = Error
instance Functor Result where
    fmap = liftM
instance Applicative Result where
    pure = return
    (<*>) = ap

data Content = ContentRaw String
             | ContentVar Deref
             | ContentEmbed Deref
    deriving (Show, Eq, Read, Data, Typeable)

data Line = LineForall Deref Binding
          | LineIf Deref
          | LineElseIf Deref
          | LineElse
          | LineWith [(Deref, Binding)]
          | LineMaybe Deref Binding
          | LineNothing
          | LineCase Deref
          | LineOf Binding
          | LineTag
            { _lineTagName :: String
            , _lineAttr :: [(Maybe Deref, String, [Content])]
            , _lineContent :: [Content]
            , _lineAttrs :: [Deref]
            }
          | LineContent [Content]
    deriving (Eq, Show, Read)

parseLines :: String -> Result [(Int, Line)]
parseLines s =
    case parse (many parseLine) s s of
        Left e -> Error $ show e
        Right x -> Ok x

parseLine :: Parser (Int, Line)
parseLine = do
    ss <- fmap sum $ many ((char ' ' >> return 1) <|>
                           (char '\t' >> fail "Tabs are not allowed in Hamlet indentation"))
    x <- comment <|>
         htmlComment <|>
         backslash <|>
         controlIf <|>
         controlElseIf <|>
         (try (string "$else") >> spaceTabs >> eol >> return LineElse) <|>
         controlMaybe <|>
         (try (string "$nothing") >> spaceTabs >> eol >> return LineNothing) <|>
         controlForall <|>
         controlWith <|>
         controlCase <|>
         controlOf <|>
         angle <|>
         invalidDollar <|>
         (eol' >> return (LineContent [])) <|>
         (do
            cs <- content InContent
            isEof <- (eof >> return True) <|> return False
            if null cs && ss == 0 && isEof
                then fail "End of Hamlet template"
                else return $ LineContent cs)
    return (ss, x)
  where
    eol' = (char '\n' >> return ()) <|> (string "\r\n" >> return ())
    eol = eof <|> eol'
    invalidDollar = do
        _ <- char '$'
        fail "Received a command I did not understand. If you wanted a literal $, start the line with a backslash."
    comment = do
        _ <- try $ string "$#"
        _ <- many $ noneOf "\r\n"
        eol
        return $ LineContent []
    htmlComment = do
        _ <- try $ string "<!--"
        _ <- manyTill anyChar $ try $ string "-->"
        x <- many nonComments
        eol
        return $ LineContent [ContentRaw $ concat x] -- FIXME handle variables?
    nonComments = (many1 $ noneOf "\r\n<") <|> (do
        _ <- char '<'
        (do
            _ <- try $ string "!--"
            _ <- manyTill anyChar $ try $ string "-->"
            return "") <|> return "<")
    backslash = do
        _ <- char '\\'
        (eol >> return (LineContent [ContentRaw "\n"]))
            <|> (LineContent <$> content InContent)
    controlIf = do
        _ <- try $ string "$if"
        spaces
        x <- parseDeref
        _ <- spaceTabs
        eol
        return $ LineIf x
    controlElseIf = do
        _ <- try $ string "$elseif"
        spaces
        x <- parseDeref
        _ <- spaceTabs
        eol
        return $ LineElseIf x
    binding = do
        y <- identPattern
        spaces
        _ <- string "<-"
        spaces
        x <- parseDeref
        _ <- spaceTabs
        return (x,y)
    bindingSep = char ',' >> spaceTabs
    controlMaybe = do
        _ <- try $ string "$maybe"
        spaces
        (x,y) <- binding
        eol
        return $ LineMaybe x y
    controlForall = do
        _ <- try $ string "$forall"
        spaces
        (x,y) <- binding
        eol
        return $ LineForall x y
    controlWith = do
        _ <- try $ string "$with"
        spaces
        bindings <- (binding `sepBy` bindingSep) `endBy` eol
        return $ LineWith $ concat bindings -- concat because endBy returns a [[(Deref,Ident)]]
    controlCase = do
        _ <- try $ string "$case"
        spaces
        x <- parseDeref
        _ <- spaceTabs
        eol
        return $ LineCase x
    controlOf = do
        _   <- try $ string "$of"
        spaces
        x <- identPattern
        _   <- spaceTabs
        eol
        return $ LineOf x
    content cr = do
        x <- many $ content' cr
        case cr of
            InQuotes -> void $ char '"'
            NotInQuotes -> return ()
            NotInQuotesAttr -> return ()
            InContent -> eol
        return $ cc x
      where
        cc [] = []
        cc (ContentRaw a:ContentRaw b:c) = cc $ ContentRaw (a ++ b) : c
        cc (a:b) = a : cc b
    content' cr =     contentHash cr
                  <|> contentCaret
                  <|> contentReg cr
    contentHash cr = do
        x <- parseHash
        case x of
            Left "#" -> case cr of
                          NotInQuotes -> fail "Expected hash at end of line, got Id"
                          _ -> return $ ContentRaw "#"
            Left str -> return $ ContentRaw str
            Right deref -> return $ ContentVar deref
    contentCaret = do
        x <- parseCaret
        case x of
            Left str -> return $ ContentRaw str
            Right deref -> return $ ContentEmbed deref
    contentReg InContent = (ContentRaw . return) <$> noneOf "#@^\r\n"
    contentReg NotInQuotes = (ContentRaw . return) <$> noneOf "@^#. \t\n\r>"
    contentReg NotInQuotesAttr = (ContentRaw . return) <$> noneOf "@^ \t\n\r>"
    contentReg InQuotes = (ContentRaw . return) <$> noneOf "#@^\"\n\r"
    tagAttribValue notInQuotes = do
        cr <- (char '"' >> return InQuotes) <|> return notInQuotes
        content cr
    tagCond = do
        d <- between (char ':') (char ':') parseDeref
        tagAttrib (Just d)
    tagAttrib cond = do
        s <- many1 $ noneOf " \t=\r\n><"
        v <- (char '=' >> tagAttribValue NotInQuotesAttr) <|> return []
        return $ TagAttrib (cond, s, v)

    tagAttrs = do
        _ <- char '*'
        d <- between (char '{') (char '}') parseDeref
        return $ TagAttribs d

    tag' = foldr tag'' ("div", [], [])
    tag'' (TagName s) (_, y, as) = (s, y, as)
    tag'' (TagAttrib s) (x, y, as) = (x, s : y, as)
    tag'' (TagAttribs s) (x, y, as) = (x, y, s : as)

    ident :: Parser Ident
    ident = do
      i <- many1 (alphaNum <|> char '_' <|> char '\'')
      white
      return (Ident i)
     <?> "identifier"

    parens = between (char '(' >> white) (char ')' >> white)

    brackets = between (char '[' >> white) (char ']' >> white)

    braces = between (char '{' >> white) (char '}' >> white)

    comma = char ',' >> white

    atsign = char '@' >> white

    equals = char '=' >> white

    white = skipMany $ char ' '

    wildDots = string ".." >> white

    isVariable (Ident (x:_)) = not (isUpper x)
    isVariable (Ident []) = error "isVariable: bad identifier"

    isConstructor (Ident (x:_)) = isUpper x
    isConstructor (Ident []) = error "isConstructor: bad identifier"

    identPattern :: Parser Binding
    identPattern = gcon True <|> apat
      where
      apat = choice
        [ varpat
        , gcon False
        , parens tuplepat
        , brackets listpat
        ]

      varpat = do
        v <- try $ do v <- ident
                      guard (isVariable v)
                      return v
        option (BindVar v) $ do
          atsign
          b <- apat
          return (BindAs v b)
       <?> "variable"

      gcon :: Bool -> Parser Binding
      gcon allowArgs = do
        c <- try $ do c <- dataConstr
                      return c
        choice
          [ record c
          , fmap (BindConstr c) (guard allowArgs >> many apat)
          , return (BindConstr c [])
          ]
       <?> "constructor"

      dataConstr = do
        p <- dcPiece
        ps <- many dcPieces
        return $ toDataConstr p ps

      dcPiece = do
        x@(Ident y) <- ident
        guard $ isConstructor x
        return y

      dcPieces = do
        _ <- char '.'
        dcPiece

      toDataConstr x [] = DCUnqualified $ Ident x
      toDataConstr x (y:ys) =
          go (x:) y ys
        where
          go front next [] = DCQualified (Module $ front []) (Ident next)
          go front next (rest:rests) = go (front . (next:)) rest rests

      record c = braces $ do
        (fields, wild) <- option ([], False) $ go
        return (BindRecord c fields wild)
        where
        go = (wildDots >> return ([], True))
           <|> (do x         <- recordField
                   (xs,wild) <- option ([],False) (comma >> go)
                   return (x:xs,wild))

      recordField = do
        field <- ident
        p <- option (BindVar field) -- support punning
                    (equals >> identPattern)
        return (field,p)

      tuplepat = do
        xs <- identPattern `sepBy` comma
        return $ case xs of
          [x] -> x
          _   -> BindTuple xs

      listpat = BindList <$> identPattern `sepBy` comma

    angle = do
        _ <- char '<'
        name' <- many  $ noneOf " \t\r\n>"
        let name = if null name' then "div" else name'
        xs <- many $ try ((many $ oneOf " \t\r\n") >>
              (tagCond <|> tagAttrs <|> tagAttrib Nothing))
        _ <- many $ oneOf " \t\r\n"
        _ <- char '>'
        c <- content InContent
        let (tn, attr, attrsd) = tag' $ TagName name : xs
        return $ LineTag tn attr c attrsd

data TagPiece = TagName String
              | TagAttrib (Maybe Deref, String, [Content])
              | TagAttribs Deref
    deriving Show

data ContentRule = InQuotes | NotInQuotes | NotInQuotesAttr | InContent

data Nest = Nest Line [Nest]

nestLines :: [(Int, Line)] -> [Nest]
nestLines [] = []
nestLines ((i, l):rest) =
    let (deeper, rest') = span (\(i', _) -> i' > i) rest
     in Nest l (nestLines deeper) : nestLines rest'

data Doc = DocForall Deref Binding [Doc]
         | DocWith [(Deref, Binding)] [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocMaybe Deref Binding [Doc] (Maybe [Doc])
         | DocCase Deref [(Binding, [Doc])]
         | DocTag String [(Maybe Deref, String, [Content])] [Deref] [Doc]
         | DocContent Content
         -- FIXME PIs
    deriving (Show, Eq, Read, Data, Typeable)

nestToDoc :: [Nest] -> Result [Doc]
nestToDoc [] = Ok []
nestToDoc (Nest (LineForall d i) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ DocForall d i inside' : rest'
nestToDoc (Nest (LineWith dis) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ DocWith dis inside' : rest'
nestToDoc (Nest (LineIf d) inside:rest) = do
    inside' <- nestToDoc inside
    (ifs, el, rest') <- parseConds ((:) (d, inside')) rest
    rest'' <- nestToDoc rest'
    Ok $ DocCond ifs el : rest''
nestToDoc (Nest (LineMaybe d i) inside:rest) = do
    inside' <- nestToDoc inside
    (nothing, rest') <-
        case rest of
            Nest LineNothing ninside:x -> do
                ninside' <- nestToDoc ninside
                return (Just ninside', x)
            _ -> return (Nothing, rest)
    rest'' <- nestToDoc rest'
    Ok $ DocMaybe d i inside' nothing : rest''
nestToDoc (Nest (LineCase d) inside:rest) = do
    let getOf (Nest (LineOf x) insideC) = do
            insideC' <- nestToDoc insideC
            Ok (x, insideC')
        getOf _ = Error "Inside a $case there may only be $of.  Use '$of _' for a wildcard."
    cases <- mapM getOf inside
    rest' <- nestToDoc rest
    Ok $ DocCase d cases : rest'
nestToDoc (Nest (LineTag tn attrs content attrsD) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ (DocTag tn attrs attrsD $ map DocContent content ++ inside') : rest'
nestToDoc (Nest (LineContent content) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ map DocContent content ++ inside' ++ rest'
nestToDoc (Nest (LineElseIf _) _:_) = Error "Unexpected elseif"
nestToDoc (Nest LineElse _:_) = Error "Unexpected else"
nestToDoc (Nest LineNothing _:_) = Error "Unexpected nothing"
nestToDoc (Nest (LineOf _) _:_) = Error "Unexpected 'of' (did you forget a $case?)"

parseDoc :: String -> Result [Doc]
parseDoc s = do
    ls <- parseLines s
    let notEmpty (_, LineContent []) = False
        notEmpty _ = True
    let ns = nestLines $ filter notEmpty ls
    ds <- nestToDoc ns
    return ds

parseConds :: ([(Deref, [Doc])] -> [(Deref, [Doc])])
           -> [Nest]
           -> Result ([(Deref, [Doc])], Maybe [Doc], [Nest])
parseConds front (Nest LineElse inside:rest) = do
    inside' <- nestToDoc inside
    Ok $ (front [], Just inside', rest)
parseConds front (Nest (LineElseIf d) inside:rest) = do
    inside' <- nestToDoc inside
    parseConds (front . (:) (d, inside')) rest
parseConds front rest = Ok (front [], Nothing, rest)

data Binding = BindVar Ident
             | BindAs Ident Binding
             | BindConstr DataConstr [Binding]
             | BindTuple [Binding]
             | BindList [Binding]
             | BindRecord DataConstr [(Ident, Binding)] Bool
    deriving (Eq, Show, Read, Data, Typeable)

data DataConstr = DCQualified Module Ident
                | DCUnqualified Ident
    deriving (Eq, Show, Read, Data, Typeable)

newtype Module = Module [String]
    deriving (Eq, Show, Read, Data, Typeable)

spaceTabs :: Parser String
spaceTabs = many $ oneOf " \t"
