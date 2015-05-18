{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Text.XML.Stream.ConduitParser
  ( -- * Conduit parser monad
    ConduitParser()
  , runConduitParser
    -- * XML parsers
  , tag
  , tagName
  , tagPredicate
  , tagNoAttr
  , content
    -- * Low level: event parsers
  , beginDocument
  , endDocument
  , beginDoctype
  , endDoctype
  , instruction
  , beginElement
  , endElement
  , contentEntity
  , contentText
  , comment
  , cdata
  , text
    -- * Re-exports
  , XmlException(..)
  , AttrParser()
  , attr
  , optionalAttr
  , ignoreAttrs
  , requireAttr
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State

import           Data.Char
import           Data.Conduit                 hiding (await)
import qualified Data.Conduit                 as Conduit
import           Data.Conduit.List            (peek)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    as Text (Text, all, pack, unpack)
import           Data.Void
import           Data.XML.Types

import           Text.Parser.Combinators
import           Text.XML.Stream.Parse        (AttrParser (), XmlException (..),
                                               attr, ignoreAttrs, optionalAttr,
                                               requireAttr, runAttrParser)

-- | Conduit parser monad transformer.
newtype ConduitParser i m a = ConduitParser { unConduitParser :: EitherT Text (StateT (Maybe Text, [i]) (ConduitM i Void m)) a }

deriving instance Applicative (ConduitParser i m)
deriving instance Functor (ConduitParser i m)
deriving instance (Monad m) => Monad (ConduitParser i m)

-- | The use of 'guard' is not recommended as it generates unhelpful error messages.
-- Please consider using 'unexpected' instead.
instance (Monad m) => Alternative (ConduitParser i m) where
  empty = unexpected "ConduitParser.empty"

  (ConduitParser parserA) <|> (ConduitParser parserB) = do
    b <- resetBuffer
    resultA <- ConduitParser $ optional parserA

    case resultA of
      Just a -> prependBuffer b >> return a
      _ -> do
        backtrack
        resultB <- ConduitParser $ optional parserB

        case resultB of
          Just a -> return a
          _ -> backtrack >> prependBuffer b >> ConduitParser (left "A <|> B")

instance (Monad m) => MonadThrow (ConduitParser i m) where
  throwM = unexpected . show

instance MonadTrans (ConduitParser i) where
  lift = ConduitParser . lift . lift . lift

-- | Note: backtracking is automatic.
instance (Monad m) => Parsing (ConduitParser i m) where
  try parser = parser

  parser <?> name = do
    setParserName $ pack name
    a <- parser
    resetParserName
    return a

  unexpected e = do
    backtrack
    name <- getParserName
    ConduitParser . left $ maybe "" (\n -> "Unable to parse " <> n <> ": ") name <> pack e

  eof = do
    result <- ConduitParser . lift . lift $ peek
    maybe (unexpected "Expected end of input.") (void . return) result

  notFollowedBy parser = do
    result <- optional parser
    name <- fromMaybe "unnamed parser" <$> getParserName
    forM_ result $ \_ -> unexpected $ "Should not be followed by: " ++ (unpack name)


-- | Run a 'ConduitParser'. Any parsing failure will be thrown as an exception.
runConduitParser :: (MonadThrow m) => ConduitParser i m a -> ConduitM i Void m a
runConduitParser (ConduitParser p) = do
  (result, s) <- runStateT (runEitherT p) (mempty, mempty)
  case result of
    Right a -> return a
    Left e -> throwM $ XmlException (unpack e) Nothing

getParserName :: ConduitParser i m (Maybe Text)
getParserName = ConduitParser . lift $ gets fst

setParserName :: Text -> ConduitParser i m ()
setParserName name = ConduitParser . lift . modify $ \(_, b) -> (Just name, b)

resetParserName :: ConduitParser i m ()
resetParserName = ConduitParser . lift . modify $ \(_, b) -> (Nothing, b)

getBuffer :: ConduitParser i m [i]
getBuffer = ConduitParser . lift $ gets snd

appendBuffer :: [i] -> ConduitParser i m ()
appendBuffer new = ConduitParser . lift $ modify (\(n, b) -> (n, b ++ new))

prependBuffer :: [i] -> ConduitParser i m ()
prependBuffer new = ConduitParser . lift $ modify (\(n, b) -> (n, new ++ b))

resetBuffer :: (Monad m) => ConduitParser i m [i]
resetBuffer = do
  b <- getBuffer
  ConduitParser . lift $ modify (\(n, _) -> (n, mempty))
  return b

backtrack :: (Monad m) => ConduitParser i m ()
backtrack = mapM_ (ConduitParser . lift . lift . leftover) =<< resetBuffer

-- | Wrapping around 'Conduit.await'.
await :: (Monad m) => ConduitParser i m i
await = do
  event <- ConduitParser . lift . lift $ Conduit.await
  e     <- maybe (unexpected "Unexpected end of input.") return event
  appendBuffer [e]
  return e

-- | Parse an 'EventBeginDocument'.
beginDocument :: (Monad m) => ConduitParser Event m ()
beginDocument = do
  event <- await
  case event of
   EventBeginDocument -> return ()
   _ -> unexpected $ "Expected XML begin document, got: " ++ (show event)

-- | Parse an 'EventEndDocument'.
endDocument :: (Monad m) => ConduitParser Event m ()
endDocument = do
  event <- await
  case event of
   EventEndDocument -> return ()
   _ -> unexpected $ "Expected XML end document, got: " ++ (show event)

-- | Parse an 'EventBeginDoctype'.
beginDoctype :: (Monad m) => ConduitParser Event m (Text, Maybe ExternalID)
beginDoctype = do
  event <- await
  case event of
   EventBeginDoctype doctype externalID -> return (doctype, externalID)
   _ -> unexpected $ "Expected XML begin doctype, got: " ++ (show event)

-- | Parse an 'EventEndDoctype'.
endDoctype :: (Monad m) => ConduitParser Event m ()
endDoctype = do
  event <- await
  case event of
   EventEndDoctype -> return ()
   _ -> unexpected $ "Expected XML end doctype, got: " ++ (show event)

-- | Parse an 'EventInstruction'.
instruction :: (Monad m) => ConduitParser Event m Instruction
instruction = do
  event <- await
  case event of
   EventInstruction i -> return i
   _ -> unexpected $ "Expected XML instruction, got: " ++ (show event)

-- | Parse an 'EventBeginElement'.
beginElement :: (Monad m) => ConduitParser Event m (Name, [(Name, [Content])])
beginElement = do
  event <- await
  case event of
   EventBeginElement n a -> return (n, a)
   _ -> unexpected $ "Expected XML begin element, got: " ++ (show event)

-- | Parse an 'EventEndElement'.
endElement :: (Monad m) => ConduitParser Event m Name
endElement = do
  event <- await
  case event of
   EventEndElement n -> return n
   _ -> unexpected $ "Expected XML end element, got: " ++ (show event)

-- | Parse a 'ContentEntity' (within an 'EventContent').
contentEntity :: (Monad m) => ConduitParser Event m Text
contentEntity = do
  event <- await
  case event of
   EventContent (ContentEntity t) -> return t
   _ -> unexpected $ "Expected XML content entity, got: " ++ (show event)

-- | Parse a 'ContentText' (within an 'EventContent').
contentText :: (Monad m) => ConduitParser Event m Text
contentText = do
  event <- await
  case event of
   EventContent (ContentText t) -> return t
   _ -> unexpected $ "Expected XML textual content, got: " ++ (show event)

-- | Parse an 'EventComment'.
comment :: (Monad m) => ConduitParser Event m Text
comment = do
  event <- await
  case event of
   EventComment t -> return t
   _ -> unexpected $ "Expected XML comment, got: " ++ (show event)

-- | Parse an 'EventCDATA'.
cdata :: (Monad m) => ConduitParser Event m Text
cdata = do
  event <- await
  case event of
   EventCDATA t -> return t
   _ -> unexpected $ "Expected XML CDATA, got: " ++ (show event)

-- | Parse a textual 'EventContent' or an 'EventCDATA'.
text :: (Monad m) => ConduitParser Event m Text
text = contentText <|> cdata


-- | Parse an XML tag.
tag :: (Monad m)
    => (Name -> Maybe a)
    -> (a -> AttrParser b)
    -> (b -> ConduitParser Event m c)
    -> ConduitParser Event m c
tag checkName attrParser f = do
  skipMany ignored
  (name, attributes) <- beginElement
  a <- maybe (unexpected $ "Invalid element name: " ++ show name) return $ checkName name
  b <- either (unexpected . show) return $ runAttrParser' (attrParser a) attributes
  result <- f b
  skipMany ignored
  endName <- endElement
  when (endName /= name) . unexpected $ "Invalid closing tag: expected </" ++ unpack (nameLocalName name) ++ ">, got </" ++ unpack (nameLocalName endName) ++ ">"
  return result

  where ignored = beginDocument <|> endDocument <|> void beginDoctype <|> void endDoctype <|> void instruction <|> void comment <|> spaceContent
        spaceContent = do
          event <- await
          case event of
            EventContent (ContentText t)
              | Text.all isSpace t -> return ()
              | otherwise -> raiseError event
            _ -> raiseError event
          where raiseError event = unexpected $ "Unexpected element: " ++ show event

        runAttrParser' parser attributes = case runAttrParser parser attributes of
          Left e -> Left e
          Right ([], x) -> Right x
          Right (attr, _) -> Left $ toException $ UnparsedAttributes attr


tagPredicate :: (Monad m) => (Name -> Bool) -> AttrParser a -> (a -> ConduitParser Event m b) -> ConduitParser Event m b
tagPredicate p attrParser = tag (guard . p) (const attrParser)

tagName :: (Monad m) => Name -> AttrParser a -> (a -> ConduitParser Event m b) -> ConduitParser Event m b
tagName name = tagPredicate (== name)

tagNoAttr :: Monad m => Name -> ConduitParser Event m a -> ConduitParser Event m a
tagNoAttr name f = tagName name (return ()) $ const f


content :: Monad m => ConduitParser Event m Text
content = do
  skipMany ignored
  mconcat <$> sepEndBy text ignored
  where ignored = beginDocument <|> endDocument <|> void beginDoctype <|> endDoctype <|> void instruction <|> void comment
