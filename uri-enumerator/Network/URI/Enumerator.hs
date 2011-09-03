{-# LANGUAGE OverloadedStrings #-}
module Network.URI.Enumerator
    ( URI (..)
    , URIAuth (..)
    , hasExtension
    , readURI
    , writeURI
    , decodeString
    , relativeTo
    , parseURI
    , parseURIReference
    , parseRelativeReference
    , nullURI
    , copyURI
    ) where

import qualified Network.URI as N
import Data.Text (Text, cons, isSuffixOf, pack, unpack)
import Data.Enumerator (Enumerator, run_, ($$))
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO)
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T
import Data.Enumerator.Binary (enumFile, iterHandle)

data URI = URI
    { uriScheme :: Text
    , uriAuthority :: Maybe URIAuth
    , uriPath :: Text
    , uriQuery :: Text
    , uriFragment :: Text
    }
    deriving (Show, Eq, Ord)

data URIAuth = URIAuth
    { uriUserInfo :: Text
    , uriRegName :: Text
    , uriPort :: Text
    }
    deriving (Show, Eq, Ord)

parseURI :: Text -> Maybe URI
parseURI = fmap fromNetworkURI . N.parseURI . unpack

parseURIReference :: Text -> Maybe URI
parseURIReference = fmap fromNetworkURI . N.parseURIReference . unpack

parseRelativeReference :: Text -> Maybe URI
parseRelativeReference = fmap fromNetworkURI . N.parseRelativeReference . unpack

hasExtension :: URI -> Text -> Bool
hasExtension URI { uriPath = p } t = (cons '.' t) `isSuffixOf` p

-- FIXME make much more extensible
readURI :: URI -> Enumerator ByteString IO b -- FIXME make it work in other monads
readURI uri step =
    case uriScheme uri of
        "file:" -> do
            let fp = FP.fromText $ uriPath uri
            enumFile (FP.encodeString fp) step
        x -> error $ "Unknown URI scheme: " ++ show x

writeURI :: URI -> Enumerator ByteString IO () -> IO ()
writeURI uri enum =
    case uriScheme uri of
        "file:" -> do
            let fp = FP.fromText $ uriPath uri
            F.createTree $ FP.directory fp
            F.withFile fp F.WriteMode $ \h -> run_ $ enum $$ iterHandle h

decodeString :: String -> IO URI
decodeString s =
    case N.parseURI s of
        Just u -> return $ fromNetworkURI u
        Nothing -> do
            wd <- F.getWorkingDirectory
            let fp = wd FP.</> FP.decodeString s
            case N.parseURI $ unpack $ T.append "file://" $ T.map fixSlash $ either id id $ FP.toText fp of
                Nothing -> error $ "Could not parse URI: " ++ s
                Just uri -> return $ fromNetworkURI uri
  where
    fixSlash '\\' = '/'
    fixSlash c = c

toNetworkURI :: URI -> N.URI
toNetworkURI u = N.URI
    { N.uriScheme = unpack $ uriScheme u
    , N.uriAuthority = fmap go $ uriAuthority u
    , N.uriPath = unpack $ uriPath u
    , N.uriQuery = unpack $ uriQuery u
    , N.uriFragment = unpack $ uriFragment u
    }
  where
    go a = N.URIAuth
        { N.uriUserInfo = unpack $ uriUserInfo a
        , N.uriRegName = unpack $ uriRegName a
        , N.uriPort = unpack $ uriPort a
        }

fromNetworkURI :: N.URI -> URI
fromNetworkURI u = URI
    { uriScheme = pack $ N.uriScheme u
    , uriAuthority = fmap go $ N.uriAuthority u
    , uriPath = pack $ N.uriPath u
    , uriQuery = pack $ N.uriQuery u
    , uriFragment = pack $ N.uriFragment u
    }
  where
    go a = URIAuth
        { uriUserInfo = pack $ N.uriUserInfo a
        , uriRegName = pack $ N.uriRegName a
        , uriPort = pack $ N.uriPort a
        }

relativeTo :: URI -> URI -> Maybe URI
relativeTo a b = fmap fromNetworkURI $ toNetworkURI a `N.relativeTo` toNetworkURI b

nullURI :: URI
nullURI = fromNetworkURI N.nullURI

copyURI :: URI -> URI -> IO ()
copyURI src dst = writeURI dst $ readURI src
