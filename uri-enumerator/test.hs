import Network.URI.Enumerator
import Network.URI.Enumerator.File
import Network.URI.Enumerator.HTTP
import System.Environment (getArgs)

main :: IO ()
main = withManager $ \m -> do
    [src', dst'] <- getArgs
    src <- decodeString src'
    dst <- decodeString dst'
    copyURI (toSchemeMap [fileScheme, httpScheme m]) src dst
