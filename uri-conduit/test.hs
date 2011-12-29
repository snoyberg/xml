import Network.URI.Conduit
import Network.URI.Conduit.File
import System.Environment (getArgs)

main :: IO ()
main = do
    [src', dst'] <- getArgs
    src <- decodeString src'
    dst <- decodeString dst'
    copyURI (toSchemeMap [fileScheme]) src dst
