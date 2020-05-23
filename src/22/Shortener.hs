-- Page 871

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

alphanumerics :: String
alphanumerics = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1

  randomDigit <- SR.randomRIO (0, maxIndex)
  return $ xs !! randomDigit

shortGenerator :: IO [Char]
shortGenerator = replicateM 7 $ randomElement alphanumerics

saveURI :: R.Connection -> BC.ByteString -> BC.ByteString -> IO (Either R.Reply R.Status)
saveURI connection shortURI uri = R.runRedis connection $ R.set shortURI uri

getURI :: R.Connection -> BC.ByteString -> IO (Either R.Reply (Maybe BC.ByteString))
getURI connection shortURI = R.runRedis connection $ R.get shortURI

linkShort :: String -> String
linkShort short = concat
  [ "<a href=\""
  , short
  , "\">Copy and paste your short URL.</a>"
  ]

shortCreated :: Show a => a -> String -> TL.Text
shortCreated response short = TL.concat
  [ TL.pack $ show response
  , " Shortened is: "
  , TL.pack $ linkShort short
  ]

shortIsNotURI :: TL.Text -> TL.Text
shortIsNotURI uri = TL.concat
  [ uri
  , " wasn't a URL."
  ]

shortFound :: TL.Text -> TL.Text
shortFound tbs = TL.concat
  [ "<a href=\""
  , tbs
  , "\">"
  , tbs
  , "</a>"
  ]

app :: R.Connection -> ScottyM ()
app redisConnection = do
  get "/" $ do
    uri <- param "uri"

    let parsedURI :: Maybe URI
        parsedURI = parseURI $ TL.unpack uri

    case parsedURI of
      Just _ -> do
        short <- liftIO shortGenerator

        let short' = BC.pack short
            uri' = encodeUtf8 $ TL.toStrict uri

        response <- liftIO $ saveURI redisConnection short' uri'
        html $ shortCreated response short

      Nothing ->
        text $ shortIsNotURI uri

  get "/:short" $ do
    short <- param "short"
    uri <- liftIO $ getURI redisConnection short

    case uri of
      Left reply ->
        text $ TL.pack $ show reply

      Right mbBS ->
        case mbBS of
          Nothing ->
            text "URI not found."
          Just bs ->
            html $ shortFound tbs
              where
                tbs :: TL.Text
                tbs = TL.fromStrict $ decodeUtf8 bs

main :: IO ()
main = undefined
  -- Skipping for now, until I complete Chapter 26 on monad transformers.

  -- Old implementation:
  -- redisConnection <- R.connect R.defaultConnectInfo
  -- scotty 3000 $ app redisConnection