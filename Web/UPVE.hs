module Web.UPVE (login) where

import                  Web.UPVE.Types

import                  Network.HTTP.Simple
import                  Network.HTTP.Base             (urlEncodeVars)
import qualified        Data.Text                     as T
import qualified        Data.ByteString.Internal      as BI
import qualified        Data.ByteString.Lazy.Char8    as Char8

login :: BI.ByteString -> Int -> String -> String -> IO (Either JSONException (BI.ByteString, Int, Credentials))
login host port username password = do
  let request = setRequestHost    host
              $ setRequestPort    port
              $ setRequestPath    "/api2/json/access/ticket"
              $ setRequestMethod  "POST"
              $ setRequestBodyLBS (Char8.pack $ urlEncodeVars [("username", username), ("password", password)])
              $ setRequestSecure  True
              $ defaultRequest
  response <- httpJSONEither request
  case getResponseBody response of
    Left  err -> return $ Left err
    Right c   -> return $ Right (host, port, c)
