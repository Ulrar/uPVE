module Web.UPVE (login
                , getVMList
                , getStorageList
                , changeStatus
                ) where

import                  Web.UPVE.Types
import                  Web.UPVE.Internal

import Data.Time.Clock
import Data.Time.Calendar

import                  Network.HTTP.Simple
import                  Network.HTTP.Conduit          (Cookie(..), createCookieJar, Request(cookieJar))
import                  Network.HTTP.Base             (urlEncodeVars)
import                  Data.Aeson                    (FromJSON)
import qualified        Data.Text                     as T
import qualified        Data.ByteString.Char8         as B
import qualified        Data.ByteString.Internal      as BI
import qualified        Data.ByteString.Lazy.Char8    as Char8

--
-- Auth
--

login :: BI.ByteString -> Int -> String -> String -> IO (Either JSONException PVEServer)
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
    Right c   -> return $ Right (PVEServer {host = host, port = port, credentials = c})

-- FIXME: Do that, but clean
mkAuthCookie pve = do
  let future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)
  let past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
  let tc = Cookie { cookie_name = "PVEAuthCookie"
                  , cookie_value = B.pack $ T.unpack $ ticket $ credentials pve
                  , cookie_expiry_time = future
                  , cookie_domain = host pve
                  , cookie_path = "/"
                  , cookie_creation_time = past
                  , cookie_last_access_time = past
                  , cookie_persistent = False
                  , cookie_host_only = True
                  , cookie_secure_only = True
                  , cookie_http_only = False
                  }
  Just $ createCookieJar [tc]

--
-- Ressources
--

getRessources :: (FromJSON a, FromJSON b) => PVEServer -> String -> (a -> [b]) -> IO (Either JSONException [b])
getRessources pve t u = do
  let request = setRequestHost    (host pve)
              $ setRequestPort    (port pve)
              $ setRequestPath    (B.pack $ "/api2/json/cluster/resources?type=" ++ t)
              $ setRequestMethod  "GET"
              $ setRequestSecure  True
              $ defaultRequest
  response <- httpJSONEither (request {cookieJar = mkAuthCookie pve})
  case getResponseBody response of
    Left  err -> return $ Left err
    Right l   -> return $ Right (u l)

getVMList :: PVEServer -> IO (Either JSONException [VM])
getVMList pve = getRessources pve "vm" vml

getStorageList :: PVEServer -> IO (Either JSONException [Storage])
getStorageList pve = getRessources pve "storage" sl

--
-- Status
--

changeStatus :: PVEServer -> Int -> Action -> IO (Bool)
changeStatus pve id action = do
  let request = setRequestHost    (host pve)
              $ setRequestPort    (port pve)
              $ setRequestPath    (B.pack ("/api2/json/nodes/s1/qemu/" ++ (show id) ++ "/status/" ++ (show action)))
              $ setRequestMethod  "POST"
              $ setRequestHeader  "CSRFPreventionToken" [B.pack $ T.unpack $ token $ credentials pve]
              $ setRequestSecure  True
              $ defaultRequest
  response <- httpLBS (request {cookieJar = mkAuthCookie pve})
  return True
