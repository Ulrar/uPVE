module Web.UPVE.Types (Credentials(..)) where

import Data.Text
import Data.Aeson

data Credentials = Credentials {ticket :: Text, token :: Text }

instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \b -> (b .: "data") >>= \v -> Credentials
    <$> v .: "ticket"
    <*> v .: "CSRFPreventionToken"
