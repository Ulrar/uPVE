module Web.UPVE.Internal ( VMList(..)
                         , StorageList(..)
                         ) where

import                  Web.UPVE.Types

import                  Data.Text
import                  Data.Aeson

--
-- Ressources
--

data VMList = VMList { vml :: [VM] }

instance FromJSON VMList where
  parseJSON = withObject "VMList" $ \v -> VMList
    <$> v .: "data"

data StorageList = StorageList { sl :: [Storage] }

instance FromJSON StorageList where
  parseJSON = withObject "StorageList" $ \v -> StorageList
    <$> v .: "data"
