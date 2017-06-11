module Web.UPVE.Internal ( VMRList(..)
                         , StorageRList(..)
                         ) where

import                  Web.UPVE.Types
import qualified        Web.UPVE.Resources as R

import                  Data.Text
import                  Data.Aeson

--
-- Ressources
--

data VMRList = VMRList { vml :: [R.VM] }

instance FromJSON VMRList where
  parseJSON = withObject "VMRList" $ \v -> VMRList
    <$> v .: "data"

data StorageRList = StorageRList { sl :: [R.Storage] }

instance FromJSON StorageRList where
  parseJSON = withObject "StorageRList" $ \v -> StorageRList
    <$> v .: "data"
