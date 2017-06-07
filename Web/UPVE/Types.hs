module Web.UPVE.Types (Credentials(..), PVEServer(..), VM(..), VMList(..)) where

import                  Data.Text
import                  Data.Aeson
import qualified        Data.ByteString.Internal      as BI


-- Authentication
data Credentials = Credentials {ticket :: Text, token :: Text } deriving Show

instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \b -> (b .: "data") >>= \v -> Credentials
    <$> v .: "ticket"
    <*> v .: "CSRFPreventionToken"

data PVEServer = PVEServer {host :: BI.ByteString, port :: Int, credentials :: Credentials} deriving Show

-- VMs

data VMStatus = Running | Stopped | Suspended deriving Show

data VM = VM { name     :: Text
             , node     :: Text
             , id       :: Text
             , vmid     :: Int
             , template :: Bool
             , mem      :: Int
             , maxmem   :: Int
             , disk     :: Int
             , maxdisk  :: Int
             , cpu      :: Float
             , maxcpu   :: Int
             , netin    :: Float
             , netout   :: Float
             , status   :: VMStatus
             } deriving Show

instance FromJSON VM where
  parseJSON = withObject "VM" $ \v -> VM
    <$> v .: "name"
    <*> v .: "node"
    <*> v .: "id"
    <*> v .: "vmid"
    <*> (do
      val <- v .: "template"
      if (val :: Int) == 0
        then return False
        else return True)
    <*> v .: "mem"
    <*> v .: "maxmem"
    <*> v .: "disk"
    <*> v .: "maxdisk"
    <*> v .: "cpu"
    <*> v .: "maxcpu"
    <*> v .: "netin"
    <*> v .: "netout"
    <*> (do
      val <- v .: "status"
      case (val :: Text) of
          "running"   -> return Running
          "stopped"   -> return Stopped
          "suspended" -> return Suspended
        )

data VMList = RessourceVM { vml :: [VM] }

instance FromJSON VMList where
  parseJSON = withObject "RessourceVM" $ \v -> RessourceVM
    <$> v .: "data"
