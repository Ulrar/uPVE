module Web.UPVE.Types ( Credentials(..)
                      , PVEServer(..)
                      , VM(..)
                      , Storage(..)
                      , Action(..)
                      ) where

import                  Data.Text
import                  Data.Aeson
import qualified        Data.ByteString.Char8         as B
import qualified        Data.ByteString.Internal      as BI

--
-- Authentication
--

data Credentials = Credentials {ticket :: Text, token :: BI.ByteString } deriving Show

instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \b -> (b .: "data") >>= \v -> Credentials
    <$> v .: "ticket"
    <*> ((v .: "CSRFPreventionToken") >>= return . B.pack)

data PVEServer = PVEServer {host :: BI.ByteString, port :: Int, credentials :: Credentials} deriving Show

--
-- VMs
--

data Status = Running | Stopped | Suspended deriving Show

data VM = VM { vmName     :: Text
             , vmNode     :: Text
             , vmId       :: Text
             , vmNId      :: Int
             , vmTemplate :: Bool
             , vmMem      :: Int
             , vmMaxmem   :: Int
             , vmDisk     :: Int
             , vmMaxdisk  :: Int
             , vmCpu      :: Float
             , vmMaxcpu   :: Int
             , vmNetin    :: Float
             , vmNetout   :: Float
             , vmStatus   :: Status
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

--
-- Storage
--

data Storage = Storage { storageId     :: Text
                       , storageName   :: Text
                       , storageNode   :: Text
                       , storageSize   :: Int
                       , storageUsed   :: Int
                       } deriving Show

instance FromJSON Storage where
  parseJSON = withObject "Storage" $ \v -> Storage
    <$> v .: "id"
    <*> v .: "storage"
    <*> v .: "node"
    <*> v .: "maxdisk"
    <*> v .: "disk"

data StorageList = StorageList { sl :: [Storage] }

instance FromJSON StorageList where
  parseJSON = withObject "StorageList" $ \v -> StorageList
    <$> v .: "data"

--
-- Misc
--

data Action = Start | Stop | Restart

instance Show Action where
  show Start   = "start"
  show Stop    = "stop"
  show Restart = "restart"
