module Web.UPVE.Resources ( VM(..)
                          , Storage(..)
                          ) where

import                    Data.Text
import                    Data.Aeson

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
