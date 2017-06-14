module Web.UPVE.Types ( Credentials(..)
                      , PVEServer(..)
                      , VM(..)
                      , Disk(..)
                      , DiskType(..)
                      , Action(..)
                      ) where

import                  Web.UPVE.Disks

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
-- VM
--

data VM = VM { vmName     :: Text
             , vmCores    :: Int
             , vmMemory   :: Int
             , vmBootDisk :: Text
             , vmNuma     :: Int
             , vmOSType   :: Text
             , vmSockets  :: Int
             , vmDisks    :: [Disk]
             } deriving Show

instance FromJSON VM where
  parseJSON = withObject "VM" $ \b -> (b .: "data") >>= \v -> VM
    <$> v .: "name"
    <*> v .: "cores"
    <*> v .: "memory"
    <*> v .: "bootdisk"
    <*> v .: "numa"
    <*> v .: "ostype"
    <*> v .: "sockets"
    <*> (return $ findDisks v)

--
-- Misc
--

data Action = Start | Stop | Restart

instance Show Action where
  show Start   = "start"
  show Stop    = "stop"
  show Restart = "restart"
