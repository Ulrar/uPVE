--
-- Thanks to Mark Seemann on StackOverflow for this code
--

module Web.UPVE.Disks (DiskType(..), Disk(..), findDisks) where

import Data.Aeson
import Data.Text
import Control.Applicative ((<$>), (<|>))
import Data.HashMap.Lazy (HashMap, foldlWithKey')
import Data.Foldable (toList)
import Text.Read (readMaybe)

--
-- Types
--

data DiskType = Virtio | Sata | IDE | SCSI deriving Show

data Disk = Disk { diskType    :: DiskType
                 , diskNumber  :: Int
                 , diskParams  :: Text
                 } deriving Show

--
-- Helpers
--

findNumber :: Read a => Text -> Text -> Maybe a
findNumber prefix candidate =
  stripPrefix prefix candidate >>= (readMaybe . unpack)

findDisks :: HashMap Text Value -> [Disk]
findDisks = foldlWithKey' folder []
  where
    findVirtio k s = flip (Disk Virtio) s <$> findNumber "virtio" k
    findSata   k s = flip (Disk Sata)   s <$> findNumber "sata"   k
    findIde    k s = flip (Disk IDE)    s <$> findNumber "ide"    k
    findScsi   k s = flip (Disk SCSI)   s <$> findNumber "scsi"   k
    folder acc k (String s) =
      acc ++ toList (findVirtio k s <|> findSata k s <|> findIde k s <|> findScsi k s)
    folder acc _ _ = acc
