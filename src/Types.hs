module Types (
  Df
, DfFs(..)
, Meminfo
, MeminfoEntry(..)
) where


import Data.Text (Text)
import Data.Word


-- All 'df' output
type Df = [DfFs]

-- A line of 'df' output
data DfFs = DfFs
  { dffsFilesystem :: !Text
  , dffsMountpoint :: !Text
  , dffsCapacity :: !Word64
  , dffsUsed :: !Word64
  , dffsAvailable :: !Word64
  } deriving (Eq, Show)


type Meminfo = [MeminfoEntry]

data MeminfoEntry =
  MeminfoEntry { mieKey :: !Text
               , mieVal :: !Word64 -- 32 is needed by we resolve kB as we parse
               } deriving (Eq, Show)
