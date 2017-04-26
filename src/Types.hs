module Types (
  Df
, DfFs(..)
, Meminfo
, MeminfoEntry(..)
, Ntp(..)
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


-- All of the /proc/meminfo entries
type Meminfo = [MeminfoEntry]

-- One of the /proc/meminfo entries
data MeminfoEntry =
  MeminfoEntry { mieKey :: !Text
               , mieVal :: !Word64 -- 32 is needed by we resolve kB as we parse
               } deriving (Eq, Show)


-- Offset/jitter from the remote peer or server presently used as the primary reference
data Ntp =
  Ntp { ntpOffset :: !Double -- ^ Mean offset (phase) in the times reported between this local host and the remote peer or server (RMS, milliseconds);
      , ntpJitter :: !Double -- ^ Mean deviation (jitter) in the time reported for that remote peer or server (RMS of difference of multiple time samples, milliseconds);
      } deriving (Eq, Show)
