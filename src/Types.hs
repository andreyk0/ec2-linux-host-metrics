module Types (
  CPUInfo
, CPUInfoSection
, CPUInfoSummary(..)
, Df
, DfFs(..)
, Loadavg(..)
, Meminfo
, MeminfoEntry(..)
, NSCounter(..)
, NetStat(..)
, NetStatLookup(..)
, Ntp(..)
, Stat(..)
) where


import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Word


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


-- At the moment we only look at cpu info to count processor cores
-- (to normalize other metrics across different hardware profiles)
newtype CPUInfoSummary =
  CPUInfoSummary { cpuisNumPhysCores :: Word16
                 } deriving (Eq, Show)


-- Results of parsing /proc/cpuinfo
type CPUInfo = [CPUInfoSection]
type CPUInfoSection = [CPUInfoEntry]
type CPUInfoEntry = (Text, Text)


-- Offset/jitter from the remote peer or server presently used as the primary reference
data Ntp =
  Ntp { ntpOffset :: !Double -- ^ Mean offset (phase) in the times reported between this local host and the remote peer or server (RMS, milliseconds);
      , ntpJitter :: !Double -- ^ Mean deviation (jitter) in the time reported for that remote peer or server (RMS of difference of multiple time samples, milliseconds);
      } deriving (Eq, Show)


-- Results of parsing /proc/loadavg
data Loadavg =
  Loadavg { lavgCPU1  :: !Double -- ^ 1 min load avg
          , lavgCPU5  :: !Double -- ^ 5 min load avg
          , lavgCPU10 :: !Double -- ^ 10 min load avg
          , lavgProcRunning :: !Word64 -- ^ number of processes running
          , lavgProcTotal:: !Word64 -- ^ total number of processes
          } deriving (Eq, Show)


-- Results of parsing /proc/stat (https://www.kernel.org/doc/Documentation/filesystems/proc.txt)
data Stat =
  Stat { statCPUUser :: !Word64
       , statCPUNice :: !Word64
       , statCPUSystem :: !Word64
       , statCPUIdle :: !Word64
       , statCPUIOWait :: !Word64
       , statCPUIRQ :: !Word64
       , statCPUSoftIRQ :: !Word64
       , statCPUSteal :: !Word64
       , statCPUGuest :: !Word64
       , statCPUGuestNice :: !Word64
       , statCPUIntr :: !Word64
       , statCPUCtxt :: !Word64
       , statProcsCreated :: !Word64
       } deriving (Eq, Show)


-- Access results of parsing /proc/net/snmp
data NetStatLookup =
  NetStatLookup { netStatLookupIp  :: Text -> Maybe Integer
                , netStatLookupTcp :: Text -> Maybe Integer
                , netStatLookupUdp :: Text -> Maybe Integer
                }

instance Show NetStatLookup where
  show _ = "NetStatLookup"


newtype NSCounter =
  NSCounter {
    unNSCounter:: Map Text Integer
  } deriving (Eq, Show)

instance Monoid NSCounter where
  mempty = NSCounter mempty
  mappend (NSCounter c1) (NSCounter c2) = NSCounter $ Map.unionWith (+) c1 c2


-- Results of parsing /proc/net/snmp
-- Fields are either signed or unsigned 64 bit ints,
-- for convenience they are all treated as Integers
-- to avoid keeping track of semantics of individual fields
--
--  https://www.ietf.org/rfc/rfc1213.txt
--
data NetStat =
  NetStat { netStatIp  :: !NSCounter
          , netStatTcp :: !NSCounter
          , netStatUdp :: !NSCounter
          } deriving (Eq, Show)


instance Monoid NetStat where
  mempty = NetStat mempty mempty mempty

  mappend ns1 ns2 = NetStat (af netStatIp) (af netStatTcp) (af netStatUdp)
    where af f = f ns1 `mappend` f ns2
