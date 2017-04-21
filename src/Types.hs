module Types (
  Df
, DfFs(..)
) where


import Data.Text (Text)


-- All 'df' output
type Df = [DfFs]

-- A line of 'df' output
data DfFs = DfFs
  { dffsFilesystem :: !Text
  , dffsMountpoint :: !Text
  , dffsCapacity :: !Int
  , dffsUsed :: !Int
  , dffsAvailable :: !Int
  } deriving (Eq, Show)
