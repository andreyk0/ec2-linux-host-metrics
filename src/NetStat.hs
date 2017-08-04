{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


module NetStat (
  netStatsSinceLastRun
, netStatDiffs
, netStatLookup
, parseNetSnmp
, parseNetStat
) where


import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Attoparsec.Text as AT
import           Data.Char
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word
import           FileDiff
import           Types


-- | Constructs helper functions to lookup diffs (between current and last saved state)
-- in the monotonically incrementing fields.
--
-- All such fields are unsigned 64bit numbers, so we diff via converted Word64 values
-- to avoid overflows.
--
netStatDiffs :: (Maybe NetStat, NetStat)
             -> NetStatLookup

netStatDiffs (Nothing, _) = NetStatLookup (const Nothing) (const Nothing) (const Nothing)

netStatDiffs (Just prevStats, currentStats) = NetStatLookup (diff netStatIp) (diff netStatTcp) (diff netStatUdp)
  where diff field k =
          do pVal <- Map.lookup k ((unNSCounter . field) prevStats)
             cVal <- Map.lookup k ((unNSCounter . field) currentStats)
             return $ fromIntegral $ (fromIntegral cVal ::Word64) - (fromIntegral pVal :: Word64)


-- | Partially applied Map lookups
netStatLookup :: NetStat
              -> NetStatLookup
netStatLookup NetStat{..} = NetStatLookup (lookupWith netStatIp) (lookupWith netStatTcp) (lookupWith netStatUdp)
  where lookupWith (NSCounter field) k = Map.lookup k field


netStatsSinceLastRun :: (MonadCatch m, MonadResource m, MonadLogger m)
                     => m (Either String (Maybe NetStat, NetStat))
netStatsSinceLastRun = do
  eSnmp <- fileDiff "/proc/net/snmp" parseNetSnmp
  eNetstat <- fileDiff "/proc/net/netstat" parseNetStat

  return $ do snmp <- eSnmp
              netstat <- eNetstat
              return $ snmp <> netstat


-- Parses the linux's /proc/net/snmp entries
parseNetSnmp :: Parser NetStat
parseNetSnmp = do
  sectionStats <- parseSectionStats
  NetStat <$> sectionStats "Ip" <*> sectionStats "Tcp" <*> sectionStats "Udp"


-- Parses the linux's /proc/net/netstat entries
parseNetStat :: Parser NetStat
parseNetStat = do
  sectionStats <- parseSectionStats
  NetStat <$> sectionStats "IpExt" <*> sectionStats "TcpExt" <*> return mempty


-- | Helper to parse named sections out of proc net files
parseSectionStats :: (Monad m)
                  => Parser (Text -> m NSCounter)
parseSectionStats = do
  sections <- Map.fromList <$> many1' parseSection

  let sectionStats sn = case Map.lookup sn sections
                          of Nothing -> error $ "Can't find section " <> T.unpack sn
                             Just x  -> (return . NSCounter) x
  return sectionStats


-- | Parses one section into a section name and corresponding KV mappings
parseSection :: Parser (Text, Map Text Integer)
parseSection = do
  (sName1, fNames) <- parseLine (AT.takeWhile1 (not . isSpace))
  (sName2, fVals)  <- parseLine (signed decimal)

  if sName1 == sName2
  then return (sName1, Map.fromList (zip fNames fVals))
  else error $ "Section names don't match: " <> T.unpack sName1 <> " /= " <> T.unpack sName2

  where parseLine :: (Show f) => Parser f -- ^ field parser
                  -> Parser (Text, [f]) -- ^ section name and a list of parsed fields
        parseLine fP = do
          sectionName <- (takeWhile1 (/= ':') <* string ":") <?> "sectionName"
          fields <- many1' (skipWhile isHorizontalSpace >> fP) <?> "fields"
          _ <- endOfLine <?> "EOL"
          return (sectionName, fields)
