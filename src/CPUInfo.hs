{-# LANGUAGE OverloadedStrings  #-}


module CPUInfo (
  cpuinfo
, cpuinfoSummary
, parseCPUInfo
, summarizeCPUInfo
) where


import           Control.Applicative
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Attoparsec.Text as AT
import           Data.List (nub)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Parse
import           Types


cpuinfoSummary :: (MonadCatch m, MonadResource m, MonadLogger m)
               => m (Either String CPUInfoSummary)
cpuinfoSummary = (>>= summarizeCPUInfo) <$> cpuinfo


cpuinfo :: (MonadCatch m, MonadResource m, MonadLogger m)
        => m (Either String CPUInfo)
cpuinfo = parseFile parseCPUInfo "/proc/cpuinfo"


summarizeCPUInfo :: CPUInfo
                 -> Either String CPUInfoSummary
summarizeCPUInfo cpui =
  if null physCores
  then Left $ "Unable to parse cores from cpuinfo result " <> show cpui
  else Right $ CPUInfoSummary $ fromIntegral $ length $ nub physCores

  where entryMaps = Map.fromList <$> cpui -- [Map Text Text]

        onePhysCore m = do pid <- Map.lookup "physical id" m
                           ci  <- Map.lookup "core id" m
                           return (pid, ci)

        physCores = catMaybes $ onePhysCore <$> entryMaps


-- Parses the linux's /proc/cpuinfo entries
parseCPUInfo :: Parser CPUInfo
parseCPUInfo = many1' parseCPUInfoSection <?> "CPUInfoSections"

  where endOfSection = (endOfLine <|> endOfInput) <?> "endOfSection"

        parseCPUInfoLine = do k <- (takeWhile1 (notInClass ":\n\r") <* char ':')  <?> "key"
                              v <- (AT.takeTill isEndOfLine <* endOfLine) <?> "val"
                              return (T.strip k, T.strip v)

        parseCPUInfoSection = (many1' parseCPUInfoLine <* endOfSection) <?> "Section"
