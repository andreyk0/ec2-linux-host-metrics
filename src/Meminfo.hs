{-# LANGUAGE OverloadedStrings  #-}

module Meminfo (
  meminfo
, parseMeminfo
) where


import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Attoparsec.Text as AT
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary
import           Data.Conduit.Shell
import qualified Data.Conduit.Text as CT
import           Data.Maybe
import           Data.Monoid
import           Types


meminfo :: (MonadCatch m, MonadResource m)
        => m Meminfo
meminfo =
  catch ( sourceFile "/proc/meminfo" =$= CT.decode CT.utf8 $$ sinkParser parseMeminfo )
        (\ (pe@ParseError{}) -> error $ "Unable to parse contents of /proc/meminfo: " <> show pe )


-- Parses the linux's /proc/meminfo entries
parseMeminfo :: Parser Meminfo
parseMeminfo = do
  maybeEntries <- many1' $ ( Just          <$> parseMeminfoLine ) -- parse what we can
                       <|> ( const Nothing <$> consumeLine      ) -- skip the rest

  return $ catMaybes maybeEntries

  where consumeLine = (AT.takeWhile (not . isEndOfLine) <* endOfLine) <?> "skipLine"

        kbMultiplier = const 1024 <$> (skipSpace >> string "kB" <* endOfLine) <?> "kB"
        noMultiplier = const 1    <$> skipSpace <?> "noMultiplier"

        parseMeminfoLine = do k          <- (takeWhile1 (/= ':') <* string ":" <* skipSpace) <?> "key"
                              v          <- decimal <?> "val"
                              multiplier <- (kbMultiplier <|> noMultiplier) <?> "maybeMultiplier"

                              return $ MeminfoEntry k (v * multiplier)
