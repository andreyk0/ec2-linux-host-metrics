{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Meminfo (
  meminfo
, parseMeminfo
) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import qualified Control.Monad.Catch as MC
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Attoparsec.Text as AT
import           Data.Bifunctor
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary
import           Data.Conduit.Shell
import qualified Data.Conduit.Text as CT
import           Data.Maybe
import           Data.Monoid
import           Types


meminfo :: (MonadCatch m, MonadResource m, MonadLogger m)
        => m (Either String Meminfo)
meminfo = do
  res <- MC.try $ sourceFile "/proc/meminfo" =$= CT.decode CT.utf8 $$ sinkParser parseMeminfo
  $(logDebugSH) res

  return $ first (\ (MC.SomeException e) ->  "Unable to parse contents of /proc/meminfo: " <> show e) res


-- Parses the linux's /proc/meminfo entries
parseMeminfo :: Parser Meminfo
parseMeminfo = do
  maybeEntries <- many1' $ ( Just          <$> parseMeminfoLine ) -- parse what we can
                       <|> ( const Nothing <$> consumeLine      ) -- skip the rest

  let res = catMaybes maybeEntries

  when (null res) $ error "No meminfo results"
  return res

  where consumeLine = (AT.takeWhile (not . isEndOfLine) <* endOfLine) <?> "skipLine"

        kbMultiplier = const 1024 <$> (skipSpace >> string "kB" <* endOfLine) <?> "kB"
        noMultiplier = const 1    <$> skipSpace <?> "noMultiplier"

        parseMeminfoLine = do k          <- (takeWhile1 (/= ':') <* string ":" <* skipSpace) <?> "key"
                              v          <- decimal <?> "val"
                              multiplier <- (kbMultiplier <|> noMultiplier) <?> "maybeMultiplier"

                              return $ MeminfoEntry k (v * multiplier)
