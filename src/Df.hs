{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Df (
  diskFree
, parseDiskFree
) where


import           Control.Monad.Catch (MonadCatch)
import qualified Control.Monad.Catch as MC
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Attoparsec.Text
import           Data.Bifunctor
import           Data.Char
import           Data.Conduit.Attoparsec
import           Data.Conduit.Shell
import qualified Data.Conduit.Text as CT
import           Data.Monoid
import           Types


diskFree :: (MonadCatch m, MonadIO m, MonadLogger m)
         => m (Either String Df)
diskFree = do
  res <- MC.try $ liftIO $ run ( proc "/bin/df" ["-k", "-l", "-P"] $| conduit (CT.decode CT.utf8 =$= sinkParser parseDiskFree) )
  $(logDebugSH) res

  return $ first (\ (MC.SomeException e) -> "Unable to parse output of /bin/df: " <> show e) res


-- Parses the output of Linux's '/bin/df -k -l -P'
parseDiskFree :: Parser Df
parseDiskFree = do
  _ <- (takeTill isEndOfLine <* endOfLine) <?> "header"
  many1' parseDfLine

  where parseDfLine = do fs                <- (takeWhile1 (not . isSpace) <* skipSpace) <?> "fs"
                         kBlocksTotal      <- (decimal <* skipSpace) <?> "total"
                         kBlocksUsed       <- (decimal <* skipSpace) <?> "used"
                         kBlocksAvailable  <- (decimal <* skipSpace) <?> "available"
                         _                 <- (takeWhile1 (not . isSpace) <* skipSpace) <?> "%"
                         mountPoint        <- (takeWhile1 (not . isSpace) <* skipSpace) <?> "mountpoint"
                         return $ DfFs fs mountPoint kBlocksTotal kBlocksUsed kBlocksAvailable
