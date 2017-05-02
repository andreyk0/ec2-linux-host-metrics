{-# LANGUAGE OverloadedStrings  #-}


module Df (
  diskFree
, parseDiskFree
) where


import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Conduit.Shell
import           Parse
import           Types


diskFree :: (MonadCatch m, MonadIO m, MonadLogger m, MonadResource m)
         => m (Either String Df)
diskFree = parseShellCommandOutput parseDiskFree "/bin/df" (proc "/bin/df" ["-k", "-l", "-P"])


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
