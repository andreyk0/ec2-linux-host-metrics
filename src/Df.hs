{-# LANGUAGE OverloadedStrings  #-}


module Df (
  diskFree
, parseDiskFree
) where


import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Conduit.Attoparsec
import           Data.Conduit.Shell
import qualified Data.Conduit.Text as CT
import           Data.Monoid
import           Types


diskFree :: (MonadCatch m, MonadIO m)
         => m Df
diskFree =
  catch ( liftIO $ run (proc "/bin/df" ["-k", "-l", "-P"] $| conduit (CT.decode CT.utf8 =$= sinkParser parseDiskFree)) )
        (\ (pe@ParseError{}) -> error $ "Unable to parse output of /bin/df: " <> show pe )


-- Parses the output of Linux's '/bin/df -k -l -P'
parseDiskFree :: Parser Df
parseDiskFree = do
  _ <- takeTill isEndOfLine <* endOfLine -- skip 1st line (header)
  many1' parseDfLine

  where parseDfLine = do fs                <- takeWhile1 (not . isSpace) <* skipSpace
                         kBlocksTotal      <- decimal <* skipSpace
                         kBlocksUsed       <- decimal <* skipSpace
                         kBlocksAvailable  <- decimal <* skipSpace
                         _                 <- takeWhile1 (not . isSpace) <* skipSpace -- % column
                         mountPoint        <- takeWhile1 (not . isSpace) <* skipSpace
                         return $ DfFs fs mountPoint kBlocksTotal kBlocksUsed kBlocksAvailable
