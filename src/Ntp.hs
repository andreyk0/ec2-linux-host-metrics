{-# LANGUAGE OverloadedStrings  #-}

module Ntp (
  ntpQuery
, parseNtpOffset
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


ntpQuery :: (MonadCatch m, MonadIO m, MonadLogger m, MonadResource m)
         => m (Either String Ntp)
ntpQuery = parseShellCommandOutput parseNtpOffset
                                   "/usr/sbin/ntpq (expected to find a line starting with a '*' character, this may mean that NTP server is down or there are connectivity issues)"
                                   (proc "/usr/sbin/ntpq" ["-np"])


-- Parses offset/jitter from the ntp server selected as a primary reference
-- (the ntpq -np line starting with '*')
parseNtpOffset :: Parser Ntp
parseNtpOffset = do
  _ <- takeWhile1 (/= '*') <?> "headerAndNonPrimaryRefs"
  _ <- char '*' <?> "primaryReferenceLine"
  _ <- skipColumn "remote"
  _ <- skipColumn "refid"
  _ <- skipColumn "st"
  _ <- skipColumn "t"
  _ <- skipColumn "when"
  _ <- skipColumn "poll"
  _ <- skipColumn "reach"
  _ <- skipColumn "delay"

  offset <- (double <* skipSpace) <?> "offset"
  jitter <- (double <* takeText) <?> "jitter"

  return $ Ntp offset jitter

  where skipColumn cname = (takeWhile1 (not . isSpace) <* skipSpace) <?> cname
