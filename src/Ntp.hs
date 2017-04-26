{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Ntp (
  ntpQuery
, parseNtpOffset
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


ntpQuery :: (MonadCatch m, MonadIO m, MonadLogger m)
         => m (Either String Ntp)
ntpQuery = do
  res <- MC.try $ liftIO $ run (proc "/usr/sbin/ntpq" ["-np"] $| conduit (CT.decode CT.utf8 =$= sinkParser parseNtpOffset))
  $(logDebugSH) res

  return $ first (\ (MC.SomeException e) -> "Can't parse 'primary reference' offset from the output of ntpq -np: [" <> show e <>
                                        "], expected to find a line starting with a '*' character, this may mean that NTP server is down or there are connectivity issues!"
                 ) res


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
