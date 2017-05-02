{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Parse (
  parseFile
, parseShellCommandOutput
) where


import           Control.Monad.Catch (MonadCatch)
import qualified Control.Monad.Catch as MC
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Attoparsec.Text as AT
import           Data.Bifunctor
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary
import           Data.Conduit.Shell
import qualified Data.Conduit.Text as CT
import           Data.Monoid


parseFile :: (MonadCatch m, MonadResource m, MonadLogger m, Show a)
          => Parser a
          -> FilePath
          -> m (Either String a)
parseFile p fP = do
  res <- MC.try $ sourceFile fP =$= CT.decode CT.utf8 $$ sinkParser p
  $(logDebugSH) res
  return $ first (\ (MC.SomeException e) ->  "Unable to parse contents of " <> fP <> ": " <> show e) res


parseShellCommandOutput :: (MonadCatch m, MonadResource m, MonadLogger m, Show a)
                        => Parser a
                        -> String
                        -> Segment ()
                        -> m (Either String a)
parseShellCommandOutput p msg s = do
  res <- MC.try $ liftIO $ run ( s $| conduit (CT.decode CT.utf8 =$= sinkParser p) )
  $(logDebugSH) res
  return $ first (\ (MC.SomeException e) -> "Unable to parse output of " <> msg <> ": " <> show e) res
