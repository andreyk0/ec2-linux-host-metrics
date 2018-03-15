{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module App (
  App
, InstanceID
, getArgs
, getInstanceID
, isVerbose
, runApp
, tshow
) where

import           Args
import           Control.Lens
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Reader hiding (ask)
import           Control.Monad.Trans.Resource
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.GitRev
import           Network.AWS.EC2.Metadata
import           System.Exit

type InstanceID = Text


data AppState = AppState { asArgs :: !Args
                         , asAWSEnv :: !Env
                         , asInstanceID :: !InstanceID
                         }


newtype App a =
  App { unApp :: ReaderT AppState (AWST' AppState (ResourceT (LoggingT IO))) a
      } deriving ( Applicative
                 , Functor
                 , Monad
                 , MonadBase IO
                 , MonadCatch
                 , MonadIO
                 , MonadLogger
                 , MonadLoggerIO
                 , MonadReader AppState
                 , MonadResource
                 , MonadThrow
                 )

-- AWST' doesn't integrate with MonadLogger by default, add a couple of instances,
-- default implementations should be fine
instance MonadLogger m => MonadLogger (AWST' r m)
instance MonadLoggerIO m => MonadLoggerIO (AWST' r m)


-- | Amazonka AWS environment
instance HasEnv AppState where
  environment f as@AppState{..} = fmap (\e -> as { asAWSEnv = e}) (f asAWSEnv)


isVerbose :: App Bool
isVerbose = fmap argsVerbose getArgs

getArgs :: App Args
getArgs = App $ fmap asArgs ask

getInstanceID :: App InstanceID
getInstanceID = App $ fmap asInstanceID ask

tshow :: (Show a) => a -> Text
tshow = T.pack . show


runApp :: App a
       -> IO a
runApp appA  = do
  cliArgs@Args{..} <- parseCliArgs

  -- if none of the more specific options are given - ask for all metrics by default
  let args = if ( argsCPU
               || argsMemory
               || isJust argsDisk
               || argsNTP
               || argsNetIP
               || argsNetTCP
               || argsNetUDP
                )
             then cliArgs
             else cliArgs { argsCPU = True
                          , argsMemory = True
                          , argsDisk = Just []
                          , argsNTP = True
                          , argsNetIP = True
                          , argsNetTCP = True
                          , argsNetUDP = True
                          }

  when argsVersion $ die $ "Version: " <> $(gitBranch) <> "@" <> $(gitHash)

  env <- newEnv Discover

  myID <- identity (env ^. envManager) >>= \idRes -> case idRes
                                                       of Left e -> error $ "Unable to determine EC2 instance identity: " <> e
                                                          Right iDoc -> return $ iDoc ^. instanceId

  let llf _ ll = argsVerbose || ll >= LevelInfo
      appState = AppState args env myID

  runStderrLoggingT $ filterLogger llf $
    runResourceT $ runAWST appState $
      runReaderT (unApp appA) appState
