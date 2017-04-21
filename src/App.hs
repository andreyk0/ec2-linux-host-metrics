{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module App (
  getArgs
, isVerbose
, runApp
, tshow
) where

import           Args
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.GitRev
import           System.Exit


data AppState = AppState { asArgs :: !Args
                         , asAWSEnv :: !Env
                         }


newtype App a =
  App { unApp :: ReaderT AppState (AWST' AppState (ResourceT (LoggingT IO))) a
      } deriving ( Applicative
                 , Functor
                 , Monad
                 , MonadCatch
                 , MonadIO
                 , MonadLogger
                 , MonadLoggerIO
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

tshow :: (Show a) => a -> Text
tshow = T.pack . show


runApp :: App a
       -> IO a
runApp appA  = do
  args@Args{..} <- parseCliArgs

  when argsVersion $ die $ "Version: " <> $(gitBranch) <> "@" <> $(gitHash)

  env <- newEnv Discover

  let llf _ ll = argsVerbose || ll >= LevelInfo
      appState = AppState args env

  runStderrLoggingT $ filterLogger llf $
    runResourceT $ runAWST appState $
      runReaderT (unApp appA) appState
