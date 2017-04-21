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
import           Control.Monad.Trans.Reader
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.GitRev
import           System.Exit


data AppState = AppState { asArgs :: !Args
                           -- more state
                         }


newtype App a =
  App { unApp :: ReaderT AppState (LoggingT IO) a
      } deriving ( Applicative
                 , Functor
                 , Monad
                 , MonadCatch
                 , MonadIO
                 , MonadLogger
                 , MonadLoggerIO
                 , MonadThrow
                 )

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

  let llf _ ll = argsVerbose || ll >= LevelInfo

  runStderrLoggingT $ filterLogger llf $ runReaderT (unApp appA) (AppState args)

