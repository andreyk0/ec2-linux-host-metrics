{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Main where

import App
import Args
import Control.Monad.IO.Class
import Control.Monad.Logger
import Df


main:: IO ()
main = runApp $ do
  args@Args{..} <- getArgs
  liftIO $ print args
  $(logInfoSH) args

  df <- diskFree

  $(logInfoSH) df