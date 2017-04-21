{-# LANGUAGE OverloadedStrings  #-}


module Main where


import           Data.Attoparsec.Text
import qualified Data.Text.IO as TIO
import           Df
import           Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Types


main = defaultMain tests

tests = [
    testGroup "Parser" [
      testCase "parse df output 1" test_parseDfOutput1
    ]
  ]


test_parseDfOutput1 = do
  dfTxt <- TIO.readFile "data/eg-df-output1"
  parseOnly parseDiskFree dfTxt @?= Right [ DfFs "devtmpfs"   "/dev"     2015020       92   2014928
                                          , DfFs "tmpfs"      "/dev/shm" 2023948        0   2023948
                                          , DfFs "/dev/xvda1" "/"        51473000 4941940  46430812
                                          ]
