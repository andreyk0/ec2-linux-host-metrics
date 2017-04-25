{-# LANGUAGE OverloadedStrings  #-}


module Main where


import           Data.Attoparsec.Text
import qualified Data.Text.IO as TIO
import           Df
import           Meminfo
import           Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Types


main = defaultMain tests

tests = [
    testGroup "Parser" [
      testCase "parse df output 1" test_parseDfOutput1
    , testCase "parse meminfo output 1" test_parseMeminfoOutput1
    ]
  ]


test_parseDfOutput1 = do
  dfTxt <- TIO.readFile "data/eg-df-output1"
  parseOnly parseDiskFree dfTxt @?= Right [ DfFs "devtmpfs"   "/dev"     2015020       92   2014928
                                          , DfFs "tmpfs"      "/dev/shm" 2023948        0   2023948
                                          , DfFs "/dev/xvda1" "/"        51473000 4941940  46430812
                                          ]

test_parseMeminfoOutput1 = do
  miTxt <- TIO.readFile "data/meminfo1"
  parseOnly parseMeminfo miTxt @?= Right
    [ MeminfoEntry "MemTotal"         4145049600
    , MeminfoEntry "MemFree"          1197182976
    , MeminfoEntry "MemAvailable"     2755862528
    , MeminfoEntry "Buffers"          211939328
    , MeminfoEntry "Cached"           1302056960
    , MeminfoEntry "SwapCached"       0
    , MeminfoEntry "Active"           887377920
    , MeminfoEntry "Inactive"         790917120
    , MeminfoEntry "Active(anon)"     164306944
    , MeminfoEntry "Inactive(anon)"   90112
    , MeminfoEntry "Active(file)"     723070976
    , MeminfoEntry "Inactive(file)"   790827008
    , MeminfoEntry "Unevictable"      0
    , MeminfoEntry "Mlocked"          0
    , MeminfoEntry "SwapTotal"        0
    , MeminfoEntry "SwapFree"         0
    , MeminfoEntry "Dirty"            4096
    , MeminfoEntry "Writeback"        0
    , MeminfoEntry "AnonPages"        164323328
    , MeminfoEntry "Mapped"           51712000
    , MeminfoEntry "Shmem"            98304
    , MeminfoEntry "Slab"             1244016640
    , MeminfoEntry "SReclaimable"     338235392
    , MeminfoEntry "SUnreclaim"       905781248
    , MeminfoEntry "KernelStack"      2363392
    , MeminfoEntry "PageTables"       3776512
    , MeminfoEntry "NFS_Unstable"     0
    , MeminfoEntry "Bounce"           0
    , MeminfoEntry "WritebackTmp"     0
    , MeminfoEntry "CommitLimit"      2072522752
    , MeminfoEntry "Committed_AS"     473653248
    , MeminfoEntry "VmallocTotal"     35184372087808
    , MeminfoEntry "VmallocUsed"      0
    , MeminfoEntry "VmallocChunk"     0
    , MeminfoEntry "AnonHugePages"    0
    , MeminfoEntry "ShmemHugePages"   0
    , MeminfoEntry "ShmemPmdMapped"   0
    , MeminfoEntry "HugePages_Total"  0
    , MeminfoEntry "HugePages_Free"   0
    , MeminfoEntry "HugePages_Rsvd"   0
    , MeminfoEntry "HugePages_Surp"   0
    , MeminfoEntry "Hugepagesize"     2097152
    , MeminfoEntry "DirectMap4k"      12582912
    , MeminfoEntry "DirectMap2M"      4282384384
    ]
