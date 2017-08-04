{-# LANGUAGE OverloadedStrings  #-}


module Main where



import           CPUInfo
import           Data.Attoparsec.Text
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import           Df
import           Loadavg
import           Meminfo
import           NetStat
import           Ntp
import           Stat
import           Test.Framework (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Types


main = defaultMain tests

tests = [
    testGroup "Parser" [
      testCase "parse df output 1" test_parseDfOutput1
    , testCase "parse meminfo output 1" test_parseMeminfoOutput1
    , testCase "parse ntp offset from selected peer" test_parseNtpOffset1
    , testCase "parse cpuinfo" test_parseCPUInfo1
    , testCase "summarize cpuinfo" test_summarizeCPUInfo1
    , testCase "parse loadavg" test_parseLoadavg1
    , testCase "parse stat" test_parseStat1
    , testCase "parse net stats" test_parseNetStats1
    , testCase "parse snmp net stats" test_parseSNMPNetStats1
    ]
  ]


test_parseDfOutput1 = do
  dfTxt <- TIO.readFile "data/df1"
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


test_parseNtpOffset1 = do
  ntpqTxt <- TIO.readFile "data/ntpq1"
  parseOnly parseNtpOffset ntpqTxt @?= Right Ntp {ntpOffset = 1.064, ntpJitter = 0.299}


test_parseCPUInfo1 = do
  cpuiTxt <- TIO.readFile "data/cpuinfo1"
  parseOnly parseCPUInfo cpuiTxt @?= Right [
      [ ("processor","0")
      , ("vendor_id","GenuineIntel")
      , ("cpu family","6")
      , ("model","63")
      , ("model name","Intel(R) Xeon(R) CPU E5-2676 v3 @ 2.40GHz")
      , ("stepping","2")
      , ("microcode","0x25")
      , ("cpu MHz","2394.698")
      , ("cache size","30720 KB")
      , ("physical id","0")
      , ("siblings","2")
      , ("core id","0")
      , ("cpu cores","2")
      , ("apicid","0")
      , ("initial apicid","0")
      , ("fpu","yes")
      , ("fpu_exception","yes")
      , ("cpuid level","13")
      , ("wp","yes")
      , ("flags","fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ht syscall nx rdtscp lm constant_tsc rep_good nopl xtopology eagerfpu pni pclmulqdq ssse3 fma cx16 pcid sse4_1 sse4_2 x2apic movbe popcnt tsc_deadline_timer aes xsave avx f16c rdrand hypervisor lahf_lm abm fsgsbase bmi1 avx2 smep bmi2 erms invpcid xsaveopt")
      , ("bugs","")
      , ("bogomips","4789.07")
      , ("clflush size","64")
      , ("cache_alignment","64")
      , ("address sizes","46 bits physical, 48 bits virtual")
      , ("power management","")
      ]
    , [ ("processor","1")
      , ("vendor_id","GenuineIntel")
      , ("cpu family","6")
      , ("model","63")
      , ("model name","Intel(R) Xeon(R) CPU E5-2676 v3 @ 2.40GHz")
      , ("stepping","2")
      , ("microcode","0x25")
      , ("cpu MHz","2394.698")
      , ("cache size","30720 KB")
      , ("physical id","0")
      , ("siblings","2")
      , ("core id","1")
      , ("cpu cores","2")
      , ("apicid","2")
      , ("initial apicid","2")
      , ("fpu","yes")
      , ("fpu_exception","yes")
      , ("cpuid level","13")
      , ("wp","yes")
      , ("flags","fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ht syscall nx rdtscp lm constant_tsc rep_good nopl xtopology eagerfpu pni pclmulqdq ssse3 fma cx16 pcid sse4_1 sse4_2 x2apic movbe popcnt tsc_deadline_timer aes xsave avx f16c rdrand hypervisor lahf_lm abm fsgsbase bmi1 avx2 smep bmi2 erms invpcid xsaveopt")
      , ("bugs","")
      , ("bogomips","4848.29")
      , ("clflush size","64")
      , ("cache_alignment","64")
      , ("address sizes","46 bits physical, 48 bits virtual")
      ]
    ]


test_summarizeCPUInfo1 = do
  cpuiTxt <- TIO.readFile "data/cpuinfo1"
  let r = parseOnly parseCPUInfo cpuiTxt
  (r >>= summarizeCPUInfo) @?= (Right $ CPUInfoSummary 2)


test_parseLoadavg1 = do
  loadavgTxt <- TIO.readFile "data/loadavg1"
  parseOnly parseLoadavg loadavgTxt @?= Right
        Loadavg { lavgCPU1 = 0.49
                , lavgCPU5 = 0.28
                , lavgCPU10 = 0.21
                , lavgProcRunning = 1
                , lavgProcTotal = 172
                }


test_parseStat1 = do
  statTxt <- TIO.readFile "data/stat1"
  parseOnly parseStat statTxt @?= Right
        Stat { statCPUUser = 13477
             , statCPUNice = 330
             , statCPUSystem = 2323
             , statCPUIdle = 69157680
             , statCPUIOWait = 16418
             , statCPUIRQ = 0
             , statCPUSoftIRQ = 114
             , statCPUSteal = 2254
             , statCPUGuest = 1
             , statCPUGuestNice = 2
             , statCPUIntr = 16080195
             , statCPUCtxt = 56244086
             , statProcsCreated = 44862
             }


test_parseSNMPNetStats1 = do
  statTxt <- TIO.readFile "data/snmp1"
  parseOnly parseNetSnmp statTxt @?= Right
    NetStat { netStatIp = NSCounter $ Map.fromList [
                  ("DefaultTTL",255)
                , ("ForwDatagrams",18675692)
                , ("Forwarding",1)
                , ("FragCreates",0)
                , ("FragFails",0)
                , ("FragOKs",0)
                , ("InAddrErrors",2)
                , ("InDelivers",4895649)
                , ("InDiscards",0)
                , ("InHdrErrors",0)
                , ("InReceives",23571343)
                , ("InUnknownProtos",0)
                , ("OutDiscards",0)
                , ("OutNoRoutes",0)
                , ("OutRequests",23586621)
                , ("ReasmFails",0)
                , ("ReasmOKs",0)
                , ("ReasmReqds",0)
                , ("ReasmTimeout",0)
                ]

            , netStatTcp = NSCounter $ Map.fromList [
                  ("ActiveOpens",429421)
                , ("AttemptFails",0)
                , ("CurrEstab",3)
                , ("EstabResets",62680)
                , ("InCsumErrors",0)
                , ("InErrs",0)
                , ("InSegs",4164006)
                , ("MaxConn",-1)
                , ("OutRsts",77923)
                , ("OutSegs",4734084)
                , ("PassiveOpens",3)
                , ("RetransSegs",60)
                , ("RtoAlgorithm",1)
                , ("RtoMax",120000)
                , ("RtoMin",200)
                ]

            , netStatUdp = NSCounter $ Map.fromList [
                  ("IgnoredMulti",0)
                , ("InCsumErrors",0)
                , ("InDatagrams",731623)
                , ("InErrors",0)
                , ("NoPorts",13)
                , ("OutDatagrams",731721)
                , ("RcvbufErrors",0)
                , ("SndbufErrors",0)
                ]
            }


test_parseNetStats1 = do
  statTxt <- TIO.readFile "data/netstat1"
  parseOnly parseNetStat statTxt @?= Right
    NetStat { netStatIp = NSCounter $ Map.fromList [
                 ("InBcastOctets",0)
                ,("InBcastPkts",0)
                ,("InCEPkts",0)
                ,("InCsumErrors",0)
                ,("InECT0Pkts",0)
                ,("InECT1Pkts",0)
                ,("InMcastOctets",0)
                ,("InMcastPkts",0)
                ,("InNoECTPkts",24689703)
                ,("InNoRoutes",0)
                ,("InOctets",7349649097)
                ,("InTruncatedPkts",0)
                ,("OutBcastOctets",0)
                ,("OutBcastPkts",0)
                ,("OutMcastOctets",0)
                ,("OutMcastPkts",0)
                ,("OutOctets",13125660443)
                ]

            , netStatTcp = NSCounter $ Map.fromList [
                 ("ArpFilter",0)
                ,("BusyPollRxPackets",0)
                ,("DelayedACKLocked",0)
                ,("DelayedACKLost",11)
                ,("DelayedACKs",85149)
                ,("EmbryonicRsts",0)
                ,("IPReversePathFilter",0)
                ,("ListenDrops",0)
                ,("ListenOverflows",0)
                ,("LockDroppedIcmps",0)
                ,("OfoPruned",0)
                ,("OutOfWindowIcmps",0)
                ,("PAWSActive",0)
                ,("PAWSEstab",0)
                ,("PAWSPassive",0)
                ,("PruneCalled",16)
                ,("RcvPruned",0)
                ,("SyncookiesFailed",0)
                ,("SyncookiesRecv",0)
                ,("SyncookiesSent",0)
                ,("TCPACKSkippedChallenge",0)
                ,("TCPACKSkippedFinWait2",0)
                ,("TCPACKSkippedPAWS",0)
                ,("TCPACKSkippedSeq",0)
                ,("TCPACKSkippedSynRecv",0)
                ,("TCPACKSkippedTimeWait",1)
                ,("TCPAbortFailed",0)
                ,("TCPAbortOnClose",57809)
                ,("TCPAbortOnData",9767)
                ,("TCPAbortOnLinger",0)
                ,("TCPAbortOnMemory",0)
                ,("TCPAbortOnTimeout",1)
                ,("TCPAutoCorking",72412)
                ,("TCPBacklogDrop",0)
                ,("TCPChallengeACK",0)
                ,("TCPDSACKIgnoredNoUndo",0)
                ,("TCPDSACKIgnoredOld",0)
                ,("TCPDSACKOfoRecv",0)
                ,("TCPDSACKOfoSent",0)
                ,("TCPDSACKOldSent",2)
                ,("TCPDSACKRecv",0)
                ,("TCPDSACKUndo",0)
                ,("TCPDeferAcceptDrop",0)
                ,("TCPDirectCopyFromBacklog",0)
                ,("TCPDirectCopyFromPrequeue",0)
                ,("TCPFACKReorder",0)
                ,("TCPFastOpenActive",0)
                ,("TCPFastOpenActiveFail",0)
                ,("TCPFastOpenCookieReqd",0)
                ,("TCPFastOpenListenOverflow",0)
                ,("TCPFastOpenPassive",0)
                ,("TCPFastOpenPassiveFail",0)
                ,("TCPFastRetrans",4)
                ,("TCPForwardRetrans",0)
                ,("TCPFromZeroWindowAdv",0)
                ,("TCPFullUndo",4)
                ,("TCPHPAcks",193647)
                ,("TCPHPHits",713491)
                ,("TCPHPHitsToUser",0)
                ,("TCPHystartDelayCwnd",0)
                ,("TCPHystartDelayDetect",0)
                ,("TCPHystartTrainCwnd",6564)
                ,("TCPHystartTrainDetect",399)
                ,("TCPKeepAlive",0)
                ,("TCPLossFailures",0)
                ,("TCPLossProbeRecovery",0)
                ,("TCPLossProbes",1)
                ,("TCPLossUndo",29)
                ,("TCPLostRetransmit",0)
                ,("TCPMD5Failure",0)
                ,("TCPMD5NotFound",0)
                ,("TCPMD5Unexpected",0)
                ,("TCPMTUPFail",0)
                ,("TCPMTUPSuccess",0)
                ,("TCPMemoryPressures",0)
                ,("TCPMinTTLDrop",0)
                ,("TCPOFODrop",0)
                ,("TCPOFOMerge",0)
                ,("TCPOFOQueue",10345)
                ,("TCPOrigDataSent",2525031)
                ,("TCPPartialUndo",0)
                ,("TCPPrequeueDropped",0)
                ,("TCPPrequeued",0)
                ,("TCPPureAcks",1469759)
                ,("TCPRcvCoalesce",762243)
                ,("TCPRcvCollapsed",2943)
                ,("TCPRenoFailures",0)
                ,("TCPRenoRecovery",4)
                ,("TCPRenoRecoveryFail",0)
                ,("TCPRenoReorder",0)
                ,("TCPReqQFullDoCookies",0)
                ,("TCPReqQFullDrop",0)
                ,("TCPRetransFail",0)
                ,("TCPSACKDiscard",0)
                ,("TCPSACKReneging",0)
                ,("TCPSACKReorder",0)
                ,("TCPSYNChallenge",0)
                ,("TCPSackFailures",0)
                ,("TCPSackMerged",0)
                ,("TCPSackRecovery",0)
                ,("TCPSackRecoveryFail",0)
                ,("TCPSackShiftFallback",3)
                ,("TCPSackShifted",0)
                ,("TCPSchedulerFailed",0)
                ,("TCPSlowStartRetrans",5)
                ,("TCPSpuriousRTOs",3)
                ,("TCPSpuriousRtxHostQueues",1)
                ,("TCPSynRetrans",14)
                ,("TCPTSReorder",0)
                ,("TCPTimeWaitOverflow",0)
                ,("TCPTimeouts",36)
                ,("TCPToZeroWindowAdv",0)
                ,("TCPWantZeroWindowAdv",76)
                ,("TCPWinProbe",0)
                ,("TW",170331)
                ,("TWKilled",0)
                ,("TWRecycled",0)
                ]

            , netStatUdp = mempty
            }
