# ec2-linux-host-metrics

Publishes a few metrics about linux hosts to CloudWatch.
At the moment we look at ntp offset, disk and memory utilization.
Metrics are published under `System/Linux` CW namespace.
Running without `-p` flag should just display a full list of metrics that would be published..

It also reports a namesake Ec2LinuxHostMetricsAgent metric with a `result` dimension which captures
counts of published metrics and execution errors.
Execution errors should remain 0 at all times and should be alerted on in CW.

Same metrics are published with different sets of dimensions to support different types of grouping
in CloudWatch console. At least one set of dimensions is required.
A special dimension value `INSTANCE_ID` will be replaced with an actual instance ID.

```
Publishes a few metrics about linux hosts to CloudWatch.

Usage: ec2-linux-host-metrics [-v|--verbose] [-V|--version]
                              [-p|--publish-metrics]
                              [-d|--metric-dimensions name=val[,name1=val1]]

Available options:
  -h,--help                Show this help text
  -v,--verbose             Be verbose.
  -V,--version             Print version and exit.
  -p,--publish-metrics     Publish generated metrics to CloudWatch.
  -d,--metric-dimensions name=val[,name1=val1]
                           Dimension(s) to attach to all generated data. A
                           special value INSTANCE_ID will be replaced with an
                           actual current instance ID.

Source: https://github.com/gilt/ec2-linux-host-metrics

At least one set of dimensions is required. A special dimension value INSTANCE_ID will be replaced with an actual current instance ID.

Metrics will be published once per each given set of dimensions to support different levels of grouping.

E.g. command:

	ec2-linux-host-metrics --publish-metrics --metric-dimensions my-app=test --metric-dimensions my-app=test,InstanceId=INSTANCE_ID
```

# Building

Requires [haskell stack](https://docs.haskellstack.org/en/stable/README/)
```bash
make bindist
```

Building a statically linked linux binary via a docker container (requires [someting like](https://github.com/andreyk0/docker-haskell-platform-alpine)):

```bash
docker-haskell-platform-alpine make bindist
```

# Deployment

You can run it periodically from a crontab:

```shell
cat > /etc/cron.d/ec2-linux-host-metrics  <<_E_
# Send host metrics every minute
* * * * * root sleep $(($RANDOM % 10)) ; /usr/local/bin/ec2-linux-host-metrics --publish-metrics --metric-dimensions my-service-name=test 2>&1 | /usr/bin/logger -t ec2-linux-host-metrics
_E_

# Pick up new crontab
/etc/init.d/crond reload
```

Some random delay (but consistent between runs) is there to reduce herding effect.


# Examples

To publish generated metrics use -p flag:
```bash
ec2-linux-host-metrics -p -d test=test
```

To view what would be published:

```bash
ec2-linux-host-metrics -d test=test,InstanceId=INSTANCE_ID
```


Performance of the app itself:
```
MetricDatum' {_mdValue = Just 22.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "success"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "error"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
```

Metrics derived from `/proc/loadavg`. Load numbers are normalized per core, numbers of running/total processes are absolute.
```
MetricDatum' {_mdValue = Just 14.000000000000002, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "CPULoadAvgPerCore1"}
MetricDatum' {_mdValue = Just 21.5, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "CPULoadAvgPerCore5"}
MetricDatum' {_mdValue = Just 10.5, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "CPULoadAvgPerCore10"}
MetricDatum' {_mdValue = Just 1.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "ProcessesRunning"}
MetricDatum' {_mdValue = Just 131.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "ProcessesTotal"}
```
See [understanding-load-averages](http://blog.scoutapp.com/articles/2009/07/31/understanding-load-averages)


Output of `/bin/df -k -l -P`, by mountpoint:
```
MetricDatum' {_mdValue = Just 92.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2014928.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 4.565711506585543e-3, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2023948.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 4794620.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 4.6578132e7, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 9.31482524818837, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
```


Metrics derived from `/proc/meminfo`:
```
MetricDatum' {_mdValue = Just 4.1450496e9, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemTotal"}
MetricDatum' {_mdValue = Just 1.090072576e9, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemFree"}
MetricDatum' {_mdValue = Just 2.751766528e9, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemAvailable"}
MetricDatum' {_mdValue = Just 66.38681785617234, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemAvailablePercent"}
MetricDatum' {_mdValue = Just 1.439801344e9, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsed"}
MetricDatum' {_mdValue = Just 34.73544306924578, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsedPercent"}
```
See [Interpreting /proc/meminfo ...](https://access.redhat.com/solutions/406773)


NTP metrics derived from `/usr/sbin/ntpq -np`:
```
MetricDatum' {_mdValue = Just 1.865, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpOffsetAbs"}
MetricDatum' {_mdValue = Just 0.879, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpJitterAbs"}
```


CPU metrics (derived from `/proc/stat`) only show up on a second run (current /proc/stat values are diffed against previous ones saved to /tmp/ec2-linux-host-metrics-proc-stat each time it runs).
The assumption is that it'll run from cron at a regular interval.
```
MetricDatum' {_mdValue = Just 0.8153281695882593, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "CPUUtilizationPercent"}
MetricDatum' {_mdValue = Just 0.6522625356706074, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "CPUUtilizationUserPercent"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "CPUUtilizationNicePercent"}
MetricDatum' {_mdValue = Just 0.16306563391765186, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "CPUUtilizationSystemPercent"}
MetricDatum' {_mdValue = Just 1289.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "CPUInterrupts"}
MetricDatum' {_mdValue = Just 3812.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "CPUContextSwitches"}
MetricDatum' {_mdValue = Just 22.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "ProcessesCreated"}
```


Host network metrics from [/proc/net/snmp](https://www.ietf.org/rfc/rfc1213.txt), only non-0 fields are reported, gauges remain gauges, counters are diffed against previous run.
```
MetricDatum' {_mdValue = Just 1.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NetStatTcpCurrEstab"}
MetricDatum' {_mdValue = Just 120.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NetStatIpInDelivers"}
MetricDatum' {_mdValue = Just 120.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NetStatIpInReceives"}
MetricDatum' {_mdValue = Just 103.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NetStatIpOutRequests"}
MetricDatum' {_mdValue = Just 6.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NetStatTcpActiveOpens"}
MetricDatum' {_mdValue = Just 102.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NetStatTcpInSegs"}
MetricDatum' {_mdValue = Just 85.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NetStatTcpOutSegs"}
MetricDatum' {_mdValue = Just 18.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NetStatUdpInDatagrams"}
MetricDatum' {_mdValue = Just 18.0, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NetStatUdpOutDatagrams"}
```


Subset of metrics from [/proc/net/netstat](https://github.com/ecki/net-tools/blob/master/statistics.c).
You can see which ones in [Main.hs](src/Main.hs). There's a lot there, so, to keep the CW costs down only non-0 values are published.
