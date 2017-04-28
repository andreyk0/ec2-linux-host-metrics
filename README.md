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

```cron
# Send host metrics every minute
* * * * * root   /usr/local/bin/ec2-linux-host-metrics --publish-metrics --metric-dimensions my-service-name=test 2>&1 | /usr/bin/logger -t ec2-linux-host-metrics
```


# Examples

To publish generated metrics use -p flag:
```bash
ec2-linux-host-metrics -p -d test=test
```

To view what would be published:

```bash
ec2-linux-host-metrics -d test=test,InstanceId=INSTANCE_ID
```

generates something like

```
MetricDatum' {_mdValue = Just 16.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "success"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "error"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
MetricDatum' {_mdValue = Just 92.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2014928.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 4.565711506585543e-3, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2023948.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 4754860.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 4.6617892e7, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 9.237580867639345, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 4.1450496e9, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemTotal"}
MetricDatum' {_mdValue = Just 1.133785088e9, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemFree"}
MetricDatum' {_mdValue = Just 2.753396736e9, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemAvailable"}
MetricDatum' {_mdValue = Just 1.43710208e9, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsed"}
MetricDatum' {_mdValue = Just 34.67032288347044, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsedPercent"}
MetricDatum' {_mdValue = Just 0.574, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpOffsetAbs"}
MetricDatum' {_mdValue = Just 0.112, _mdDimensions = Just [Dimension' {_dName = "test", _dValue = "test"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpJitterAbs"}
```
