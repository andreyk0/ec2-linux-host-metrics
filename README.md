# ec2-linux-host-metrics

Publishes a few metrics about linux hosts to CloudWatch.
At the moment we look at ntp offset, disk and memory utilization.
Metrics are published under `System/Linux` CW namespace.
Running without `-p` flag should just display a full list of metrics that would be published..

It also reports a namesake Ec2LinuxHostMetricsAgent metric with a `result` dimension which captures
counts of published metrics and execution errors.
Execution errors should remain 0 at all times and should be alerted on in CW.

Same metrics are published with different sets of dimensions to support different types of grouping
in CloudWatch console. At the minimum one set of metrics is published with an `InstanceId` dimension
to debug individual hosts. Additional groups of metric dimensions can be given on the command line.

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
                           Extra dimension(s) to attach to all generated data.

Source: https://github.com/gilt/ec2-linux-host-metrics

Each set of metrics will be published with InstanceId dimension.

In addition it'll be published once per each --metric-dimensions option value with dimensions specified by the option.
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

# Examples

To publish generated metrics use -p flag:
```bash
ec2-linux-host-metrics -p
```

To view what would be published:

```bash
ec2-linux-host-metrics
```

generates something like

```
MetricDatum' {_mdValue = Just 16.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "success"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "error"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
MetricDatum' {_mdValue = Just 92.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2014928.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 4.565711506585543e-3, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2023948.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 4701740.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 4.6671012e7, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 9.134381131855536, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 4.1450496e9, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemTotal"}
MetricDatum' {_mdValue = Just 1.193005056e9, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemFree"}
MetricDatum' {_mdValue = Just 2.75470336e9, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemAvailable"}
MetricDatum' {_mdValue = Just 1.435041792e9, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsed"}
MetricDatum' {_mdValue = Just 34.62061809827318, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsedPercent"}
MetricDatum' {_mdValue = Just (-1.144), _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpOffset"}
MetricDatum' {_mdValue = Just 0.39, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpJitter"}
```


```bash
ec2-linux-host-metrics -d foo=bar,blah=baz
```

generates metrics with extra dimensions

```
MetricDatum' {_mdValue = Just 16.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "success"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "error"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
MetricDatum' {_mdValue = Just 92.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2014928.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 4.565711506585543e-3, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2023948.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 4701744.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 4.6671008e7, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 9.134388902919978, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 4.1450496e9, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemTotal"}
MetricDatum' {_mdValue = Just 1.193070592e9, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemFree"}
MetricDatum' {_mdValue = Just 2.754772992e9, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemAvailable"}
MetricDatum' {_mdValue = Just 1.43497216e9, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsed"}
MetricDatum' {_mdValue = Just 34.61893821487685, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsedPercent"}
MetricDatum' {_mdValue = Just (-1.144), _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpOffset"}
MetricDatum' {_mdValue = Just 0.39, _mdDimensions = Just [Dimension' {_dName = "InstanceId", _dValue = "i-01c15908108dd68f9"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpJitter"}
MetricDatum' {_mdValue = Just 16.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "success"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "result", _dValue = "error"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Count, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "Ec2LinuxHostMetricsAgent"}
MetricDatum' {_mdValue = Just 92.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2014928.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 4.565711506585543e-3, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 2023948.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 0.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/dev/shm"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 4701744.0, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUsed"}
MetricDatum' {_mdValue = Just 4.6671008e7, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Kilobytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceAvailable"}
MetricDatum' {_mdValue = Just 9.134388902919978, _mdDimensions = Just [Dimension' {_dName = "Mountpoint", _dValue = "/"},Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "DiskSpaceUtilization"}
MetricDatum' {_mdValue = Just 4.1450496e9, _mdDimensions = Just [Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemTotal"}
MetricDatum' {_mdValue = Just 1.193070592e9, _mdDimensions = Just [Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemFree"}
MetricDatum' {_mdValue = Just 2.754772992e9, _mdDimensions = Just [Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemAvailable"}
MetricDatum' {_mdValue = Just 1.43497216e9, _mdDimensions = Just [Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Bytes, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsed"}
MetricDatum' {_mdValue = Just 34.61893821487685, _mdDimensions = Just [Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Percent, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "MemUsedPercent"}
MetricDatum' {_mdValue = Just (-1.144), _mdDimensions = Just [Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpOffset"}
MetricDatum' {_mdValue = Just 0.39, _mdDimensions = Just [Dimension' {_dName = "foo", _dValue = "bar"},Dimension' {_dName = "blah", _dValue = "baz"}], _mdUnit = Just Milliseconds, _mdTimestamp = Nothing, _mdStatisticValues = Nothing, _mdMetricName = "NtpJitter"}
```
