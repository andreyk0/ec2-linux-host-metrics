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


```bash
Publishes a few metrics about linux hosts to CloudWatch.

Usage: ec2-linux-host-metrics [-v|--verbose] [-V|--version]
                              [-p|--publish-metrics]
                              [-d|--metric-dimension name=value]

Available options:
  -h,--help                Show this help text
  -v,--verbose             Be verbose.
  -V,--version             Print version and exit.
  -p,--publish-metrics     Publish generated metrics to CloudWatch.
  -d,--metric-dimension name=value
                           Extra dimension to attach to all generated data
```
