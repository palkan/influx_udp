## 0.8.0
Features:
- Add pools functionality

Breaking changes:
- `influx_udp:write/1,2` requires pool name or pid
```erlang
%% Before:
influx_udp:write(Series, Point).
influx_udp:write(Bin).

%% After:
influx_udp:write(Pool, Series, Point).
influx_udp:write(Pool, Bin).
```

Major and minor versions now follows InfluxDB version.

## 0.1.0
- Basic functionality