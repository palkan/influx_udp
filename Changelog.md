# Change log

## 1.0.0 (2019-01-06)

[Hex package]() is available 🎉.

- Default pool behaviour changed.

Previously, the default configuration had both `influx_host` and `influx_port` defined, which made
the application start the _default_ pool connecting to `127.0.0.1:4444` (which didn't make a lot of sense, did it?).

We also had a special `{default_pool, false}` configuration variable to disable the default pool.

We removed the default value for `influx_host` (and changed the default `influx_port` to **8089**).

Now if `influx_host` is not defined, we do not start the default pool.

## 0.9.1 (2015-06-14)

- Add Line encoder

## 0.9.0 (2015-06-14)

- Add support for InfluxDB 0.9 (tags, line protocol)

## 0.8.0 (2015-06-13)

- Add pools functionality

Major and minor versions now follows InfluxDB version.

## 0.1.0 (2014-12-17)

- Basic functionality.
