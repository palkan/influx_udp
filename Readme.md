Erlang InfluxDB UDP Writer
==========================

Write data to influxdb using JSON UDP interface [(influxdb docs)](http://influxdb.com/docs/v0.8/api/reading_and_writing_data.html#writing-data-through-json-+-udp).

## Setup

app.config
```
[
  {influx_udp,
    [
        {influx_host, 'my@influx.com'}, %% default '127.0.0.1'
        {influx_port, 1234}, %% default 4444
        {pool_size, 5}, 
        {max_overflow, 10} %% poolboy settings
    ]
  }
].
```

## Usage

```
influx_udp:write(Series::string()|atom()|binary(), Points). %% Points can be list of maps (proplists) or map (proplist) 

influx_udp:write(Data::binary()). %% Data contains valid influxdb JSON 
```
