[![Build Status](https://travis-ci.org/palkan/influx_udp.svg?branch=master)](https://travis-ci.org/palkan/influx_udp)

Erlang InfluxDB UDP Writer
==========================

Write data to Influxdb (~>0.9) using JSON UDP interface [(influxdb docs)](http://influxdb.com/docs/v0.9/concepts/reading_and_writing_data.html).

**For InfluxDB ~> 0.8 see [this branch]().**

**Erlang version:** >=17.1

## Setup

rebar.config
```erlang
...
{deps, [
  ...
  {influx_udp, ".*", {git, "https://github.com/palkan/influx_udp.git", "master"}}
]}.
...
```

app.config
```erlang
[
  {influx_udp,
    [
        {pool_size, 5}, 
        {max_overflow, 10} %% poolboy settings (defaults are 1 and 0)
    ]
  }
].
```


## Usage

First, start application: 

```erlang
influx_udp:start().
``` 

Second, start new connection pool:

```erlang

# Start named pool with custom host and port
{ok, Pid} = influx_udp:start_pool(my_pool, #{ host => "localhost", port => 4445, database => "my_db" }).

# Or start anonymous pool (with default host and port ("localhost:4444")) 
{ok, Pid} = influx_udp:start_pool(#{ database => "my_db" }).

And then write data:

```erlang

# Writing to named pool with tags
influx_udp:write(my_pool, Series::string()|atom()|binary(), Points::list(map())|list(proplist())|map()|proplist(), Tags::list(string())|list(atom())). 

# Writing to anonymous pool without tags
influx_udp:write(Pid, Series, Points).

# Writing raw valid InfluxDB input data
influx_udp:write(Data::binary()).
```
