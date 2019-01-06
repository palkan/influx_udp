[![Build Status](https://travis-ci.org/palkan/influx_udp.svg?branch=master)](https://travis-ci.org/palkan/influx_udp)

# Erlang InfluxDB UDP Writer

Write data to [InfluxDB](http://influxdb.com) (>= 0.9) via UDP (see [(InfluxDB docs)](https://docs.influxdata.com/influxdb/v1.7/supported_protocols/udp/)).

**Erlang/OTP version:** >=17.1

## Setup

rebar.config
```erlang
%% using Hex
{deps, [
  influx_udp
]}.

%% from source
{deps, [
  {influx_udp, ".*", {git, "https://github.com/palkan/influx_udp.git", "master"}}
]}.
```

app.config
```erlang
[
  {influx_udp,
    [
      {influx_host, '127.0.0.1'},
      {influx_port, 8089},
      {pool_size, 5}, %% defaults to 3
      {max_overflow, 10} %% defaults to 1
    ]
  }
].
```

## Usage

First, you need to start the application: 

```erlang
influx_udp:start().
``` 

Now you can create _pools_ and write data to InfluxDB.

## Pools

The default pool is started on application start if you specified `influx_host` and `influx_port` in the configuration file (see above).

You can run pools manually:

```erlang
influx_udp:start_pool(my_pool, #{ host => 'yet.another.influx.host' }).
```

Options not specified in `influx_udp:start_pool/2` would be taken from the default configuration.

## Writing data

```erlang

%% Writing to the named pool with tags
influx_udp:write_to(
  my_pool,
  Series::string()|atom()|binary(), Points::list(map())|list(proplists:proplist())|map()|proplists:proplist(),
  Tags::proplists:proplist()|map()). 

influx_udp:write_to(my_pool, "cpu", [{value, 88}], [{host, 'eu-west'}]).

%% Writing to default pool
influx_udp:write("cpu", [#{value => 88}, #{value => 22}, #{value => 33}], [{host, 'eu-west'}]).

%% Writing data with time
influx_udp:write("cpu", #{value => 88}, #{host => 'eu-west'}, 1434055562000000000).

%% or with current time
influx_udp:write("cpu", #{value => 88}, #{host => 'eu-west'}, true).

%% Writing to default pool without tags
influx_udp:write(Series, Points).

%% Writing raw valid InfluxDB input data
influx_udp:write(Data::binary()).

%% or
influx_udp:write_to(my_pool, Data::binary()).

%% Write Influx-valid map or proplist
influx_udp:write(#{ measurement => test, fields => #{ val => 1} }).

%% or many points
influx_udp:write(
  #{ measurement => test, fields => #{ val => 1} },
  #{ measurement => test2, fields => #{ val => 2}, tags => { host => test}}
)
```

## Encoder

Module `influx_line` provides methods to encode erlang terms to Line protocol.
Encoder automatically sets timestamps (unique) when encoding list of points (see below).

```erlang

%% convert map or proplist to line
influx_line:encode(#{ measurement => test, fields => #{ val => 1} }).

#=> <<"test val=1">>

%% convert list of points to lines
influx_line:encode([
  #{ measurement => test, fields => #{ val => 1} },
  #{ measurement => test2, fields => #{ val => 2}, tags => { host => test}}
]).

#=> <<"test val=1 1434305562895000000\ntest2,host=test val=2 1434305562895000001">>

%% convert any map/proplist to line
influx_line:encode(test, #{ val => 1}).

#=> <<"test val=1">>

%% convert many points with the same measurement and tags to line
influx_line:encode(test, [#{ val => 1}, #{ val => 2}], #{ host => test}, 100).

#=> <<"test,host=test val=1 100\ntest,host=test val=2 101\n">>
```

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/palkan/influx_udp.

## License

The library is available as open source under the terms of the [MIT License](https://opensource.org/licenses/MIT).
