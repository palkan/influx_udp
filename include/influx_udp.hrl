-type(key() ::atom()|binary()|string()).

-type(influx_data_point() ::map()|list({key(), any()})).
-type(influx_data_points() ::influx_data_point()|list(influx_data_point())).

-export_type([influx_data_point/0, influx_data_points/0]).