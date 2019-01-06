REBAR=./rebar

run-influxdb:
	docker run --rm \
	-p 8086:8086 -p 8089:8089/udp \
	-v ${PWD}/files/influxdb.conf:/etc/influxdb/influxdb.conf:ro \
	-e INFLUXDB_DB=db \
  influxdb:latest
