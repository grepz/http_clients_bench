#!/usr/bin/env sh

if [ "$#" -lt 1 ]; then
    echo "Incorrect number of arguments"
    exit 1
fi

function run_server {
    erl +SDio 1 +SDcpu 1:1 +A 1 +K true +P 262144 -pa ./ebin -pa _build/default/lib/*/ebin -s http_clients_bench_server start
}

function run_client {
    CLIENT=$1
    erl +SDio 1 +SDcpu 1:1 +A 1 +K true +P 262144 -pa ./ebin -pa _build/default/lib/*/ebin -s http_clients_bench_client start $CLIENT
}


if [ $1 == "client" ]; then
    run_client $2
elif [ $1 == "server" ]; then
    run_server
else
    echo "Unknown command"
fi
