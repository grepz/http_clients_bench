http_clients_bench
=====

Benchmark for hackney and ibrowse HTTP clients.

Tweak benching parameters if needed.

```
-module(http_client_tester).

...

-define(CONC, 10).
-define(END_CYCLE, 5000).
```

Server is starting on port 8080, change if needed.

```
-module(http_clients_bench_server).

...

-define(PORT, 8080).
```


Build
-----

    $ make

Run
---

    # To run server
    $ ./run.sh server

    # To run benchmarking
    $ ./run.sh client [hackney|ibrowse]
