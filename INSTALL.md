## Installation and Build

To install and build the system, run:

dune build

You may need the `csv` and `lwt` packages. Install them with:

opam update
opam upgrade
opam install csv lwt

## Running the Program

Open separate terminals and follow the steps below.

1. **Server** (first terminal):
dune exec bin/server.exe 127.0.0.1 5000

2. **Client** (second terminal):
dune exec bin/client.exe 127.0.0.1 5000

3. **Worker 1** (third terminal):
dune exec bin/worker.exe 127.0.0.1 5000 worker1

4. **Worker 2** (optional, fourth terminal):
dune exec bin/worker.exe 127.0.0.1 5000 worker2

## Sending a Job

From the client terminal (step 2), run:

int add mat1.csv mat2.csv

The outputs will appear on the worker terminals (steps 3 and 4).