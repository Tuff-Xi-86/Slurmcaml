## Installation and Build

To install and build the system, run:

dune build

You may need the `csv` and `lwt` packages. Install them with:

```bash
opam update
opam upgrade
opam install csv lwt
```
## Running the Program

Open separate terminals and follow the steps below.

1. **Server** (first terminal):
`dune exec bin/server.exe 127.0.0.1 5000`

2. **Client** (second terminal):
`dune exec bin/client.exe 127.0.0.1 5000`

3. **Worker 1** (third terminal):
`dune exec bin/worker.exe 127.0.0.1 5000 worker1`

4. **Worker 2** (optional, fourth terminal):
`dune exec bin/worker.exe 127.0.0.1 5000 worker2`

## Sending a Job

From the client terminal (step 2), run any command in any of the following formats below:
mat1 and mat2 are matrix files paths, order does not matter

if mat1 and mat2 are integers, here are a list of runable commands:

```bash
int add mat1.csv mat2.csv
int subtract mat1.csv mat2.csv
int scale mat1.csv 10
int multiply mat1.csv mat2.csv
```

if mat1 and mat2 are floats, here are a list of runable commands:

```bash
float add mat1.csv mat2.csv
float subtract mat1.csv mat2.csv
float scale mat1.csv 10
float multiply mat1.csv mat2.csv
```