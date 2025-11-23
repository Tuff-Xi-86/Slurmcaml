To install and build the system, run dune build. You may need to install the `csv` and `lwt` packages. Do this with
```
$ opam update
$ opam upgrade
$ opam install csv lwt
```

To run the program follow the steps listed below
(i) on the first terminal run: "dune exec bin/server.exe server 127.0.0.1 5000"
(ii) on the seperate second terminal run: "dune exec bin/client.exe head 127.0.0.1 5000"
(iii) on the seperate third terminal run: "dune exec bin/worker.exe worker 127.0.0.1 5000 worker1"
(optional) (iv) (iii) on the seperate fourth terminal run: "dune exec bin/worker.exe worker 127.0.0.1 5000 worker2"

(v) on client(second terminal from step (ii)) send the following instruction: int add mat1.csv mat2.csv
(vi) outputs are currently posted on the worker nodes (terminals from step (iii and iv))