# Slurmcaml

**Slurmcaml** is a distributed parallel computing system written in OCaml. It allows users to run parallel matrix operations distributed across multiple computers, scaling compute power by utilizing a cluster of devices.

## Vision & Overview

Our project is a program to allow you to run parallel matrix operations distributed across multiple computers. These operations include **addition**, **subtraction**, **multiplication**, **scaling**, and **transposition**.

The system operates on a **Client-Server-Worker** architecture:
1.  **Client:** Users provide the matrices (via CSV) and the desired operation to the server. Note that only the client needs to possess the source data.
2.  **Server:** Divides the work into atomic sub-tasks and distributes them amongst available worker nodes.
3.  **Workers:** Process their assigned workloads (even on separate devices) and report partial results back to the server.

This program allows you to scale your computer power on matrices across multiple devices. It is particularly helpful if you do not possess a powerful single device but can access multiple less powerful devices to act as a cluster with greater aggregate compute power.

## Key Features

* **Distributed Architecture:** Seamlessly splits jobs between a central server and multiple worker nodes.
* **Matrix Operations:**
    * Addition
    * Subtraction
    * Multiplication
    * Scalar Multiplication (Scaling)
  
* **Data Support:** Full support for both **Integer** and **Floating-point** matrices parsed from CSV files.
* **Resilience:** Robust handling of worker/client disconnects, malformed CSVs, and dimension mismatches.

## Installation & Build

### Prerequisites
* [OCaml](https://ocaml.org/) (v4.14+ recommended)
* [Opam](https://opam.ocaml.org/)
* [Dune](https://dune.build/)

### Steps

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/Tuff-Xi-86/Slurmcaml.git](https://github.com/Tuff-Xi-86/Slurmcaml.git)
    cd Slurmcaml
    ```

2.  **Install dependencies:**
    ```bash
    opam install . --deps-only --with-test
    ```

3.  **Build the project:**
    ```bash
    dune build
    ```

## Usage

To run the project, use `dune exec`. You will likely need to specify whether you are running a **Server**, a **Worker**, or a **Client** command.

```bash
dune exec bin/main.exe -- [ARGUMENTS]
