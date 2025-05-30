# Documentation for Source/NEGF_Main.F90

## Overview

This file contains the main program (`NEGF_Main`) for the Non-Equilibrium Green's Function (NEGF) simulation. It orchestrates the overall workflow of the calculation, including initialization, iterative self-consistent loops, Hamiltonian construction, energy sweeps, and data output. It appears to be the entry point of the simulation code.

## Key Components

- **`PROGRAM NEGF_Main`**: The main executable program.
    - It initializes the simulation by calling `Read_In_Allocate`.
    - It then enters a loop (controlled by `ite` up to `iteMax`) for self-consistent calculations.
    - Inside the loop:
        - It potentially calls `SIP_Main` (for Poisson calculations) and `Build_Hamiltonian` on the root MPI process.
        - It broadcasts data using `MPI_Broadcast`.
        - It performs an energy sweep by calling `Energy_Sweep` across MPI processes.
        - It gathers data using `MPI_Gather_Iteration`.
        - Finally, if convergence is reached or max iterations are met, it calls `MPI_Gather_Final` and `Dump_Data` on the root MPI process.

## Important Variables/Constants

- **`iteMax`**: An integer that likely controls the maximum number of self-consistent iterations. (Implicitly used from a module, likely `NEGF_Module` or `SIP_Module`).
- **`mpiRank`**, **`mpiRoot`**, **`mpiSize`**: MPI-related variables controlling parallel execution. (Implicitly used).
- **`includePoisson`**: An integer flag, likely to enable/disable the Poisson solver (`SIP_Main`). (Implicitly used).
- **`eStpnum`**: Likely the number of energy steps. (Implicitly used).
- **`EAxis`**: An array holding energy values for the sweep. (Implicitly used).
- **`poissonIsConverged`**: A flag indicating the convergence status of the Poisson solver. (Implicitly used).
- **`Htotd`**, **`HopX`**, **`RhoN`**, **`RhoP`**: Variables likely related to Hamiltonian and density matrices. (Implicitly used and reset/modified in the loop).

*Note: Many variables are used without explicit declaration in this file, implying they are imported from `USE`d modules (e.g., `SIP_Module`, and potentially `NEGF_Module` if `SIP_Module` itself uses it).*

## Usage Examples

This is the main program and is executed to run the simulation. The command line or script used to run this program would constitute its usage. For example:

```bash
# Assuming the compiled executable is named 'negf_solver'
./negf_solver [command-line-arguments]
```
*(Specific command-line arguments would depend on how `Read_In_Allocate` is implemented).*

## Dependencies and Interactions

- **`USE SIP_Module`**: This program explicitly uses `SIP_Module`. It likely also implicitly depends on modules used by `SIP_Module` (e.g. `NEGF_Module` which contains many of the simulation parameters).
- **Calls Subroutines**:
    - `Read_In_Allocate`: For setting up initial parameters and allocating memory.
    - `SIP_Main`: (Conditional) For solving the Poisson equation.
    - `Build_Hamiltonian`: For constructing the system Hamiltonian.
    - `MPI_Broadcast`: For distributing data among MPI processes.
    - `Energy_Sweep`: For performing calculations at different energy points.
    - `MPI_Gather_Iteration`: For collecting results from MPI processes after each energy sweep iteration.
    - `MPI_Gather_Final`: For collecting final results.
    - `Dump_Data`: For writing simulation results to files.
- **Parallelism**: The code is structured for MPI-based parallel execution, with specific tasks performed only on the `mpiRoot` process and energy sweeps distributed among processes.
