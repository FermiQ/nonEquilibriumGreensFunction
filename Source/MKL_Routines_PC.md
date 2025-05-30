# Documentation for Source/MKL_Routines_PC.F90

## Overview

This file is very similar to `MKL_Routines_Cluster.F90`. It provides a set of linear algebra utility subroutines, likely intended for a personal computer (PC) environment, leveraging Intel Math Kernel Library (MKL) routines through the Fortran 95 interface (`USE mkl95_blas`). It includes functions for:
- General matrix inversion (`Invert_Matrix`).
- Eigenvalue calculation for Hermitian matrices (`Get_Eigvals`).
- Generalized eigenvalue calculation (`Get_CEigvals`).
- Inversion of tridiagonal matrices (`Invert_Tridiag`).

A key difference from the "Cluster" version is the explicit `USE mkl95_blas` statement, which provides a Fortran 95 interface to MKL BLAS and LAPACK routines. This version also includes dummy MPI subroutines (`MPI_Initialize`, `MPI_Broadcast`, `MPI_Gather_Iteration`, `MPI_Gather_Final`), suggesting this file is used when compiling for a non-MPI (serial) execution environment, providing compatibility for code that might call these MPI routines.

## Key Components

The core linear algebra subroutines are identical in functionality and implementation details to those in `MKL_Routines_Cluster.F90`:

- **`SUBROUTINE Invert_Matrix(invG, G, N, errorFlag)`**: Inverts matrix `G` into `invG` using `ZGETRF` and `ZGETRI`.
- **`SUBROUTINE Get_Eigvals(Nx, Nz, Htotd, HopZ, eigvals)`**: Calculates eigenvalues of a block Hermitian matrix using `ZHEEVD`.
- **`SUBROUTINE Get_CEigvals(N, A, B, eigval)`**: Solves `A*psi = eigval*B*psi` by converting to `inv(B)*A*psi = eigval*psi` and using `zgemm`, `ZGEHRD`, `ZHSEQR`.
- **`SUBROUTINE Invert_Tridiag(invG, G, N, errorFlag)`**: Inverts a tridiagonal matrix `G` using a recursive algorithm.

Additionally, this file includes:

- **`SUBROUTINE MPI_Initialize`**:
    - Dummy MPI initialization.
    - Sets `mpiSize = 1`, `mpiRank = 0`, `mpiRoot = 0` (from `NEGF_Module`).
- **`SUBROUTINE MPI_Broadcast`**: Dummy subroutine.
- **`SUBROUTINE MPI_Gather_Iteration`**: Dummy subroutine.
- **`SUBROUTINE MPI_Gather_Final`**: Dummy subroutine.

## Important Variables/Constants

- The variables and constants for the linear algebra routines are the same as in `MKL_Routines_Cluster.F90`.
- **Module Usage**:
    - `USE mkl95_blas`: For MKL LAPACK/BLAS routines.
    - `USE Constants`: For `c0`, `c1`.
    - `USE NEGF_Module`: In `MPI_Initialize` for MPI-related variables.

## Usage Examples

The linear algebra routines are used similarly to the "Cluster" version. The dummy MPI routines allow code that expects an MPI environment (like `NEGF_Main.F90`) to compile and run in a serial mode without changes to its MPI calls.

```fortran
! Example of using Invert_Matrix
USE mkl95_blas
USE Constants
IMPLICIT NONE
COMPLEX(8), DIMENSION(10,10) :: matrix_A, inv_A
INTEGER :: N_dim = 10, err_flag

! ... initialize matrix_A ...
CALL Invert_Matrix(inv_A, matrix_A, N_dim, err_flag)

! MPI calls will execute the dummy versions:
CALL MPI_Initialize
! ...
CALL MPI_Broadcast
```

## Dependencies and Interactions

- **`USE mkl95_blas`**: Provides access to MKL routines.
- **`USE Constants`**: Accesses basic complex constants.
- **`USE NEGF_Module`**: Used by `MPI_Initialize` to set default serial MPI parameters.
- **Intel MKL**: Relies on Intel MKL being available and correctly linked.
- **LAPACK/BLAS Calls**: The routines `ZGETRF`, `ZGETRI`, `ZHEEVD`, `zgemm`, `ZGEHRD`, `ZHSEQR` are sourced from MKL via the `mkl95_blas` module.
- **Called by**:
    - `Invert_Matrix` is called by `Sancho_Rubio`, `Recursive_Inversion` (in `Contact_Self_Energy.F90`), and `Get_CEigvals`.
    - The dummy MPI routines would be called by the main program flow if it's written to use MPI (e.g., `NEGF_Main.F90`).
- **Purpose of Dummy MPI Routines**: To allow a single codebase that uses MPI calls to be compiled and run on a PC without MPI installed or without wanting to run in parallel. The linker will choose these dummy versions over the actual MPI library versions (e.g., from `MPI_Routines.F90`) based on the build configuration.
