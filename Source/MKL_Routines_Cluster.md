# Documentation for Source/MKL_Routines_Cluster.F90

## Overview

This file provides a set of linear algebra utility subroutines, presumably optimized for or intended for use on a cluster environment, leveraging Intel Math Kernel Library (MKL) routines. It includes functions for:
- General matrix inversion (`Invert_Matrix`) using LU decomposition.
- Eigenvalue calculation for Hermitian matrices (`Get_Eigvals`) using `ZHEEVD`.
- Generalized eigenvalue calculation (`Get_CEigvals`) for `A*psi = eigval*B*psi` by transforming to a standard eigenvalue problem.
- Inversion of tridiagonal matrices (`Invert_Tridiag`) using a recursive Green's function approach (Thomas algorithm variant).

The comment `!INCLUDE 'mkl.h'` and library names suggest a dependency on Intel MKL for the LAPACK routines like `ZGETRF`, `ZGETRI`, `ZHEEVD`, `ZGEHRD`, `ZHSEQR`, and BLAS routines like `zgemm` (though `zgemm` is called directly in `Get_CEigvals` rather than via a `USE mkl95_blas` which is seen in the PC version).

## Key Components

- **`SUBROUTINE Invert_Matrix(invG, G, N, errorFlag)`**:
    - Computes the inverse of a general complex matrix `G` of size `N`x`N`, storing the result in `invG`.
    - Uses LAPACK routines:
        - `ZGETRF`: Computes LU factorization.
        - `ZGETRI`: Computes the inverse from the LU factorization.
    - Sets `errorFlag` based on the `INFO` returned by LAPACK routines.
    - `WORK` and `IPIV` are workspace and pivot arrays for LAPACK.

- **`SUBROUTINE Get_Eigvals(Nx, Nz, Htotd, HopZ, eigvals)`**:
    - Calculates eigenvalues for a block tridiagonal Hermitian matrix, likely representing a 2D system or slices of a 3D system.
    - `Htotd(Nx,Nx,Nz)` contains the diagonal blocks.
    - `HopZ(Nx,Nx)` contains the off-diagonal coupling blocks.
    - Constructs a full matrix `HDMat` of size `(Nx*Nz)`x`(Nx*Nz)` from these blocks.
    - Calls `ZHEEVD` (LAPACK driver routine for eigenvalues and optionally eigenvectors of a complex Hermitian matrix) to find eigenvalues, stored in `eigvals`. `'N'` indicates eigenvalues only.
    - `WORK`, `RWORK`, `IWORK` are workspace arrays for `ZHEEVD`.

- **`SUBROUTINE Get_CEigvals(N, A, B, eigval)`**:
    - Solves the generalized eigenvalue problem `A*psi = eigval*B*psi`.
    - First, it computes `C = inv(B)*A` by calling `Invert_Matrix` and then `zgemm` (BLAS matrix multiplication).
    - Then, it solves the standard eigenvalue problem `C*psi = eigval*psi`.
        - `ZGEHRD`: Reduces `C` to upper Hessenberg form.
        - `ZHSEQR`: Computes eigenvalues of the Hessenberg matrix. (Note: `ZHSEQR` is typically for Hessenberg matrices, but if `C` is general, `ZGEEV` would be more direct for `C*psi = eigval*psi`. Using `ZGEHRD` then `ZHSEQR` is a common path if only eigenvalues are needed or for specific forms).
    - Stores eigenvalues in `eigval`.

- **`SUBROUTINE Invert_Tridiag(invG, G, N, errorFlag)`**:
    - Computes the inverse of a complex tridiagonal matrix `G` of size `N`x`N` using a recursive algorithm (related to Thomas algorithm or Green's function approach for 1D systems).
    - First, checks if the input matrix `G` is indeed tridiagonal.
    - Calculates diagonal elements of the inverse using forward and backward recursions for `gLR` (likely local Green's functions).
    - Calculates off-diagonal elements based on the diagonal elements and `gLR`.
    - Checks for `NaN` values in the resulting `invG`.

## Important Variables/Constants

- **Input/Output for `Invert_Matrix`**:
    - `G(N,N)`: Input matrix.
    - `invG(N,N)`: Output inverse matrix.
    - `N`: Matrix dimension.
    - `errorFlag`: Output error status.
- **Input/Output for `Get_Eigvals`**:
    - `Htotd(Nx,Nx,Nz)`, `HopZ(Nx,Nx)`: Input Hamiltonian blocks.
    - `Nx`, `Nz`: Dimensions of blocks.
    - `eigvals(Nx*Nz)`: Output eigenvalues.
    - `errorFlag`: Output error status.
- **Input/Output for `Get_CEigvals`**:
    - `A(N,N)`, `B(N,N)`: Input matrices for generalized problem.
    - `N`: Matrix dimension.
    - `eigval(N)`: Output eigenvalues.
    - `errorFlag`: Output error status.
- **Input/Output for `Invert_Tridiag`**:
    - `G(N,N)`: Input tridiagonal matrix.
    - `invG(N,N)`: Output inverse matrix.
    - `N`: Matrix dimension.
    - `errorFlag`: Output error status.
- **Module Usage**: `USE Constants` for `c0`, `c1`.
- **LAPACK/BLAS routine names**: `ZGETRF`, `ZGETRI`, `ZHEEVD`, `ZGEHRD`, `ZHSEQR`, `zgemm`.

## Usage Examples

These subroutines are called by other parts of the simulation code when matrix operations are needed.

```fortran
! Example of using Invert_Matrix
USE Constants
IMPLICIT NONE
COMPLEX(8), DIMENSION(10,10) :: matrix_A, inv_A
INTEGER :: N_dim = 10, err_flag

! ... initialize matrix_A ...
CALL Invert_Matrix(inv_A, matrix_A, N_dim, err_flag)
IF (err_flag == 0) THEN
    ! Use inv_A
ENDIF

! Example of using Get_Eigvals (conceptual)
! CALL Get_Eigvals(Nx_val, Nz_val, H_blocks, Hop_blocks, all_eigenvalues)

! Example of using Invert_Tridiag
! CALL Invert_Tridiag(inv_tridiag_G, tridiag_G, N_dim, err_flag)
```

## Dependencies and Interactions

- **`USE Constants`**: Accesses basic complex constants.
- **Intel MKL**: Relies on the Intel Math Kernel Library for LAPACK and BLAS routines. The specific linking command is hinted in comments: `! mkl_solver_ilp64_sequential.lib mkl_intel_ilp64.lib mkl_sequential.lib  mkl_core.lib`.
- **Calling LAPACK/BLAS**: Directly calls `ZGETRF`, `ZGETRI`, `ZHEEVD`, `ZGEHRD`, `ZHSEQR` and `zgemm`.
- **Called by**:
    - `Invert_Matrix` is called by `Sancho_Rubio` and `Recursive_Inversion` in `Contact_Self_Energy.F90`, and by `Get_CEigvals` in this file.
    - Other routines like `Get_Eigvals`, `Get_CEigvals`, `Invert_Tridiag` are likely utilities available for various parts of the physics simulation that might require them (though direct calls are not seen in the currently reviewed high-level files like `Energy_Sweep`).
