# Documentation for Source/Recursive_Greens.F90

## Overview

The `Get_Retarded_Greens` subroutine calculates the retarded Green's function (`GR`) for the device. It takes the inverse Green's function (`InvGRd`), which includes the Hamiltonian and contact self-energies, and computes its inverse. This is a core part of the NEGF calculation. The method used is a recursive algorithm, often referred to as the Recursive Green's Function (RGF) method, suitable for systems composed of coupled slices (layers). It computes block components of `GR`: `GRd` (diagonal blocks), `GRu` (upper off-diagonal blocks), `GRl` (lower off-diagonal blocks), and also `GRLef` and `GRRgt` which are related to the Green's function "seen" from the left and right contacts, respectively.

## Key Components

- **`SUBROUTINE Get_Retarded_Greens`**:
    - Allocates temporary matrices `cmat1`, `cmat2`, `gLR`, `gRR`.
    - **Forward Recursion (calculating `gLR`)**:
        - Iterates from `ii = 1` to `Npx` (number of slices in x-direction).
        - For each slice `ii`, calculates a local Green's function `gLR(:,:,ii)`.
            - `gLR_inv = InvGRd(:,:,ii) - (-HopX)' * gLR(:,:,ii-1) * (-HopX)` for `ii > 1`.
            - `gLR_inv = InvGRd(:,:,1)` for `ii = 1`.
        - Inverts `gLR_inv` using `CALL Invert_Matrix` and stores it in `gLR(:,:,ii)`.
        - Includes error checking for singular matrices.
    - **Backward Recursion (calculating `GRd`, `GRu`, `GRl`)**:
        - Initializes `GRd(:,:,Npx) = gLR(:,:,Npx)`.
        - Iterates backwards from `ii = Npx-1` down to `1`.
        - `GRl(:,:,ii) = -GRd(:,:,ii+1) * (-HopX)' * gLR(:,:,ii)`
        - `GRu(:,:,ii) = -gLR(:,:,ii) * (-HopX) * GRd(:,:,ii+1)`
        - `GRd(:,:,ii) = gLR(:,:,ii) * (I - (-HopX) * GRl(:,:,ii))`
        - (Note: `-HopX` is used for coupling to the right, `(-HopX)'` for coupling to the left, assuming `HopX` is defined as the rightward hopping matrix).
    - **Construct `GRRgt`**:
        - Initializes `GRRgt` with `GRd(:,:,Npx)` and `GRu(:,:,Npx-1)`.
        - Expands to other off-diagonal blocks of `GRRgt` using `gLR` and `HopX` in a loop.
    - Deallocates `gLR`.
    - **Forward Recursion for Left-Looking Green's Functions (calculating `gRR`)**:
        - Similar to the first forward recursion but iterates from `ii = Npx` down to `1`.
        - Calculates `gRR(:,:,ii)` based on `InvGRd(:,:,ii)` and `gRR(:,:,ii+1)`.
        - `gRR_inv = InvGRd(:,:,ii) - (-HopX) * gRR(:,:,ii+1) * (-HopX)'` for `ii < Npx`.
        - `gRR_inv = InvGRd(:,:,Npx)` for `ii = Npx`.
        - Inverts `gRR_inv` into `gRR(:,:,ii)`.
    - **Construct `GRLef`**:
        - Initializes `GRLef` with `GRd(:,:,1)` and `GRl(:,:,1)`.
        - Expands to other blocks of `GRLef` using `gRR` and `HopX` in a loop.
    - Deallocates `gRR`, `cmat1`, `cmat2`.
    - Final error check.

## Important Variables/Constants

This subroutine uses and modifies variables from `NEGF_Module`.
- **Input/Operand Matrices (from `NEGF_Module`):**
    - `InvGRd(NNz,NNz,Npx)`: The inverse retarded Green's function of the device (input).
    - `HopX(NNz,NNz)`: Hopping matrix between slices in the x-direction.
- **Output/Result Matrices (modified in `NEGF_Module`):**
    - `GRd(NNz,NNz,Npx)`: Diagonal blocks of the retarded Green's function.
    - `GRu(NNz,NNz,Npx-1)`: Upper off-diagonal blocks of `GR`.
    - `GRl(NNz,NNz,Npx-1)`: Lower off-diagonal blocks of `GR`.
    - `GRLef(Ntot, NNz)`: Left-looking Green's function components.
    - `GRRgt(Ntot, NNz)`: Right-looking Green's function components.
- **Parameters (from `NEGF_Module`):**
    - `NNz`: Size of a slice matrix (number of orbitals/points in YZ plane * Norb).
    - `Npx`: Number of slices in the x-direction.
    - `Ntot`: Total size parameter, used for `GRLef`, `GRRgt` dimensions.
    - `errorFlag`: Error status from matrix inversion.
    - `energy`: Current energy (used in error messages).
- **Local Variables:**
    - `gLR(NNz,NNz,Npx)`: Allocatable array for left-to-right recursive Green's function components.
    - `gRR(NNz,NNz,Npx)`: Allocatable array for right-to-left recursive Green's function components.
    - `cmat1(NNz,NNz)`, `cmat2(NNz,NNz)`: Allocatable temporary matrices for calculations.
- **Constants (likely from `Constants` module via `NEGF_Module`):**
    - `c0`, `c1`: Complex zero and one.

## Usage Examples

This subroutine is called within the `Energy_Sweep` subroutine, after `InvGRd` has been constructed.

```fortran
! In Energy_Sweep.F90
USE NEGF_Module
IMPLICIT NONE

! ... (InvGRd, HopX are set up from Htotd and SigmaL, SigmaR) ...

CALL Get_Retarded_Greens

! ... (GRd, GRu, GRl, GRLef, GRRgt in NEGF_Module are now populated) ...
! ... (These are then used by Calculate_Gnp) ...
```

## Dependencies and Interactions

- **`USE NEGF_Module`**: Accesses `InvGRd`, `HopX` and updates `GRd`, `GRu`, `GRl`, `GRLef`, `GRRgt`, etc.
- **`CALL Invert_Matrix`**: Relies on an external matrix inversion routine (from `MKL_Routines_*.F90`).
- **`CALL zgemm`**: Uses `zgemm` (complex matrix multiplication from BLAS/MKL) extensively. This call is likely direct to MKL if not using `USE mkl95_blas`.
- **Algorithm**: Implements the standard RGF algorithm for layered systems. The assumption `Al = conjg(transpose(Au))` means that the coupling `HopX` is used for hopping in one direction, and its conjugate transpose for the other.
- **Error Handling**: Checks `errorFlag` after calls to `Invert_Matrix` and prints messages if inversion fails (matrix is singular).
