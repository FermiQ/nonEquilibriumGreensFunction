# Documentation for Source/Calculate_Gnp.F90

## Overview

This subroutine, `Calculate_Gnp`, is responsible for computing the electron (`Gn`) and hole (`Gp`) correlation functions (lesser and greater Green's functions) within the NEGF formalism. These functions are crucial for determining carrier densities and currents. The calculation involves matrix multiplications of retarded Green's functions (`GR`), broadening functions (`GammaL`, `GammaR`), and Fermi-Dirac distribution functions (`f1`, `f2`, `f1v`, `f2v`). Finally, it calculates the on-site density matrices `RhoN` and `RhoP` by integrating `Gnd` and `Gpd` respectively, and optionally stores energy-resolved and orbital-resolved densities.

## Key Components

- **`SUBROUTINE Calculate_Gnp`**:
    - Allocates a temporary complex matrix `cmat1`.
    - **Calculates `Gn` (electron correlation function)**:
        - Initializes `Gnd`, `Gnu`, `Gnl` (diagonal, upper, and lower block components of `Gn`) to zero.
        - Computes `GRLef * GammaL * f1` and stores it in `cmat1`.
        - Computes `Gn = cmat1 * GRLef'` (adjoint) and accumulates results into `Gnd`, `Gnu`, `Gnl`.
        - Computes `GRRgt * GammaR * f2` and stores it in `cmat1`.
        - Computes `Gn += cmat1 * GRRgt'` and accumulates results into `Gnd`, `Gnu`, `Gnl`.
    - **Calculates `Gp` (hole correlation function)**:
        - Initializes `Gpd`, `Gpu`, `Gpl` (diagonal, upper, and lower block components of `Gp`) to zero.
        - Computes `GRLef * GammaL * f1v` and stores it in `cmat1`.
        - Computes `Gp = cmat1 * GRLef'` and accumulates results into `Gpd`, `Gpu`, `Gpl`.
        - Computes `GRRgt * GammaR * f2v` and stores it in `cmat1`.
        - Computes `Gp += cmat1 * GRRgt'` and accumulates results into `Gpd`, `Gpu`, `Gpl`.
    - Deallocates `cmat1`.
    - **Calculates on-site density matrices**:
        - Iterates over spatial grid points (`xx`, `yy`, `zz`) and orbitals (`ii`).
        - Accumulates `RhoN` from the real part of diagonal elements of `Gnd`.
        - Accumulates `RhoP` from the real part of diagonal elements of `Gpd`.
        - (Conditionally) Accumulates energy-resolved density `ERRhon` if `erCD == 1`.
        - (Conditionally) Accumulates orbital-resolved density `ORRhon` if `orCD == 1`.

## Important Variables/Constants

This subroutine uses variables primarily from `NEGF_Module`:
- **Input/Operand Matrices (from `NEGF_Module`):**
    - `GRLef`, `GRRgt`: Retarded Green's functions for left and right leads/device.
    - `GammaL`, `GammaR`: Broadening matrices for left and right contacts.
    - `f1`, `f2`, `f1v`, `f2v`: Fermi-Dirac distribution related factors for electron and hole contributions from contacts.
- **Output/Result Matrices (modified in `NEGF_Module`):**
    - `Gnd`, `Gnu`, `Gnl`: Components of the electron correlation function `Gn`.
    - `Gpd`, `Gpu`, `Gpl`: Components of the hole correlation function `Gp`.
    - `RhoN`, `RhoP`: On-site electron and hole density matrices.
    - `ERRhon`: (Conditional) Energy-resolved charge density.
    - `ORRhon`: (Conditional) Orbital-resolved charge density.
- **Parameters (from `NEGF_Module`):**
    - `Ntot`, `NNz`, `Npx`, `Npy`, `Npz`, `NN`, `Norb`: Grid dimensions and orbital counts.
    - `deltaE`: Energy step for integration.
    - `eCtr`: Current energy step counter.
    - `erCD`, `orCD`: Flags for calculating energy-resolved and orbital-resolved charge densities.
- **Local Variables:**
    - `cmat1`: Allocatable complex matrix for temporary calculations.
    - `index`: Integer for matrix indexing.
- **Constants (likely from `Constants` module via `NEGF_Module`):**
    - `c0`: Complex zero.
    - `pi`: Mathematical constant pi.

## Usage Examples

This subroutine is called within an energy loop, typically after the retarded Green's functions and self-energies for contacts have been computed for a specific energy point.

```fortran
! In an energy sweep loop (e.g., within Energy_Sweep.F90)
USE NEGF_Module
IMPLICIT NONE

! ... (previous calculations for GR, GammaL, GammaR, f1, f2, etc. for current 'energy') ...

CALL Calculate_Gnp

! ... (RhoN, RhoP, Gnd, etc. in NEGF_Module are now updated for the current energy) ...
```

## Dependencies and Interactions

- **`USE NEGF_Module`**: Explicitly uses `NEGF_Module` to access and modify a large number of shared arrays and parameters.
- **Input Data**: Relies on `GRLef`, `GRRgt`, `GammaL`, `GammaR`, `f1`, `f2`, `f1v`, `f2v` being up-to-date for the current energy point. These are typically calculated in preceding steps within an energy sweep routine.
- **Output Data**: Modifies `Gnd`, `Gnu`, `Gnl`, `Gpd`, `Gpu`, `Gpl`, `RhoN`, `RhoP`, `ERRhon`, and `ORRhon` in `NEGF_Module`. These results are then used by other subroutines, for example, to calculate observables or for self-consistent updates of the potential.
- **Called by**: Likely called from `Energy_Sweep.F90` or a similar routine that iterates over energy points.
