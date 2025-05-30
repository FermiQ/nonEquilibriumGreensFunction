# Documentation for Source/SIP_Module.F90

## Overview

The `SIP_Module` Fortran module defines parameters, constants, and allocatable arrays specifically for the Poisson solver, which uses Stone's Strongly Implicit Procedure (SIP). It includes material parameters (dielectric constants, effective masses, bandgaps for Bi2Se3, SiO2, GaAs), mesh parameters (gate voltages, grid spacings), and arrays central to the SIP algorithm (coefficient arrays, potential, charge density, iteration parameters). This module `USE`s `NEGF_Module`, indicating it builds upon or interacts with the core NEGF data structures, likely by using `eps0`, `m0`, `q`, `hbar`, `kB` from `Constants` (via `NEGF_Module`) and potentially grid dimensions like `Npx`, `Npy`, `Npz` if they are shared.

## Key Components

- **`MODULE SIP_Module`**: The primary component, encapsulating all data for the SIP Poisson solver.

## Important Variables/Constants

### General Parameters:
- **`T0`**: `REAL(8), PARAMETER`. Temperature for Poisson solver (default 300K).

### Material Parameters (Bi2Se3, SiO2, GaAs):
- **`epsBi`**: `REAL(8), PARAMETER`. Bi2Se3 dielectric constant (relative to `eps0` from `NEGF_Module`).
- **`epsGate`**: `REAL(8), PARAMETER`. Gate dielectric constant (SiO2).
- **`epsBox`**: `REAL(8), PARAMETER`. Oxide dielectric constant.
- **GaAs Parameters**:
    - `ml_gaas`, `mt_gaas`, `mlh_gaas`, `mhh_gaas`: Effective masses.
    - `mdh_gaas`, `mde_gaas`, `mc_gaas`: Derived effective masses (density of states, conductivity).
    - `Eg_gaas`: Bandgap energy of GaAs, temperature-dependent.
    - `Nc_gaas`, `Nv_gaas`: Effective density of states for conduction and valence bands in GaAs.
    - `delta_Ec`: Conduction band offset (e.g., Si to BOX).
    - `dEc_gaas`, `Norm_Eg_gaas`: Normalized intrinsic level and bandgap for GaAs.
    - `Ncnorm_gaas`, `Nvnorm_gaas`: Normalized density of states for GaAs.
    - `ni_gaas`: Intrinsic carrier concentration for GaAs.
*(Note: While GaAs parameters are extensively defined, their usage in a Bi2Se3 simulation context might be for specific layers/regions not apparent from `NEGF_Module` alone, or this module might be a more general Poisson solver module.)*

### Mesh Parameters:
- **`VTg`, `VBg`**: `REAL(8), PARAMETER`. Top and bottom gate voltages (default 0V).
- **`b0`**: `REAL(8), PARAMETER`. A lattice constant in z (default 1.0, units context-dependent).
- **`hx`, `hy`, `hz`**: `REAL(8)`. Grid spacings in x, y, z directions.
- **`Phi(:,:,:)`**: `REAL(8), ALLOCATABLE`. 3D electrostatic potential profile.
- **`ro(:,:,:)`**: `REAL(8), ALLOCATABLE`. 3D charge density profile (`rho`).
- **`epsMat(:,:,:)`**: `REAL(8), ALLOCATABLE`. 3D dielectric constant profile.
- **`doping(:,:,:)`**: `REAL(8), ALLOCATABLE`. 3D doping concentration profile.
- **`NominalDoping(:,:,:)`**: `REAL(8), ALLOCATABLE`. Nominal doping profile (e.g., for boundary conditions).
- **`n(:,:,:)`**: `REAL(8), ALLOCATABLE`. 3D electron density profile.
- **`p(:,:,:)`**: `REAL(8), ALLOCATABLE`. 3D hole density profile.
- **`materialType(:,:,:)`**: `INTEGER, ALLOCATABLE`. 3D array flagging material type at each grid point (1: oxide, 2: Bi2Se3, 3-8: Contacts).

### Parameters for Stone's SIP Algorithm:
- **`Bvec`, `Cvec`, `Dvec`, `Evec`, `Fvec`, `Gvec`, `Hvec`**: `REAL(8), DIMENSION(:,:,:), ALLOCATABLE`. Coefficient arrays for the 7-point stencil of the discretized Poisson equation used in SIP.
- **`R_vec`**: `REAL(8), DIMENSION(:,:,:), ALLOCATABLE`. Residual vector in the SIP iteration.
- **`PhiDelta`**: `REAL(8), DIMENSION(:,:,:), ALLOCATABLE`. Correction to the potential `Phi` in an SIP iteration.
- **`V_vec`**: `REAL(8), DIMENSION(:,:,:), ALLOCATABLE`. Intermediate vector in the SIP forward/backward sweeps.
- **`alphaArrayLength`**: `INTEGER, PARAMETER`. Length of the `alphaArray` (default 6).
- **`alphaArray(1:alphaArrayLength)`**: `REAL(8)`. Array of SIP iteration parameters (acceleration parameters). Initialized with `DATA` statement.
- **`alpha`**: `REAL(8)`. Current SIP iteration parameter, chosen cyclically from `alphaArray`.
- **`poissonIsConverged`**: `INTEGER`. Flag indicating convergence of the Poisson solver (0 = not converged, 1 = converged).

## Usage Examples

This module is not executed directly but is `USE`d by subroutines that implement the Poisson solver logic (e.g., `SIP_Coefficients`, `SIP_Routines`, `SIP_Solver`).

```fortran
SUBROUTINE ExamplePoissonStep
    USE SIP_Module
    IMPLICIT NONE

    ! Access parameters from SIP_Module
    IF (materialType(1,1,1) == 2) THEN ! If Bi2Se3
        PRINT *, "Dielectric const at (1,1,1):", epsMat(1,1,1)
        ! (Note: epsMat would be epsBi, but NEGF_Module.eps0 is needed for actual value)
    ENDIF

    ! Modify arrays (done within SIP solver routines)
    ! Phi(0,0,0) = ...
    ! R_vec(1,1,1) = ...
END SUBROUTINE ExamplePoissonStep
```

## Dependencies and Interactions

- **`USE NEGF_Module`**: This is a primary dependency. It implies that `SIP_Module` relies on base constants (like `eps0`, `m0` from `Constants` module via `NEGF_Module`) and potentially device dimensions (`Npx`, `Npy`, `Npz` if these are taken from `NEGF_Module` for consistency) and charge densities (`RhoN`, `RhoP` from `NEGF_Module` are used to compute `ro`).
- **Global Data for SIP**: Serves as the central repository for all data specific to the SIP Poisson solver. Subroutines like `SIP_Coefficients`, `Initialize_Grid`, `Solve_Equilibrium`, and `SIP_V1` will `USE SIP_Module` to access and modify these variables.
- **Initialization**: Variables are initialized and arrays allocated by routines like `Initialize_Grid` and `SIP_Coefficients` (found in `SIP_Routines.F90` and `SIP_Coefficients.F90`).
