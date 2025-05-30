# Documentation for Source/SIP_Routines.F90

## Overview

This file contains higher-level routines for managing the Strongly Implicit Procedure (SIP) based Poisson solver. It includes the main entry point for the SIP solver (`SIP_Main`) and initialization routines for the grid (`Initialize_Grid`) and doping profiles (`Initialize_Doping`).

## Key Components

- **`SUBROUTINE SIP_Main`**:
    - This is the main driver for a Poisson solving step.
    - Resets `poissonIsConverged = 0`.
    - **If `ite == 1` (first iteration of the overall NEGF self-consistency loop)**:
        - Calls `Initialize_Grid`: Sets up mesh parameters, material types, dielectric constants, and allocates potential/charge arrays.
        - Calls `Initialize_Doping`: Sets up doping profiles.
        - Calls `SIP_Coefficients`: Calculates the coefficient matrices for the SIP algorithm based on the grid and material properties.
    - **Else (subsequent iterations)**:
        - Calls `Solve_Equilibrium` (from `SIP_Solver.F90`): This routine performs the actual iterative solution of the Poisson equation using the pre-calculated coefficients and current charge densities.

- **`SUBROUTINE Initialize_Grid`**:
    - Allocates core arrays used by the Poisson solver, defined in `SIP_Module`:
        - `Phi` (electrostatic potential) with ghost cells (-1 and Npx, etc.).
        - `PhiDelta` (correction to potential).
        - `ro` (charge density).
        - `materialType` (integer flag for material at each point).
        - `epsMat` (dielectric constant at each point).
        - `n` (electron density), `p` (hole density).
    - Initializes these arrays (e.g., `Phi = 0D0`).
    - Sets `materialType` and `epsMat` for different regions:
        - Bulk region (1:Npx-2, etc.) is type 2 (Bi2Se3), `epsMat = epsBi/epsBi` (normalized to Bi2Se3 permittivity, assuming `epsBi` in `SIP_Module` is the actual value and it's divided by itself here for a relative permittivity of 1 within the Bi2Se3 region if `epsBi` is used as the reference for normalization elsewhere, or if `epsBi` in `SIP_Module` is already `eps0*relative_epsBi`).
        - Left face (x=0) is type 3 (contact), `epsMat = epsGate/epsBi`.
        - Right face (x=Npx-1) is type 4 (contact), `epsMat = epsGate/epsBi`.
    - Sets grid spacings `hx`, `hy`, `hz` to `a0` (lattice constant from `NEGF_Module`). This implies a uniform cubic mesh.

- **`SUBROUTINE Initialize_Doping`**:
    - Allocates `doping` and `NominalDoping` arrays.
    - Initializes `doping` to `0D0` (undoped system).
    - Sets `NominalDoping = doping`.

## Important Variables/Constants

All variables are accessed from or modify `SIP_Module` or `NEGF_Module` (since `SIP_Module` uses `NEGF_Module`).
- **From `SIP_Module` / `NEGF_Module`:**
    - `poissonIsConverged`: Flag, reset here.
    - `ite`: Current NEGF iteration number (from `NEGF_Module`).
    - `Npx`, `Npy`, `Npz`: Grid dimensions.
    - `a0`: Lattice constant used for grid spacing.
    - `epsBi`, `epsGate`: Dielectric constants.
    - Arrays being allocated/initialized: `Phi`, `PhiDelta`, `ro`, `materialType`, `epsMat`, `n`, `p`, `doping`, `NominalDoping`.
    - `hx`, `hy`, `hz`: Grid spacings.

## Usage Examples

`SIP_Main` is called from the main NEGF loop (`NEGF_Main.F90`) when `includePoisson == 1`.

```fortran
! In NEGF_Main.F90
IF (mpiRank == mpiRoot) THEN
    IF (includePoisson==1) THEN
        CALL SIP_Main   ! This will trigger initialization or solving
    ENDIF
    CALL Build_Hamiltonian
ENDIF
```
`Initialize_Grid`, `Initialize_Doping`, and `SIP_Coefficients` are called internally by `SIP_Main` during the first iteration.

## Dependencies and Interactions

- **`USE SIP_Module`**: All subroutines use this module to access parameters and store results related to the Poisson solver.
- **Calls other SIP routines**:
    - `SIP_Main` calls `Initialize_Grid`, `Initialize_Doping`, `SIP_Coefficients` (from `SIP_Coefficients.F90`), and `Solve_Equilibrium` (from `SIP_Solver.F90`).
- **Interaction with NEGF cycle**: The `ite` variable from `NEGF_Module` controls whether full initialization or just solving is performed.
- **Grid and Material Definition**: `Initialize_Grid` is crucial for defining the geometry, material regions, and dielectric properties of the simulated device for the Poisson solver.
- **Doping Profile**: `Initialize_Doping` sets the fixed charge distribution due to dopants.
