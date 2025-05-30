# Documentation for Source/SIP_Coefficients.F90

## Overview

The `SIP_Coefficients` subroutine calculates and allocates the seven coefficient arrays (`Bvec` through `Hvec`) required by the Stone's Strongly Implicit Procedure (SIP) algorithm for solving the 3D Poisson equation. These coefficients represent the discretized form of the Poisson equation `div(eps * grad(Phi)) = -rho` on a finite difference grid. The values of these coefficients depend on the local dielectric constant (`epsMat`) and the grid spacings (`hx`, `hy`, `hz`). Special handling is applied for boundary nodes and nodes within contact/gate regions.

## Key Components

- **`SUBROUTINE SIP_Coefficients`**:
    - Allocates the 3D arrays `Bvec`, `Cvec`, `Dvec`, `Evec`, `Fvec`, `Gvec`, `Hvec` with dimensions corresponding to the internal grid points (`Npx-1`, `Npy-1`, `Npz-1`).
    - Iterates through each internal grid point (`ii`, `jj`, `kk` from 1 to `Npx-1`, `Npy-1`, `Npz-1` respectively).
    - **Calculates coefficients related to x-derivatives (`Dvec`, `Fvec`)**:
        - If `ii` is at the boundary (1 or `Npx-1`), sets one of them to 0 and the other based on `epsMat(ii,jj,kk)` and `hx`.
        - Otherwise (internal `ii`), calculates `Dvec` and `Fvec` using averaged dielectric constants (`epsPlusHalf`, `epsMinusHalf`) at interfaces `(i-1/2,j,k)` and `(i+1/2,j,k)`.
    - **Calculates coefficients related to y-derivatives (`Cvec`, `Gvec`)**:
        - Similar logic as for x-derivatives, but for the y-direction, using `epsMat` and `hy`.
    - **Calculates coefficients related to z-derivatives (`Bvec`, `Hvec`)**:
        - Similar logic as for x-derivatives, but for the z-direction, using `epsMat` and `hz`.
    - **Sets coefficients for contact/gate regions**:
        - If `materialType(ii,jj,kk)` indicates a contact/gate (type 3 or 4), all off-diagonal coefficients (`Bvec` to `Dvec`, `Fvec` to `Hvec`) are set to 0, and the diagonal coefficient `Evec` is set to 1.0. This effectively sets the potential at these nodes directly (Dirichlet boundary condition embedded in the matrix).
    - **Calculates the diagonal coefficient `Evec` for non-contact regions**:
        - `Evec(ii,jj,kk) = - (sum of all other off-diagonal coefficients for that point)`. This ensures that the sum of coefficients for a row in the discretized matrix equation is related to the charge density term.

## Important Variables/Constants

This subroutine uses variables primarily from `SIP_Module`.
- **Input/State Variables (from `SIP_Module`):**
    - `Npx`, `Npy`, `Npz`: Grid dimensions.
    - `epsMat(0:Npx-1,0:Npy-1,0:Npz-1)`: 3D array of dielectric constants at each grid point.
    - `materialType(0:Npx-1,0:Npy-1,0:Npz-1)`: 3D array indicating the type of material at each grid point.
    - `hx`, `hy`, `hz`: Grid spacings in x, y, and z directions.
- **Output/Modified Variables (in `SIP_Module`):**
    - `Bvec(1:Npx-1,1:Npy-1,1:Npz-1)`
    - `Cvec(1:Npx-1,1:Npy-1,1:Npz-1)`
    - `Dvec(1:Npx-1,1:Npy-1,1:Npz-1)`
    - `Evec(1:Npx-1,1:Npy-1,1:Npz-1)`
    - `Fvec(1:Npx-1,1:Npy-1,1:Npz-1)`
    - `Gvec(1:Npx-1,1:Npy-1,1:Npz-1)`
    - `Hvec(1:Npx-1,1:Npy-1,1:Npz-1)`
    (These are the seven coefficient arrays for the SIP method, allocated and populated by this subroutine).
- **Local Variables:**
    - `epsPlusHalf`, `epsMinusHalf`: Temporary variables for averaged dielectric constants at cell interfaces.

## Usage Examples

`SIP_Coefficients` is called once during the initialization phase of the Poisson solver, typically from `SIP_Main` if it's the first iteration.

```fortran
! In SIP_Main (within SIP_Routines.F90)
USE SIP_Module
IMPLICIT NONE

IF (ite == 1) THEN  ! First iteration
    CALL Initialize_Grid      ! Sets up epsMat, materialType, hx, hy, hz
    CALL Initialize_Doping
    CALL SIP_Coefficients   ! Calculates Bvec through Hvec
ELSE
    ! ...
ENDIF
```

## Dependencies and Interactions

- **`USE SIP_Module`**: Accesses grid parameters (`Npx`, `Npy`, `Npz`, `hx`, `hy`, `hz`), material properties (`epsMat`, `materialType`), and allocates/populates the coefficient arrays (`Bvec` through `Hvec`) within this module.
- **Input Data**: Critically depends on `epsMat`, `materialType`, and grid spacings (`hx`, `hy`, `hz`) being correctly initialized before this subroutine is called. This is typically done by `Initialize_Grid`.
- **Output Data**: The primary output is the set of populated coefficient arrays (`Bvec` through `Hvec`) stored in `SIP_Module`. These arrays are then used by the `SIP_V1` solver subroutine.
