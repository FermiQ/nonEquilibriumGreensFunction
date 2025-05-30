# Documentation for Source/SIP_Solver.F90

## Overview

This file contains the subroutines that implement the iterative solution of the 3D Poisson equation using Stone's Strongly Implicit Procedure (SIP). The main routine `Solve_Equilibrium` sets up the charge density and boundary conditions, then calls `SIP_V1` to perform one iteration of the SIP algorithm. `SIP_V1` contains the core logic for the forward and backward sweeps of the SIP method to calculate the potential correction `PhiDelta`, which is then used to update the potential `Phi`.

## Key Components

- **`SUBROUTINE Solve_Equilibrium`**:
    - This routine is called in each self-consistent NEGF iteration (after the first one, which does full initialization via `SIP_Main`).
    - **If `poissonIsConverged == 0`**:
        - Updates electron (`n`) and hole (`p`) densities from `RhoN` and `RhoP` (from `NEGF_Module`).
        - Calculates the total charge density `ro = n - p + doping`.
        - Resets `PhiDelta` (potential correction) to zero.
        - **Sets Dirichlet boundary conditions for `Phi`**:
            - `Phi(-1:0,:,:) = mu(1)` (potential at the left contact/boundary).
            - `Phi(Npx-1:Npx,:,:) = mu(2)` (potential at the right contact/boundary).
            (Note: `mu(1)` and `mu(2)` are likely chemical potentials from `NEGF_Module`, used here to set fixed potentials at boundaries).
        - Updates `alpha`, the SIP iteration parameter, by selecting a value from `alphaArray` based on the current NEGF iteration `ite`.
        - **`CALL SIP_V1`**: Executes one iteration of the SIP algorithm to update `Phi`.
    - Prints the current NEGF iteration number (`ite`) and the `poissonIsConverged` status if `ite > 1`.

- **`SUBROUTINE SIP_V1`**:
    - Implements one iteration of Stone's Strongly Implicit Procedure.
    - Allocates local arrays for SIP temporary coefficients (`a` through `g`), residual `R_vec`, and intermediate vector `V_vec`.
    - The SIP method involves decomposing the matrix `A` (from `A*Phi = ro`) into `(L+P)P^-1(U+P) = LDU` approximately, where `L` and `U` are lower/upper triangular and `P` is a perturbation matrix. The solution involves forward and backward substitution-like sweeps.
    - **Alternating Sweep Directions**: The calculation of SIP coefficients (`a` through `g`) and the subsequent sweeps for `V_vec` and `PhiDelta` are performed with different loop orders depending on whether the NEGF iteration `ite` is even or odd. This is a common technique in SIP to improve stability and convergence.
        - **If `mod(ite,2)==0` (Even Iteration - Forward-like sweep for coefficients)**:
            - Loops `kk = 1..Npz-1`, `jj = 1..Npy-1`, `ii = 1..Npx-1`.
            - Calculates SIP coefficients `a, b, c, d, e, f, g` based on `Bvec` through `Hvec` (from `SIP_Module`), `alpha`, and previously computed `e, f, g` values from neighboring points. `Evec_new` incorporates the charge density `ro`.
            - Calculates the residual `R_vec(ii,jj,kk) = ro(ii,jj,kk) - [A*Phi](ii,jj,kk)`.
            - Calculates `V_vec` using a forward substitution-like sweep.
            - Calculates `PhiDelta` (potential correction) using a backward substitution-like sweep.
        - **If `mod(ite,2)==1` (Odd Iteration - Backward-like sweep for coefficients)**:
            - Loops `kk = Npz-1..1`, `jj = Npy-1..1`, `ii = 1..Npx-1` (note different loop order for `kk`, `jj`).
            - Similar calculations for `a` through `g`, `R_vec`, `V_vec`, and `PhiDelta`, but with formulas adapted for the reversed sweep direction.
    - **Convergence Check**:
        - After calculating `PhiDelta`, it checks for convergence:
            - `IF ( ite < 2 .OR. maxval(abs(PhiDelta)/(1D-10+abs(Phi))) > 1D-2 ) THEN poissonIsConverged = 0 ELSE poissonIsConverged = 1`.
            - Convergence is declared if the maximum relative change in potential is less than 1% (0.01), or if `ite < 2` (skips check for the very first iteration).
    - **Update Potential**: `Phi = Phi + PhiDelta`.
    - Deallocates local arrays.
    - Includes `STOP` conditions if any SIP coefficient `a` through `g` becomes `NaN`, or if `d` is zero.

## Important Variables/Constants

This file uses variables primarily from `SIP_Module` and `NEGF_Module`.
- **From `SIP_Module` / `NEGF_Module`:**
    - `poissonIsConverged`: Flag indicating convergence.
    - `n`, `p`: Electron and hole densities (local copies of `RhoN`, `RhoP`).
    - `RhoN`, `RhoP`: Electron and hole densities from NEGF solver (input).
    - `ro`: Total charge density (calculated).
    - `doping`: Fixed doping profile.
    - `Phi`: Electrostatic potential (input and output, updated).
    - `PhiDelta`: Correction to potential (calculated and applied).
    - `mu(1)`, `mu(2)`: Boundary potentials (from `NEGF_Module`).
    - `alpha`, `alphaArray`: SIP iteration parameters.
    - `ite`: Current NEGF iteration number (from `NEGF_Module`).
    - `Npx`, `Npy`, `Npz`: Grid dimensions.
    - `Bvec` through `Hvec`: SIP coefficients (input, from `SIP_Coefficients.F90`).
    - `materialType`: Used to adjust `Evec_new`.
    - `Evec`: Original diagonal coefficient, modified to `Evec_new` with charge.
- **Local to `SIP_V1`:**
    - `a, b, c, d, e, f, g`: Allocatable 3D arrays for SIP iteration coefficients.
    - `Evec_new`: Temporary variable for the modified diagonal SIP coefficient.
    - `R_vec`: Allocatable 3D array for the residual.
    - `V_vec`: Allocatable 3D array for an intermediate vector in SIP.

## Usage Examples

`Solve_Equilibrium` is called by `SIP_Main` (in `SIP_Routines.F90`) for `ite > 1`. `SIP_V1` is called by `Solve_Equilibrium`.

```fortran
! In SIP_Main (SIP_Routines.F90)
IF (ite == 1) THEN
    ! ... initialization ...
ELSE
    CALL Solve_Equilibrium ! This will call SIP_V1
ENDIF
```

## Dependencies and Interactions

- **`USE SIP_Module`**: Both subroutines heavily rely on this module for parameters, input arrays (like `Bvec`-`Hvec`, `RhoN`, `RhoP`), and for storing the main result (`Phi`, `poissonIsConverged`).
- **Data Flow**:
    - `Solve_Equilibrium` takes `RhoN`, `RhoP` from `NEGF_Module` (via `SIP_Module`) to calculate `ro`.
    - It sets boundary conditions on `Phi`.
    - It calls `SIP_V1`.
    - `SIP_V1` uses `ro`, `Phi` (current estimate), and SIP coefficients (`Bvec`-`Hvec`, `alpha`) to calculate `PhiDelta`.
    - `SIP_V1` updates `Phi` and `poissonIsConverged`.
- **Algorithm**: `SIP_V1` implements the core iterative steps of Stone's Strongly Implicit Procedure. The use of alternating sweep directions (`mod(ite,2)`) is a key feature of some SIP implementations.
- **Error Handling**: `SIP_V1` includes `STOP` conditions for NaN values in its internal coefficients, which can indicate numerical instability or issues with input parameters/coefficients.
