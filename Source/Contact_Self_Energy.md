# Documentation for Source/Contact_Self_Energy.F90

## Overview

This file contains subroutines related to calculating the self-energies (`SigmaL`, `SigmaR`) and corresponding broadening functions (`GammaL`, `GammaR`) for the contacts (leads) of the simulated device. These quantities are essential for describing how the semi-infinite leads are coupled to the central device region in the NEGF formalism. The main subroutine `Contact_Self_Energy` handles different types of contacts, including phenomenological models and self-consistent calculations using the Sancho-Rubio iterative algorithm. The file also includes the `Sancho_Rubio` algorithm itself and a `Recursive_Inversion` routine, likely for more complex contact geometries (e.g., graphene).

## Key Components

- **`SUBROUTINE Contact_Self_Energy`**:
    - Initializes local matrices `alpha` and `beta`.
    - Sets `contactEnergy` to `energy + ci*eta_sigma` (adds a small imaginary part).
    - Selects the method for calculating `SigmaL` and `SigmaR` based on `contactType`:
        - **`contactType == 1` (Phenomenological, big contact)**: Sets diagonal elements of `SigmaL` (and `SigmaR`) to `-ci*tunnelingRate`.
        - **`contactType == 2` (Phenomenological, small contact)**: Similar to type 1, but potentially for a smaller portion of the contact interface (`NN` vs `NNz`).
        - **`contactType == 3` (Phenomenological SC contacts)**: Calculates `SigmaL` and `SigmaR` for superconducting contacts using formulas involving `DeltaSCL`, `DeltaSCR` (superconducting gaps), `chiLR` (phase difference), and `tunnelingRate`. This model is based on PRB 62, 648 (2000).
        - **`ELSE` (Default: Semi-infinite extension of the channel)**:
            - For `SigmaL`: Sets up `alpha = contactEnergy*I - Htotd(:,:,1)` and `beta = -conjg(transpose(HopX))`. Calls `Sancho_Rubio` to compute `SigmaL`.
            - For `SigmaR`: Sets up `alpha = contactEnergy*I - Htotd(:,:,Npx)` and `beta = -HopX`. Calls `Sancho_Rubio` to compute `SigmaR`.
    - Calculates density of states for contacts `DOSc1` and `DOSc2` from the imaginary parts of `SigmaL` and `SigmaR`.
    - Computes broadening matrices: `GammaL = ci*(SigmaL - SigmaL')` and `GammaR = ci*(SigmaR - SigmaR')`.
    - Deallocates local `alpha` and `beta`.

- **`SUBROUTINE Sancho_Rubio(NN, alpha, beta, Sigma, errorFlag)`**:
    - Implements the Sancho-Rubio algorithm (J. Phys. F Vol 14, 1984, 1205) to calculate the self-energy `Sigma` for a semi-infinite lead.
    - Takes `NN` (matrix size), `alpha` (on-site Hamiltonian of the lead slice), `beta` (coupling matrix to the next slice), and outputs `Sigma` and an `errorFlag`.
    - Iteratively computes surface Green's function components `t` and `tt` until convergence (change is small or max iterations reached).
    - Uses `zgemm` for matrix multiplications and `Invert_Matrix` for matrix inversions.
    - The final self-energy is `Sigma = beta * gfinal * beta'`, where `gfinal` is the surface Green's function.

- **`SUBROUTINE Recursive_Inversion(NN, alpha1, beta1, alpha2, beta2, Sigma, errorFlag)`**:
    - A recursive Green's function method, likely for contacts composed of a repeating bi-layer unit cell (e.g., graphene).
    - Takes on-site Hamiltonians (`alpha1`, `alpha2`) and coupling matrices (`beta1`, `beta2`) for the two layers in the unit cell.
    - Iteratively solves for the Green's functions `g_old1` and `g_old2` until convergence.
    - Calculates the final self-energy `Sigma = beta1 * gfinal * beta1'`.
    - Uses `zgemm` and `Invert_Matrix`.

## Important Variables/Constants

This file uses variables from `NEGF_Module` and `Constants` (via `NEGF_Module`).
- **From `NEGF_Module`:**
    - `SigmaL`, `SigmaR`: Contact self-energies (output).
    - `GammaL`, `GammaR`: Contact broadening functions (output).
    - `DOSc1`, `DOSc2`: Density of states for contacts (output).
    - `energy`: Current energy value.
    - `contactType`: Specifies the model for contact self-energy calculation.
    - `NNz`, `NN`, `Norb`, `Nh`, `Npx`, `Npy`: Grid and orbital parameters.
    - `Htotd`, `HopX`: Device Hamiltonian and hopping matrices.
    - `DeltaSCL`, `DeltaSCR`, `chiLR`: Superconducting contact parameters.
    - `gDS`, `id`: Gamma matrices/identity matrix.
    - `errorFlag`: Error status indicator.
    - `eCtr`: Current energy counter.
- **From `Constants` (via `NEGF_Module` or directly if `USE Constants` is present in subroutines):**
    - `ci`, `c0`, `c1`: Complex constants (imaginary unit, zero, one).
    - `eta_sigma`: Small imaginary part for energy.
    - `pi`: Mathematical constant pi.
- **Local to `Contact_Self_Energy`:**
    - `alpha`, `beta`: Allocatable complex matrices for Sancho-Rubio input.
    - `tunnelingRate`: Phenomenological parameter for contact coupling.
    - `contactEnergy`, `omega`, `mAlpha`, `dAlphaU`, `dalphaL`: Variables for SC contact calculations.
- **Local to `Sancho_Rubio` and `Recursive_Inversion`:**
    - `tmp`, `betad`, `t`, `tt`, `CapT`, `Toldt`, `IdMat`, `cmat1`, `cmat2`, `change`, `gfinal`: Various allocatable complex matrices for iterative calculations.
    - `counter`, `ii`, `jj`: Loop counters and indices.
    - `eps`, `epstmp`, `damping`: Convergence parameters for `Recursive_Inversion`.

## Usage Examples

`Contact_Self_Energy` is typically called at each energy step before calculating Green's functions for the device.

```fortran
! In an energy sweep loop (e.g., within Energy_Sweep.F90)
USE NEGF_Module
IMPLICIT NONE

! ... (energy is set for the current step) ...

CALL Contact_Self_Energy

! ... (SigmaL, SigmaR, GammaL, GammaR in NEGF_Module are now updated for the current energy) ...
! ... (These are then used to calculate device Green's functions GRd, GRl, GRu etc.) ...
```
The `Sancho_Rubio` and `Recursive_Inversion` subroutines are called internally by `Contact_Self_Energy` when `contactType` dictates their use.

## Dependencies and Interactions

- **`USE NEGF_Module`**: `Contact_Self_Energy` uses this module extensively.
- **`USE Constants`**: `Sancho_Rubio` and `Recursive_Inversion` use this module.
- **Calls `Sancho_Rubio`**: `Contact_Self_Energy` calls `Sancho_Rubio` if `contactType` is the default.
- **Calls `Invert_Matrix` and `zgemm`**: `Sancho_Rubio` and `Recursive_Inversion` call a matrix inversion routine (presumably `Invert_Matrix` from `MKL_Routines_*.F90` or similar) and a matrix multiplication routine (`zgemm`, likely from BLAS/MKL).
- **Input**: `energy` and various parameters from `NEGF_Module`.
- **Output**: Modifies `SigmaL`, `SigmaR`, `GammaL`, `GammaR`, `DOSc1`, `DOSc2` in `NEGF_Module`. Sets `errorFlag` if convergence issues arise.
- **Called by**: Typically called from `Energy_Sweep.F90` or a similar top-level routine controlling the energy loop.
