# Documentation for Source/Calculate_Observables.F90

## Overview

The `Calculate_Observables` subroutine computes various physical quantities of interest from the Green's functions and Hamiltonian components. These observables include spin expectation values, electron and hole density of states (DOS), injected mode densities, transmission coefficients (including supercurrent for BdG Hamiltonians), contact currents, and spatially resolved current densities.

## Key Components

- **`SUBROUTINE Calculate_Observables`**:
    - **Spin Expectation Values**:
        - If `calcSigExp` is true, calculates `SigXExp`, `SigYExp`, `SigZExp` by summing products of `Gnd` and Pauli spin matrices (`gMx`, `gMy`, `gMz`) over spatial points and orbitals, integrating over energy (`deltaE`).
    - **Electron and Hole DOS**:
        - Calculates `EDensity` (electron DOS) and `HDensity` (hole DOS) as functions of energy by summing the real parts of the diagonal elements of `Gnd` and `Gpd`, respectively, over all sites.
    - **Injected Modes**:
        - Calculates `Modec1` and `Modec2` (modes injected from contact 1 and 2) using traces involving `GammaL`, `GammaR`, `Gnd`, and `Gpd`.
    - **Transmission and Contact Currents**:
        - Allocates temporary matrices `cmat1`, `cmat2`.
        - **If `isBdGHam` (Bogoliubov-de Gennes Hamiltonian for superconductors)**:
            - Calculates supercurrent transmission `T12` based on a formula involving `SigmaL`, `GammaL`, `GRd`, and `Gnd`.
            - Updates `ContactCurrent(1)` and `ContactCurrent(2)` using `T12`.
        - **Else (normal conductor)**:
            - Calculates `T12` using the standard formula: `real(trace(GammaL*GRRgt*GammaR*GRRgt'))`.
            - Updates `ContactCurrent(1)` and `ContactCurrent(2)` using `T12` and Fermi function differences (`f1`, `f2`).
        - Deallocates `cmat1`, `cmat2`.
    - **Spatially Resolved Current Profile**:
        - Iterates through spatial grid points (`xx`, `yy`, `zz`) and orbital pairs (`ii`, `jj`).
        - Calculates current density components `Jx`, `Jy`, `Jz` using formulas involving Hamiltonian elements (`Htotd`, `HopX`) and Green's functions (`Gnd`, `Gpd`, `Gnu`, `Gnl`, `Gpu`, `Gpl`).
        - (Conditionally) Accumulates energy-resolved current densities (`ERJx`, `ERJy`, `ERJz`) if `erCD == 1`.
        - (Conditionally) Accumulates orbital-resolved current densities (`ORJx`, `ORJy`, `ORJz`) if `orCD == 1`.

## Important Variables/Constants

This subroutine extensively uses variables from `NEGF_Module`:
- **Input/Operand Data (from `NEGF_Module`):**
    - `Gnd`, `Gpd`, `Gnu`, `Gnl`, `Gpu`, `Gpl`: Electron and hole correlation functions.
    - `GRd`, `GRRgt`: Retarded Green's functions.
    - `GammaL`, `GammaR`, `SigmaL`: Broadening and self-energy matrices from contacts.
    - `Htotd`, `HopX`: Hamiltonian matrices.
    - `gMx`, `gMy`, `gMz`: Pauli spin matrices.
    - `deltaE`: Energy step.
    - `eCtr`: Current energy step counter.
    - `f1`, `f2`: Fermi function factors.
    - `calcSigExp`, `isBdGHam`, `erCD`, `orCD`: Flags controlling specific calculations.
    - `Npx`, `Npy`, `Npz`, `NN`, `NNz`, `Norb`, `Nh`: Grid and orbital parameters.
- **Output/Result Arrays (modified in `NEGF_Module`):**
    - `SigXExp`, `SigYExp`, `SigZExp`: Spin expectation values.
    - `EDensity`, `HDensity`: Electron and hole density of states.
    - `Modec1`, `Modec2`: Injected mode densities.
    - `T12`: Transmission coefficient.
    - `ContactCurrent`: Currents at the contacts.
    - `Jx`, `Jy`, `Jz`: Spatially resolved current densities.
    - `ERJx`, `ERJy`, `ERJz`: (Conditional) Energy-resolved current densities.
    - `ORJx`, `ORJy`, `ORJz`: (Conditional) Orbital-resolved current densities.
- **Constants (likely from `Constants` module via `NEGF_Module`):**
    - `pi`: Mathematical constant pi.
    - `q`: Elementary charge.
    - `hbar`: Reduced Planck constant.
    - `ci`: Imaginary unit.
- **Local Variables:**
    - `index`: Integer for matrix indexing.
    - `cmat1`, `cmat2`: Allocatable complex matrices for temporary calculations.
    - `dumVar`: Real temporary variable for current components.
    - `sigmaZFactor`: Real factor for BdG current calculation.

## Usage Examples

This subroutine is called within an energy loop, typically after `Calculate_Gnp` has computed the necessary correlation functions for a specific energy point.

```fortran
! In an energy sweep loop (e.g., within Energy_Sweep.F90)
USE NEGF_Module
IMPLICIT NONE

! ... (previous calculations for Gnd, Gpd, etc. by Calculate_Gnp for current 'energy') ...

CALL Calculate_Observables

! ... (Observables like T12(eCtr+1), Jx, Jy, Jz etc. in NEGF_Module are now updated for the current energy) ...
```

## Dependencies and Interactions

- **`USE NEGF_Module`**: Explicitly uses `NEGF_Module` for nearly all its input data and for storing all calculated results.
- **Input Data**: Critically depends on the Green's functions (`Gnd`, `Gpd`, etc.), Hamiltonian (`Htotd`, `HopX`), and self-energies (`GammaL`, `SigmaL`, etc.) being up-to-date for the current energy point. `Calculate_Gnp` is usually called before this routine.
- **Output Data**: Modifies a wide range of observable arrays in `NEGF_Module`. These are typically integrated over energy (e.g., `ContactCurrent`, `Jx`) or stored as energy-resolved spectra (e.g., `T12`, `EDensity`).
- **Called by**: Likely called from `Energy_Sweep.F90` or a similar routine that iterates over energy points, following the calculation of correlation functions.
