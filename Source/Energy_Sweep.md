# Documentation for Source/Energy_Sweep.F90

## Overview

The `Energy_Sweep` subroutine performs the core calculations for a single energy point within the NEGF simulation. It is called iteratively for each energy in the specified energy range. For a given energy, it calculates contact self-energies, constructs the inverse retarded Green's function, computes the retarded Green's function, then calculates electron and hole correlation functions (`Gn`, `Gp`) and corresponding densities (`RhoN`, `RhoP`). Finally, if it's the last self-consistent iteration or if the Poisson solver has converged, it calculates various physical observables.

## Key Components

- **`SUBROUTINE Energy_Sweep`**:
    - Prints the current energy step counter `eCtr` if on the MPI root process.
    - Updates `deltaE` (energy step for integration) if not the last energy point, allowing for non-uniform energy meshes.
    - Calculates Fermi-Dirac distribution factors (`f1`, `f2`) and their complements (`f1v`, `f2v`) based on the current `energy` and chemical potentials `mu(1)`, `mu(2)`, `muT`. (This is a zero-temperature step-function approximation).
    - Initializes various Green's function and self-energy matrices in `NEGF_Module` to zero (e.g., `GRd`, `SigmaL`).
    - **`CALL Contact_Self_Energy`**: Computes the self-energies (`SigmaL`, `SigmaR`) and broadening functions (`GammaL`, `GammaR`) for the contacts at the current `energy`.
    - **Constructs Inverse Retarded Green's Function (`InvGRd`)**:
        - `InvGRd = (energy + i*eta_sigma)*I - Htotd` (for bulk slices).
        - `InvGRd(:,:,1) = InvGRd(:,:,1) - SigmaL` (adds left contact self-energy to the first slice).
        - `InvGRd(:,:,Npx) = InvGRd(:,:,Npx) - SigmaR` (adds right contact self-energy to the last slice).
        - (Note: `Sigmav1`, `Sigmav2` mentioned in a comment are not explicitly used here, implying they are zero or handled within `Contact_Self_Energy` or `Get_Retarded_Greens` if non-zero).
    - **`CALL Get_Retarded_Greens`**: Calculates the retarded Green's function matrices (`GRd`, `GRu`, `GRl`, `GRLef`, `GRRgt`) by inverting `InvGRd`. This likely involves a recursive Green's function algorithm.
    - **`CALL Calculate_Gnp`**: Calculates the electron (`Gnd`, `Gnu`, `Gnl`) and hole (`Gpd`, `Gpu`, `Gpl`) correlation functions, and updates the electron (`RhoN`) and hole (`RhoP`) density matrices by integrating the results for the current energy slice.
    - **(Conditional) `CALL Calculate_Observables`**: If it's the maximum iteration (`ite == iteMax`) or if the Poisson solver has converged (`poissonIsConverged == 1`), this call computes various physical observables like transmission, current, DOS, etc.

## Important Variables/Constants

This subroutine uses and modifies variables primarily from `SIP_Module` (which likely includes `NEGF_Module`).
- **Input/State Variables (from `SIP_Module`/`NEGF_Module`):**
    - `energy`: The current energy point for calculation.
    - `eCtr`: Current energy step counter.
    - `eStpnum`: Total number of energy steps.
    - `EAxis`: Array of energy points.
    - `muT`, `mu(1)`, `mu(2)`: Chemical potentials.
    - `Htotd`: Total device Hamiltonian.
    - `eta_sigma`: Small imaginary energy broadening.
    - `Npx`, `NNz`: Grid dimensions.
    - `mpiRank`, `mpiRoot`: MPI process identifiers.
    - `ite`, `iteMax`: Current and maximum self-consistent iteration count.
    - `poissonIsConverged`: Flag indicating Poisson solver convergence.
- **Output/Modified Variables (in `SIP_Module`/`NEGF_Module`):**
    - `deltaE`: Energy step size (potentially updated).
    - `f1`, `f2`, `f1v`, `f2v`: Fermi function factors.
    - `SigmaL`, `SigmaR`, `GammaL`, `GammaR`: Contact self-energies and broadenings.
    - `InvGRd`: Inverse retarded Green's function for the device.
    - `GRd`, `GRu`, `GRl`, `GRLef`, `GRRgt`: Components of the retarded Green's function.
    - `Gnd`, `Gnu`, `Gnl`, `Gpd`, `Gpu`, `Gpl`: Electron and hole correlation functions.
    - `RhoN`, `RhoP`: Electron and hole density matrices (accumulated).
    - Various observables if `Calculate_Observables` is called (e.g., `T12`, `Jx`, `EDensity`).
- **Constants (likely from `Constants` module):**
    - `c0`: Complex zero.

## Usage Examples

`Energy_Sweep` is designed to be called within a loop that iterates over an energy axis, typically from `NEGF_Main.F90`.

```fortran
! In NEGF_Main.F90
DO eCtr = 0, eStpnum-1
    ! Split up energy integration over each mpi process
    IF ((mpiSize == 1) .OR. (mod(eCtr, mpiSize) == mpiRank)) THEN
        energy = EAxis(eCtr+1)
        CALL Energy_Sweep
    ENDIF
ENDDO
```

## Dependencies and Interactions

- **`USE SIP_Module`**: Accesses and modifies numerous shared variables from `SIP_Module` and implicitly `NEGF_Module`.
- **Calls Subroutines**:
    - `Contact_Self_Energy`: To calculate contact properties.
    - `Get_Retarded_Greens`: To calculate the device's Green's function. (Likely `Recursive_Greens.F90`)
    - `Calculate_Gnp`: To calculate correlation functions and densities.
    - `Calculate_Observables`: (Conditional) To calculate final physical quantities.
- **Input Data**: Relies on `Htotd` being correctly constructed for the current self-consistent iteration, and parameters like `energy`, `mu`, `eta_sigma` being set.
- **Output Data**: Updates Green's functions, self-energies, densities, and potentially observables in the shared modules for the current energy point. `RhoN` and `RhoP` are typically accumulated across energy points.
- **MPI Context**: The main loop in `NEGF_Main.F90` shows that `Energy_Sweep` calls are distributed among MPI processes if `mpiSize > 1`. The subroutine itself prints the energy counter only on the `mpiRoot` process.
