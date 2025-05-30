# Documentation for Source/Build_Hamiltonian.F90

## Overview

This file provides subroutines for constructing and modifying the Hamiltonian matrix (`Htotd` and `HopX`) of the simulated quantum system. The main subroutine, `Build_Hamiltonian`, sets up the basic tight-binding Hamiltonian based on the specified `channelType` and then incorporates various physical effects such as BdG formalism for superconductivity, electrostatic potentials, Aharonov-Bohm phase, disorder, and magnetic impurities. Helper subroutines are included for adding specific modifications like impurities, vacancies, and magnetic disorder.

## Key Components

- **`SUBROUTINE Build_Hamiltonian`**:
    - Initializes hopping matrices (`HopX`, `HopY`, `HopZ`) and on-site energy terms (`AA`) based on `channelType`. Supported types include generic topological insulators, Bi2Se3, metals, TI surface states, and 1D quantum wires.
    - Optionally modifies these matrices for the Bogoliubov-de Gennes (BdG) Hamiltonian if `isBdGHam` is true.
    - Constructs 1D (`HH1d`) and 2D (`HH2d`) Hamiltonian slices by combining on-site and hopping terms, applying periodic boundary conditions if specified (`bcy`, `bcz`).
    - Assembles the full 3D Hamiltonian `Htotd` from the 2D slices.
    - Sequentially calls other subroutines or applies modifications if corresponding flags/parameters are set:
        - `Insert_Impurity`: If `includePoisson == 1`, adds electrostatic potential `Phi`.
        - `Add_AB_Phase`: If `abPhi` is significant, adds Aharonov-Bohm phase.
        - `Add_Correlated_Disorder`: If `disorderStrength` is significant.
        - `Add_Magnetic_Ring`: If `magneticRingWidth > 0`.
        - `Insert_Vacancy`: If `isHollowRing` is true, creates vacancies.
    - Deallocates local temporary matrices (`HH2d`, `HH1d`, `HopY`, `HopZ`).

- **`SUBROUTINE Insert_Impurity(xPos, yPos, zPos, onsitePot)`**:
    - Modifies the on-site energy block of `Htotd` at a given spatial position (`xPos`, `yPos`, `zPos`) by adding `onsitePot`.
    - Handles BdG Hamiltonians correctly by adjusting the corresponding particle-hole block.
    - Uses `NEGF_Module`.

- **`SUBROUTINE Insert_Vacancy(xPos, yPos, zPos)`**:
    - Creates a vacancy at the specified position by setting the corresponding rows and columns in `Htotd` and `HopX` to zero.
    - Uses `NEGF_Module`.

- **`SUBROUTINE Add_AB_Phase`**:
    - Modifies the hopping terms in `Htotd` to include a phase factor due to an Aharonov-Bohm flux, controlled by `abPhi`.
    - Handles BdG Hamiltonians.
    - Uses `NEGF_Module`.

- **`SUBROUTINE Add_Correlated_Disorder`**:
    - Adds correlated disorder to the system.
    - If `ite == 1` (first iteration):
        - Initializes a random number generator based on `randSeed`.
        - If `corrLength` is significant, generates a spatially correlated disorder potential `DeltaR` using a Fourier space method.
        - Otherwise, generates uncorrelated random disorder.
        - Normalizes and scales `DeltaR` by `disorderStrength`.
    - Calls `Insert_Impurity` to add the calculated `DeltaR(xx,yy,zz)` to each site.
    - Uses `SIP_Module` (and implicitly `NEGF_Module` through `Insert_Impurity` and `DeltaR`).

- **`SUBROUTINE Add_Magnetic_Ring`**:
    - Calls `Insert_Magnetic_Impurity` to add magnetic disorder in a ring shape around the device, controlled by `magneticRingWidth` and `magneticDisorderStrength`.
    - The pattern of magnetic field components depends on the position within the ring and boundary conditions.
    - Uses `NEGF_Module`.

- **`SUBROUTINE Insert_Magnetic_Impurity(xPos, yPos, zPos, magX, magY, magZ)`**:
    - Adds magnetic impurity terms (`magX*gMx + magY*gMy + magZ*gMz`) to the on-site block of `Htotd` at the specified position.
    - Handles BdG Hamiltonians.
    - Uses `SIP_Module` (and implicitly `NEGF_Module` for `Htotd`, `gMx` etc.).

## Important Variables/Constants

The subroutines in this file primarily operate on variables defined in `NEGF_Module` and `SIP_Module`. Key module variables used include:
- From `NEGF_Module` (or `SIP_Module` which likely uses `NEGF_Module`):
    - `Htotd`, `HopX`: The main Hamiltonian matrices being constructed/modified.
    - `AA`, `g0`-`g3`, `gMx`-`gMz`, `id`: Gamma matrices and identity matrix used for defining Hamiltonian terms.
    - `channelType`: Determines the base model for the Hamiltonian.
    - `Npx`, `Npy`, `Npz`, `NN`, `NNz`, `Norb`, `Nh`: Discretization and orbital parameters.
    - `a0`, `MM`, `mpC`, `mpD1`, etc.: Physical and material parameters.
    - `muT`: Chemical potential.
    - `isBdGHam`: Flag for BdG formalism.
    - `bcy`, `bcz`: Boundary condition flags.
    - `includePoisson`, `Phi`: Poisson solver flag and potential array.
    - `abPhi`: Aharonov-Bohm phase value.
    - `disorderStrength`, `corrLength`, `randSeed`, `DeltaR`: Disorder parameters.
    - `magneticRingWidth`, `magneticDisorderStrength`: Magnetic ring parameters.
    - `isHollowRing`: Flag for creating a hollow ring structure.
    - `ite`: Current iteration number (used in `Add_Correlated_Disorder`).
- Constants like `c0`, `c1`, `ci` (likely from `Constants` module via `NEGF_Module` or `SIP_Module`).

Local variables:
- `HH2d`, `HH1d`, `HopY`, `HopZ`: Temporary allocatable arrays within `Build_Hamiltonian` for staging parts of the Hamiltonian.
- `index`: Integer for calculating matrix indices.
- `onsitePot`, `randVal`, `qq`, `kmin`, `kmax`, `deltaK`, `KxAxis`, `KyAxis`, `KzAxis`, `DeltaQ`: Local variables within `Add_Correlated_Disorder`.
- `xMid`: Local variable in `Add_Magnetic_Ring`.
- `magX`, `magY`, `magZ`: Input arguments for `Insert_Magnetic_Impurity`.

## Usage Examples

These subroutines are typically called from the main simulation program (e.g., `NEGF_Main.F90`).

```fortran
! In a main program or higher-level subroutine
USE SIP_Module ! Which in turn likely uses NEGF_Module
IMPLICIT NONE

! ... (initialize parameters in NEGF_Module, SIP_Module) ...

! To build the Hamiltonian:
CALL Build_Hamiltonian

! Individual subroutines like Insert_Impurity are usually called
! by Build_Hamiltonian itself, but could be called directly if needed:
! CALL Insert_Impurity(1, 1, 1, 0.1D0) ! Add 0.1 potential to site (1,1,1)
```

## Dependencies and Interactions

- **`USE SIP_Module`**: Most subroutines use `SIP_Module`. Since `SIP_Module` likely uses `NEGF_Module`, variables from both are accessible. Some subroutines like `Insert_Impurity` explicitly state `USE NEGF_Module` if they are perhaps also intended for use in contexts where only `NEGF_Module` is directly included.
- **Inter-subroutine calls**:
    - `Build_Hamiltonian` calls:
        - `Insert_Impurity`
        - `Add_AB_Phase`
        - `Add_Correlated_Disorder`
        - `Add_Magnetic_Ring`
        - `Insert_Vacancy`
    - `Add_Correlated_Disorder` calls `Insert_Impurity` and `RANDOM_SEED`, `RANDOM_NUMBER`.
    - `Add_Magnetic_Ring` calls `Insert_Magnetic_Impurity`.
- **Module Variable Reliance**: Heavily relies on variables (parameters, flags, and main arrays like `Htotd`, `HopX`) being correctly set up and allocated in `NEGF_Module` and `SIP_Module` prior to these calls. This setup is typically done by routines like `Read_In_Allocate`.
- **Output**: The primary output of this file's subroutines are modifications to the global Hamiltonian arrays `Htotd` and `HopX` stored in `NEGF_Module`. `Add_Correlated_Disorder` also modifies `DeltaR` in `NEGF_Module`.
