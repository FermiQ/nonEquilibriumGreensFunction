# Documentation for Source/Read_In_Allocate.F90

## Overview

The `Read_In_Allocate` subroutine is responsible for initializing the simulation by reading input parameters, setting up derived parameters, and allocating memory for the numerous arrays used throughout the NEGF calculations. It handles reading parameters from command-line arguments or using default values if arguments are insufficient. It also defines and allocates various gamma matrices used in the Hamiltonian construction.

## Key Components

- **`SUBROUTINE Read_In_Allocate`**:
    - **Read Input Parameters**:
        - Uses `iargc()` and `getarg()` to read 19 command-line arguments. These include:
            - `Nprocs`: Number of MPI processors.
            - `Npx`, `Npy`, `Npz`: Grid dimensions.
            - `bcy`, `bcz`: Boundary conditions.
            - `mu(1)`, `mu(2)`, `muT`: Chemical potentials.
            - `channelType`, `contactType`: Type of material/contact.
            - `a0`: Lattice constant.
            - `abPhi`: Aharonov-Bohm phase.
            - `disorderStrength`, `corrLength`, `randSeed`: Disorder parameters.
            - `includePoisson`: Flag for Poisson solver.
            - `iteMax`: Maximum self-consistent iterations.
            - `outputfiledir`: Output directory path.
        - If the number of arguments is not 19, it prints a message and sets default values for these parameters.
        - Calls `MPI_Initialize` (from `MPI_Routines.F90` or dummy version) after `Nprocs` is read.
    - **Set Derived Parameters**:
        - Sets `Norb` (number of orbitals) based on `channelType`.
        - Sets `Nh = Norb`.
        - If `contactType == 3` (superconducting), sets `isBdGHam = 1`, doubles `Norb`, and sets SC parameters (`chiLR`, `mu`, `DeltaSCL`, `DeltaSCR`).
        - Validates and adjusts `abPhi` if periodic boundary conditions are used.
        - Sets other simulation control parameters like `eStpnum` (number of energy points), `magneticRingWidth`, `magneticDisorderStrength`, `isHollowRing`, `erCD`, `orCD`, `calcSigExp`.
        - Calculates composite grid dimensions: `NN = Npy*Norb`, `NNz = Npz*NN`, `Ntot = Npx*Npy*Npz*Norb`.
        - Adjusts `iteMax` to 1 if `includePoisson == 0`.
    - **Allocate Arrays**:
        - Allocates numerous large arrays stored in `NEGF_Module`. These include:
            - Hamiltonian matrices: `Htotd`, `HopX`.
            - Green's function components: `InvGRd`, `GRd`, `GRu`, `GRl`, `GRLef`, `GRRgt`.
            - Correlation functions: `Gnd`, `Gpd`, `Gnu`, `Gpu`, `Gnl`, `Gpl`.
            - Density matrices: `RhoN`, `RhoP`.
            - Spin expectation values (conditional): `SigXExp`, `SigYExp`, `SigZExp`.
            - Self-energies and broadenings: `GammaL`, `GammaR`, `SigmaL`, `SigmaR`.
            - Observable arrays: `EDensity`, `HDensity`, `T12`, `Modec1`, `Modec2`, `DOSc1`, `DOSc2`.
            - Energy axis: `EAxis`.
            - Current densities: `Jx`, `Jy`, `Jz`.
            - Disorder potential: `DeltaR`.
            - Contact current: `ContactCurrent`.
            - Gamma matrices: `g1`, `g2`, `g3`, `g0`, `gMx`, `gMy`, `gMz`, `gDS`.
            - On-site Hamiltonian block `AA` and identity matrix `id`.
            - Conditional arrays for energy/orbital resolved data: `ERJx`, `ERRhon`, `ORJx`, `ORRhon`.
        - Initializes allocated arrays mostly to zero (real or complex).
    - **Set Up Gamma Matrices**:
        - Defines the elements of gamma matrices (`g1`, `g2`, `g3`, `g0`, `gMx`, `gMy`, `gMz`, `gDS`) based on `Nh` (number of orbitals, usually 2 or 4). Specific definitions are provided for 4-orbital (Chen's Basis for TI model) and 2-orbital systems.
    - **Set Up Energy Range**:
        - Defines `eAbsMin` and `eAbsMax` for the energy integration based on chemical potentials, Poisson flag, or SC gap.
        - Sets `eMin = eAbsMin`, `eMax = eAbsMax`.
        - Calculates `deltaE` for a uniform energy mesh.
        - Populates `EAxis` with energy points.

## Important Variables/Constants

This subroutine initializes almost all variables in `NEGF_Module`.
- **Command-Line Interface**: `iargc()`, `getarg()`.
- **MPI Initialization**: `CALL MPI_Initialize`.
- **All variables listed in `NEGF_Module.F90` are essentially initialized or allocated here.** This includes parameters defining the physical system, simulation control, grid, and all major arrays for Hamiltonians, Green's functions, self-energies, and observables.
- **Local `arg`**: Character variable to temporarily store command-line arguments.
- **Constants (from `Constants` module via `NEGF_Module`):** `c0`, `c1`, `ci`.

## Usage Examples

`Read_In_Allocate` is one of the first subroutines called in the main program, `NEGF_Main.F90`.

```fortran
! In NEGF_Main.F90
PROGRAM NEGF_Main
    USE SIP_Module ! Which uses NEGF_Module
    IMPLICIT NONE

    ! This call reads inputs, initializes MPI (if Nprocs > 1 via command line),
    ! sets parameters, and allocates all major arrays.
    CALL Read_In_Allocate

    ! ... rest of the simulation ...
END PROGRAM NEGF_Main
```

## Dependencies and Interactions

- **`USE NEGF_Module`**: This subroutine is central to populating and preparing the data structures within `NEGF_Module`.
- **Command Line**: Relies on the Fortran intrinsics `iargc` and `getarg` for input.
- **`MPI_Initialize`**: Calls this routine (from `MPI_Routines.F90` or the dummy version in `MKL_Routines_PC.F90`) to set up MPI parameters based on the `Nprocs` argument. This is a crucial interaction for parallel execution.
- **Output**: The primary "output" is the state of all variables and allocated arrays within `NEGF_Module`. No files are written by this routine itself, but it prepares `outputfiledir`.
- **Error Handling**: Includes a `STOP` call if `channelType` is invalid. Also, some parameter sanity checks (e.g., `abPhi` with periodic boundaries).
