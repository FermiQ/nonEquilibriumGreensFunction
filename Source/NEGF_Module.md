# Documentation for Source/NEGF_Module.F90

## Overview

The `NEGF_Module` Fortran module defines a wide range of global variables, parameters, and allocatable arrays that are central to the Non-Equilibrium Green's Function (NEGF) simulation. It serves as a common repository for data shared across different parts of the NEGF code, including physical constants, material parameters, simulation control flags, discretized grid dimensions, Hamiltonian components, Green's functions, self-energies, and various physical observables.

## Key Components

- **`MODULE NEGF_Module`**: The primary component, this module encapsulates all the defined variables and parameters. Other Fortran files in the project can `USE NEGF_Module` to access these shared data structures.

## Important Variables/Constants

This module is almost entirely composed of variable and constant declarations. Below is a categorized list of some key ones, along with descriptions often taken from comments in the code:

### Device and Material Parameters:
- **`MM`**: `REAL(8)`, Mass in Dirac Hamiltonian (natural units). Default: `15D-1`.
- **`tMet`**: `REAL(8)`, Hopping energy for metal (channel or contact). Default: `1D0`.
- **`mpA1`, `mpA2`, `mpC`, `mpD1`, `mpD2`, `mpM`, `mpB1`, `mpB2`**: `REAL(8), PARAMETER`, Material parameters specific to Bi2Se3 (e.g., `mpA1 = 2.26 eV*Angstroms`).
- **`a0`**: `REAL(8)`, Lattice constant (1nm = 10 Angstroms).
- **`channelType`**: `INTEGER`, Defines the type of channel material (e.g., 0 for natural units, 1 for Bi2Se3).
- **`contactType`**: `INTEGER`, Defines the type of contact material.

### Input Parameters (often from command line):
- **`nargs`**: `INTEGER`, Number of command-line arguments.
- **`arg`**: `CHARACTER*200`, Stores a command-line argument.
- **`outputfiledir`**: `CHARACTER*200`, Directory for output files.
- **`abStpnum`**: `INTEGER`, (Purpose not specified in comment).
- **`includePoisson`**: `INTEGER`, Flag to include Poisson equation solver.
- **`Npx`, `Npy`, `Npz`**: `INTEGER`, Number of points in x, y, z dimensions.
- **`Ntot`, `NN`, `NNx`, `NNz`, `Nh`, `Norb`**: `INTEGER`, Variables related to grid size and number of orbitals.
- **`randSeed`**: `INTEGER`, Initial seed for random impurity disorder.
- **`corrLength`**: `REAL`, Correlation length for disorder.

### Iteration and Boundary Conditions:
- **`ite`, `iteMax`**: `INTEGER`, Current iteration and maximum number of iterations.
- **`bcy`, `bcz`**: `INTEGER`, Boundary conditions in y and z directions (1 for periodic, 0 for open).

### Matrices and Physical Quantities:
- **Gamma Matrices**: `g1, g2, g3, g0, gMx, gMy, gMz, id, AA, gDS` - `COMPLEX(8), DIMENSION(:,:), ALLOCATABLE`.
- **Lead Chemical Potentials**: `mu(2)` (`REAL(8)`), `muT` (`REAL(8)`).
- **Disorder Variables**: `disorderStrength`, `magneticRingWidth`, `magneticDisorderStrength` (`REAL(8)`), `isHollowRing` (`INTEGER(4)`).
- **Density Matrix**: `RhoN`, `RhoP`, `SigXExp`, `SigYExp`, `SigZExp`, `DeltaR` - `REAL(8), DIMENSION(:,:,:), ALLOCATABLE`.
- **Hamiltonian Matrices**: `Htotd` (`COMPLEX(8), DIMENSION(:,:,:), ALLOCATABLE`), `HopX` (`COMPLEX(8), DIMENSION(:,:), ALLOCATABLE`).
- **Superconductor Parameters**: `isBdGHam` (`INTEGER(4)`), `chiLR`, `DeltaSCL`, `DeltaSCR` (`REAL(8)`).
- **MPI Variables**: `Nprocs`, `mpiRank`, `mpiSize`, `mpiRoot`, `mpierror` (`INTEGER(4)`), `mpistatus(:)` (`INTEGER(4), ALLOCATABLE`).
- **Green's Function Matrices**: `GRLef`, `GRRgt`, `InvGRd`, `GRd`, `GRu`, `GRl`, `Gnd`, `Gpd`, `Gnu`, `Gpu`, `Gnl`, `Gpl` - Various complex allocatable arrays.
- **Self-Energy/Broadening Matrices**: `GammaL`, `Gammav1`, `GammaR`, `Gammav2`, `SigmaL`, `SigmaR`, `Sigmav1`, `Sigmav2` - Various complex allocatable arrays.
- **Energy Parameters**: `eStpnum` (`INTEGER`), `energy`, `f1`, `f1v`, `f2`, `f2v`, `eMin`, `eMax`, `Emax0`, `deltaE`, `deltaE0`, `eAbsMin`, `eAbsMax` (`REAL(8)`), `EAxis(:)` (`REAL(8), DIMENSION(:), ALLOCATABLE`).
- **Aharonov-Bohm Parameters**: `abPhi` (`REAL(8)`).
- **Current Densities**: `ContactCurrent(:)`, `Jx(:,:,:)`, `Jy(:,:,:)`, `Jz(:,:,:)` - Real allocatable arrays.
- **Transmission & Density of States**: `T12(:)`, `Modec1(:)`, `Modec2(:)`, `DOSc1(:)`, `DOSc2(:)` - Real allocatable arrays.
- **Electron/Hole Densities**: `EDensity(:)`, `HDensity(:)` - Real allocatable arrays.
- **Energy-Resolved Quantities**: `ERJx`, `ERJy`, `ERJz`, `ERRhon`, `ORJx`, `ORJy`, `ORJz`, `ORRhon` - Real allocatable arrays. `erCD`, `orCD` (`INTEGER`).

### Temporary/Dummy Variables:
- **`eCtr`, `ii`, `jj`, `kk`, `xx`, `yy`, `zz`**: `INTEGER`, Loop counters and indices.
- **`errorFlag`**: `INTEGER`, Flag for errors. Default: `0`.

## Usage Examples

This module is not executed directly. It is used by other program units (subroutines, functions, or main programs) via the `USE NEGF_Module` statement.

```fortran
PROGRAM ExampleProgram
    USE NEGF_Module
    IMPLICIT NONE

    ! Access variables from NEGF_Module
    PRINT *, "Lattice constant a0:", a0
    IF (Npx > 0) THEN
        ! Use Npx in some calculation
    ENDIF

    ! Modify allocatable arrays (after allocation elsewhere)
    ! Htotd(1,1,1) = (0.0D0, 0.0D0)
    ! (This assumes Htotd is allocated in another part of the code, e.g. Read_In_Allocate)
END PROGRAM ExampleProgram
```

## Dependencies and Interactions

- **`USE Constants`**: This module itself uses another module named `Constants`. This implies that `Constants.F90` (or a similar file) must define fundamental constants used herein.
- **Global Data Provider**: `NEGF_Module` serves as a global data provider for many other files in the NEGF simulation package. Any Fortran file that needs access to these shared simulation parameters, arrays, or control flags will include `USE NEGF_Module`.
- **Initialization**: Variables in this module are typically initialized in a dedicated subroutine (e.g., `Read_In_Allocate` as seen in `NEGF_Main.F90`) and then used and modified by various computational routines throughout the simulation.
