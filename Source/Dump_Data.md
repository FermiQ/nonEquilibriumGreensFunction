# Documentation for Source/Dump_Data.F90

## Overview

The `Dump_Data` subroutine is responsible for writing simulation parameters and results to various output files. It is typically called at the end of a successful simulation run. Data is organized into several files: `scalars.txt` for input parameters and simulation setup, `Greens.txt` for density matrices, `Transmissions.txt` for energy-dependent observables and currents, and conditionally `Poissons.txt`, `ERCD.txt` (Energy Resolved Current/Density), `ORCD.txt` (Orbital Resolved Current/Density), and `SigExp.txt` (Spin Expectation values).

## Key Components

- **`SUBROUTINE Dump_Data`**:
    - Opens and writes to `scalars.txt`:
        - Grid dimensions (`Npx`, `Npy`, `Npz`, `Norb`).
        - Chemical potential (`muT`), energy range (`eMin`, `eMax`), number of energy steps (`eStpnum`), energy step size (`deltaE`).
        - Contact chemical potentials (`mu(1)`, `mu(2)`).
        - `channelType`, and `a0` if `channelType == 1`.
        - Aharonov-Bohm phase (`abPhi`), correlation length (`corrLength`), disorder strength (`disorderStrength`).
    - Opens and writes to `Greens.txt`:
        - Spatially resolved electron density `RhoN(xx,yy,zz)`.
        - Spatially resolved hole density `RhoP(xx,yy,zz)`.
        - Spatially resolved disorder potential `DeltaR(xx,yy,zz)`.
    - Opens and writes to `Transmissions.txt`:
        - Electron and hole densities vs. energy (`EDensity`, `HDensity`).
        - Transmission `T12` vs. energy.
        - Contact mode densities `Modec1`, `Modec2` vs. energy.
        - Energy axis values `EAxis`.
        - Total contact currents `ContactCurrent`.
        - Spatially resolved current densities `Jx`, `Jy`, `Jz`.
        - Contact density of states `DOSc1`, `DOSc2` vs. energy.
    - (Conditional) Opens and writes to `Poissons.txt` if `includePoisson == 1`:
        - Electrostatic potential `Phi`.
        - Charge density `ro` used in Poisson solver.
    - (Conditional) Opens and writes to `ERCD.txt` if `erCD == 1`:
        - Energy-resolved current densities `ERJx`, `ERJy`, `ERJz`.
        - Energy-resolved charge density `ERRhon`.
    - (Conditional) Opens and writes to `ORCD.txt` if `orCD == 1`:
        - Orbital-resolved current densities `ORJx`, `ORJy`, `ORJz`.
        - Orbital-resolved charge density `ORRhon`.
    - (Conditional) Opens and writes to `SigExp.txt` if `calcSigExp == 1`:
        - Spatially resolved spin expectation values `SigXExp`, `SigYExp`, `SigZExp`.
    - Prints "COMPLETE: Data dumped to directory [outputfiledir]" to standard output.
    - Calls `STOP "Run Complete!"` to terminate the program.

## Important Variables/Constants

This subroutine uses variables primarily from `SIP_Module` (which likely includes `NEGF_Module`).
- **Input Data (Read from `SIP_Module`/`NEGF_Module`):**
    - `outputfiledir`: Character string for the output directory path.
    - All simulation parameters and result arrays mentioned in the "Key Components" section (e.g., `Npx`, `RhoN`, `T12`, `Phi`, `ERJx`, `SigXExp`, etc.).
    - Flags: `includePoisson`, `erCD`, `orCD`, `calcSigExp`.
- **File Unit Number:**
    - `UNIT=13` is consistently used for all file operations.
- **Format Statement:**
    - `43 format(ES11.4)`: Used for writing most real numbers in scientific notation.

## Usage Examples

This subroutine is typically the final step in the main program execution.

```fortran
! In NEGF_Main.F90 or a similar main program
USE SIP_Module
IMPLICIT NONE

! ... (entire simulation completes) ...

IF (mpiRank == mpiRoot) THEN ! Usually only the root process dumps data
    IF (ite==iteMax .OR. poissonIsConverged==1) THEN
        CALL MPI_Gather_Final ! Ensure all data is on root before dumping
        CALL Dump_Data
    ENDIF
ENDIF

! The program will STOP inside Dump_Data
```

## Dependencies and Interactions

- **`USE SIP_Module`**: Accesses a wide range of simulation parameters and results stored in modules (likely `NEGF_Module` and `SIP_Module` itself).
- **File System Interaction**: Creates multiple files in the directory specified by `outputfiledir`.
- **Program Termination**: This subroutine is designed to be the final action of the simulation, as it calls `STOP`.
- **MPI Context**: While not explicitly shown handling MPI ranks for file I/O in this snippet, in an MPI environment, data dumping is typically done only by the root process after gathering results from all other processes. The call to `Dump_Data` in `NEGF_Main.F90` is indeed guarded by `IF (mpiRank==mpiRoot)`.
