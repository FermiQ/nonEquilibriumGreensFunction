# Documentation for Source/MPI_Routines.F90

## Overview

This file contains subroutines for managing parallel execution using the Message Passing Interface (MPI). These routines handle MPI initialization, broadcasting data from the root process to other processes, gathering and reducing data from all processes to the root process, and error checking for MPI calls. These are essential for distributing the computational workload (primarily the energy sweep) across multiple processors or nodes in a cluster environment.

## Key Components

- **`SUBROUTINE MPI_Initialize`**:
    - Initializes the MPI environment if `Nprocs > 1`.
    - Calls `MPI_Init` to start MPI.
    - Calls `MPI_Comm_size` to get the total number of MPI processes (`mpisize`).
    - Calls `MPI_Comm_Rank` to get the rank of the current process (`mpirank`).
    - Allocates `mpistatus` array (used for MPI status objects).
    - If `Nprocs == 1`, sets `mpisize = 1` and `mpirank = 0` for serial execution.
    - Sets `mpiroot = 0`.

- **`SUBROUTINE MPI_Broadcast`**:
    - Broadcasts essential data from the `mpiroot` process to all other MPI processes if `Nprocs > 1`.
    - Uses `MPI_BARRIER` for synchronization before broadcasting.
    - Broadcasts `Htotd` (total Hamiltonian) and `HopX` (hopping matrix in x-direction).
    - Uses `MPI_BCAST` with `MPI_COMPLEX16` datatype.

- **`SUBROUTINE MPI_Gather_Iteration`**:
    - Gathers and reduces data from all MPI processes to the `mpiroot` process during each self-consistent iteration if `Nprocs > 1`.
    - Uses `MPI_BARRIER` for synchronization.
    - Reduces `RhoN` (electron density) and `RhoP` (hole density) using `MPI_REDUCE` with `MPI_SUM` operation. Temporary arrays (`rmat3`) are used for the send buffer.

- **`SUBROUTINE MPI_Gather_Final`**:
    - Gathers and reduces a comprehensive set of final observable data from all MPI processes to the `mpiroot` process at the end of the simulation if `Nprocs > 1`.
    - Reduces arrays like `EDensity`, `HDensity`, `T12`, `Modec1`, `Modec2`, `DOSc1`, `DOSc2`, `contactCurrent`, `Jx`, `Jy`, `Jz`.
    - Conditionally reduces energy-resolved (`ERJx`, etc.), orbital-resolved (`ORJx`, etc.), and spin expectation (`SigXExp`, etc.) arrays if their respective calculation flags (`ERcd`, `ORcd`, `calcSigExp`) are set.
    - Uses temporary allocatable arrays (`rmat1`, `rmat3`, `rmat4`) as send buffers for `MPI_REDUCE`.
    - Calls `MPI_BARRIER` for synchronization.
    - Calls `MPI_FINALIZE` to terminate the MPI environment.
    - Non-root processes call `STOP` after finalization.

- **`SUBROUTINE MPI_ErrorCheck(mpierror, condition)`**:
    - A utility subroutine to check the return status (`mpierror`) of MPI calls.
    - If `mpierror` is non-zero, it prints an error message indicating which subroutine (based on `condition` code) encountered the error and the error code itself.

## Important Variables/Constants

- **From `NEGF_Module`:**
    - `Nprocs`: Number of processors requested (input, determines if MPI is actively used).
    - `mpisize`, `mpirank`, `mpiroot`: Standard MPI size, rank, and root process ID.
    - `mpierror`: Integer for MPI error codes.
    - `mpistatus`: Array for MPI status.
    - `Htotd`, `HopX`: Hamiltonian matrices broadcasted.
    - `RhoN`, `RhoP`: Density matrices gathered per iteration.
    - All observable arrays (e.g., `EDensity`, `T12`, `Jx`, `ERJx`, `SigXExp`) are gathered in `MPI_Gather_Final`.
    - Flags: `ERcd`, `ORcd`, `calcSigExp`.
    - Dimensions: `NNz`, `Npx`, `Npy`, `Npz`, `eStpnum`, `Norb`.
- **MPI Specifics:**
    - `USE MPI`: Imports the MPI library definitions.
    - `MPI_COMM_WORLD`: Standard MPI communicator.
    - `MPI_COMPLEX16`, `MPI_DOUBLE_PRECISION`, `MPI_SUM`: MPI datatypes and reduction operation.
    - `MPI_STATUS_SIZE`: MPI constant for status object size.
- **Local Variables:**
    - `rmat1`, `rmat2`, `rmat3`, `rmat4`: Allocatable real arrays used as temporary send buffers in gather operations.
    - `cmat*`: Allocatable complex arrays declared but not used in the provided snippet of `MPI_Gather_Iteration` and `MPI_Gather_Final`.
    - `condition`: Integer input to `MPI_ErrorCheck` to identify the calling context.

## Usage Examples

These routines are called at different stages of the main program (`NEGF_Main.F90`):

```fortran
! In NEGF_Main.F90
PROGRAM NEGF_Main
    USE NEGF_Module ! Contains Nprocs
    USE MPI         ! May not be strictly needed here if MPI_Routines handles all MPI USEs
    IMPLICIT NONE

    ! Nprocs is typically read from input first (e.g. in Read_In_Allocate)
    ! Then MPI is initialized:
    CALL MPI_Initialize

    ! ... (Initial setup on root process, e.g., Read_In_Allocate) ...

    DO ite = 1, iteMax
        ! ... (Build_Hamiltonian on root process) ...

        CALL MPI_Broadcast ! Broadcast Htotd, HopX

        ! Energy sweep loop (parallelized)
        DO eCtr = 0, eStpnum-1
            IF ((mpiSize == 1) .OR. (mod(eCtr, mpiSize) == mpiRank)) THEN
                ! ... calculations for this energy slice ...
            ENDIF
        ENDDO

        CALL MPI_Gather_Iteration ! Gather RhoN, RhoP

        IF (ite==iteMax .OR. poissonIsConverged==1) THEN
            CALL MPI_Gather_Final ! Gather all observables and finalize MPI
            IF (mpiRank==mpiRoot) THEN
                CALL Dump_Data
            ENDIF
            ! Non-root processes would have STOPped inside MPI_Gather_Final
        ENDIF
    ENDDO
END PROGRAM NEGF_Main
```

## Dependencies and Interactions

- **`USE NEGF_Module`**: Accesses shared simulation variables and MPI control parameters.
- **`USE MPI`**: Directly uses the MPI Fortran library. This implies the code must be compiled and linked with an MPI library.
- **`MPI_ErrorCheck`**: Called after most MPI operations to report errors.
- **Control Flow**:
    - `MPI_Initialize` is called early.
    - `MPI_Broadcast` is called after data is ready on the root process to be sent to others.
    - `MPI_Gather_Iteration` is called within the self-consistent loop to sum up densities.
    - `MPI_Gather_Final` is called at the very end to collect all results and then terminate MPI for all processes. Non-root processes `STOP` within `MPI_Gather_Final`.
- **Comparison with `MKL_Routines_PC.F90`**: `MKL_Routines_PC.F90` contains dummy MPI routines. The build system would typically link against either this file (for real MPI) or `MKL_Routines_PC.F90` (for serial execution with dummy MPI calls) but not both.
