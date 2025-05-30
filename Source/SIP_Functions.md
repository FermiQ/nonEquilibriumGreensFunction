# Documentation for Source/SIP_Functions.F90

## Overview

This file provides several utility functions primarily related to semiconductor physics, specifically for calculating Fermi-Dirac integrals and solving transcendental equations to find intrinsic levels or potentials. These functions are likely used in the setup or boundary condition calculations for the Poisson solver.

## Key Components

- **`FUNCTION Fermi(X)`**:
    - Calculates an approximation to the Fermi-Dirac integral of order 1/2, F<sub>1/2</sub>(X).
    - `X` is the argument (E-E<sub>F</sub>)/kT.
    - Uses an empirical approximation formula involving `rnu` and `zeta`.
    - Returns a `REAL(8)` value.

- **`FUNCTION Ec_Ei(rn, rv, Eg)`**:
    - Calculates the position of the intrinsic Fermi level (E<sub>i</sub>) relative to the conduction band edge (E<sub>c</sub>), i.e., E<sub>c</sub> - E<sub>i</sub>.
    - Inputs:
        - `rn`: Effective density of states in the conduction band (N<sub>c</sub>) (or related to it).
        - `rv`: Effective density of states in the valence band (N<sub>v</sub>) (or related to it).
        - `Eg`: Bandgap energy.
    - Solves the charge neutrality equation `rn * Fermi(-x) = rv * Fermi(x - Eg)` for `x = (Ec - Ei) / kT` using a bisection-like root-finding method.
    - `Fermi` is declared as `EXTERNAL`, indicating it's the function defined above.
    - The search starts from `x = -Eg` and iteratively expands the search interval (`ddx`) until a sign change in `fx(k-1)*fx(k)` is found, then refines the root.
    - Contains a `STOP` condition if the internal array `FX` is too small.
    - Returns `xMid` which represents (E<sub>c</sub> - E<sub>i</sub>) / kT.

- **`FUNCTION VOLTAGE(C, Ncnorm, Nvnorm, Eg, dEc)`**:
    - Calculates the electrostatic potential (Voltage, likely V/kT or similar normalized potential) that satisfies a charge neutrality condition.
    - Inputs:
        - `C`: A constant related to doping or fixed charge.
        - `Ncnorm`: Normalized effective density of states in conduction band.
        - `Nvnorm`: Normalized effective density of states in valence band.
        - `Eg`: Bandgap energy (normalized).
        - `dEc`: Conduction band offset (normalized).
    - Solves an equation of the form `Ncnorm*Fermi(x-dEc) - Nvnorm*Fermi(dEc-Eg-x) + C = 0` for `x` (the voltage/potential).
    - Similar to `Ec_Ei`, it uses a bisection-like root-finding method.
    - `Fermi` is declared `EXTERNAL`.
    - The comment `! called from init_potential -> "C" is at a given node` suggests its use in initializing potential profiles.
    - Contains a `STOP` condition if `FX` array is too small.
    - Returns `xMid`, the calculated voltage.

## Important Variables/Constants

- **`pi`**: `REAL(8)`, defined locally in `Fermi` function as `4D0*atan(1D0)`.
- **Function Arguments**: As described for each function.
- **Local Variables in `Ec_Ei` and `VOLTAGE`**:
    - `x`, `x1`, `x2`: Variables for tracking search interval.
    - `f1`, `f2`, `fmid`: Function values at different points.
    - `rtbis`: Stores the current best estimate of the root.
    - `k`: Loop counter.
    - `I$FLAG`: Logical flag to control loop termination.
    - `FX(0:200)` or `FX(0:100)`: Array to store function values during root search.
    - `ddx`: Step size for searching.
    - `xMid`: Midpoint of the interval / refined root.

## Usage Examples

These functions are likely called from other parts of the SIP (Poisson solver) codebase, particularly during initialization or when setting boundary conditions.

```fortran
MODULE SomeOtherModule
    USE SIP_Module ! For context, though not directly used by these functions
    IMPLICIT NONE
    REAL(8), EXTERNAL :: Fermi, Ec_Ei, VOLTAGE

    SUBROUTINE TestFunctions
        REAL(8) :: fermi_val, arg_x, ec_minus_ei, potential
        REAL(8) :: Nc_eff, Nv_eff, bandgap, C_charge, Ec_offset

        arg_x = 0.5D0
        fermi_val = Fermi(arg_x)
        PRINT *, "Fermi(0.5) =", fermi_val

        Nc_eff = 2.5e19
        Nv_eff = 1.0e19
        bandgap = 1.12 ! eV (example, ensure units are consistent with Fermi function expectation)
        ec_minus_ei = Ec_Ei(Nc_eff, Nv_eff, bandgap)
        PRINT *, "Ec - Ei (normalized) =", ec_minus_ei

        C_charge = 0.0
        Ec_offset = 0.0
        potential = VOLTAGE(C_charge, Nc_eff, Nv_eff, bandgap, Ec_offset)
        PRINT *, "Calculated Potential (normalized) =", potential
    END SUBROUTINE TestFunctions

END MODULE SomeOtherModule
```
*(Note: The arguments to `Ec_Ei` and `VOLTAGE` might be normalized by kT/q or kT depending on how the `Fermi` function argument `X` is defined in the broader context of the Poisson solver.)*

## Dependencies and Interactions

- **Self-Contained (mostly)**: The functions themselves don't `USE` any custom modules from the project directly (like `SIP_Module` or `NEGF_Module`).
- **`EXTERNAL Fermi`**: `Ec_Ei` and `VOLTAGE` declare `Fermi` as `EXTERNAL`, meaning they rely on the `Fermi` function also present in this file or linked externally.
- **Mathematical Nature**: These are mathematical utility functions. Their correctness is crucial for the physics implemented in the Poisson solver.
- **Error Handling**: `Ec_Ei` and `VOLTAGE` include basic error handling that `STOP`s the program if an internal array `FX` used for root finding is too small, indicating the search range became too large.
