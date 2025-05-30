# Documentation for Source/Constants.F90

## Overview

The `Constants` module defines a set of fundamental physical and mathematical constants that are used throughout the NEGF simulation project. These constants are declared as `PARAMETER`s, meaning their values are fixed at compile time. Using a dedicated module for constants promotes consistency and makes it easier to manage these values.

## Key Components

- **`MODULE Constants`**: The module itself, which encapsulates all the defined constants.

## Important Variables/Constants

All entities defined in this module are constants.
- **`ci`**: `COMPLEX(8), PARAMETER`
  - Description: The imaginary unit, i.e., sqrt(-1).
  - Value: `cmplx(0D0, 1D0, 8)`
- **`c0`**: `COMPLEX(8), PARAMETER`
  - Description: Complex zero, i.e., (0.0, 0.0).
  - Value: `cmplx(0D0, 0D0, 8)`
- **`c1`**: `COMPLEX(8), PARAMETER`
  - Description: Complex one, i.e., (1.0, 0.0).
  - Value: `cmplx(1D0, 0D0, 8)`
- **`q`**: `REAL(8), PARAMETER`
  - Description: Elementary charge in Coulombs.
  - Value: `1.602D-19`
- **`m0`**: `REAL(8), PARAMETER`
  - Description: Electron rest mass in kilograms.
  - Value: `9.1019D-31`
- **`pi`**: `REAL(8), PARAMETER`
  - Description: The mathematical constant pi.
  - Value: `4D0*atan(1D0)`
- **`eps0`**: `REAL(8), PARAMETER`
  - Description: Vacuum permittivity in Farads per meter.
  - Value: `8.85418782D-12`
- **`kB`**: `REAL(8), PARAMETER`
  - Description: Boltzmann constant in Joules per Kelvin.
  - Value: `1.38066D-23`
- **`h`**: `REAL(8), PARAMETER`
  - Description: Planck constant in Joule-seconds.
  - Value: `6.626D-34`
- **`hbar`**: `REAL(8), PARAMETER`
  - Description: Reduced Planck constant (h / 2*pi).
  - Value: `h/(2D0*pi)`
- **`eta_sigma`**: `REAL(8), PARAMETER`
  - Description: A small positive infinitesimal value, often used to add a small imaginary part to energy for numerical stability in Green's function calculations or as a broadening factor.
  - Value: `1D-4`

## Usage Examples

Other Fortran modules, subroutines, or functions can access these constants by including the `USE Constants` statement.

```fortran
MODULE AnotherModule
    USE Constants
    IMPLICIT NONE

    REAL(8) :: energy_eV
    REAL(8) :: energy_Joules

    ! Example of using a constant
    energy_Joules = energy_eV * q
    PRINT *, "Pi value: ", pi
END MODULE AnotherModule

SUBROUTINE SomeCalculation
    USE Constants
    IMPLICIT NONE
    COMPLEX(8) :: z_value

    z_value = (0.5D0, 0.0D0) * ci ! z_value becomes (0.0, 0.5)
    IF (abs(z_value - c0) < eta_sigma) THEN
        PRINT *, "z_value is close to zero."
    ENDIF
END SUBROUTINE SomeCalculation
```

## Dependencies and Interactions

- **Self-contained**: This module generally has no dependencies on other custom modules within the project.
- **Provider of Constants**: It serves as a provider of fundamental constants to any other part of the codebase that requires them. For instance, `NEGF_Module` uses `Constants`.
