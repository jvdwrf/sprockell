# Sprockell
Sprockell is a **S**imple **Proc**essor in Has**kell**. It was originally written by Jan Kuper at the University of Twente. It has later been extended to allow multiple Sprockells to be run at once, communicating via shared memory.

# Features
* Simple arithmetic
* Branches / jumps
* Stack
* Local memory
* Shared memory
* IO system for numbers and string
* customizable debugger

# Documentation
The instructions are documented in [HardwareTypes.hs].

## Examples
There are a number of demo programs showing various features.
* [DemoFib.hs] shows the IO system for numeric value
* [DemoCharIO.hs] shows the IO system for characters and strings.
  It also demonstrates the use of the debugger.
* [DemoMultipleSprockells.hs]
 shows communication between multiple Sprockell cores using the shared memory

[HardwareTypes.hs]: src/Sprockell/HardwareTypes.hs#L115
[DemoFib.hs]: src/DemoFib.hs
[DemoCharIO.hs]: src/DemoCharIO.hs
[DemoMultipleSprockells.hs]: src/DemoMultipleSprockells.hs
