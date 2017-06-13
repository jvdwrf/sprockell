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
The instructions are documented in [HardwareTypes.hs](https://github.com/leonschoorl/sprockell/blob/sprockell-2017/src/Sprockell/HardwareTypes.hs#L95).

There are a number of demo programs showing various features.
* [DemoFib.hs](https://github.com/leonschoorl/sprockell/blob/sprockell-2017/src/DemoFib.hs)
 show the IO system for numeric value
* [DemoCharIO.hs](https://github.com/leonschoorl/sprockell/blob/sprockell-2017/src/DemoCharIO.hs)
 shows the IO system for characters and strings
* [DemoMultipleSprockells.hs](https://github.com/leonschoorl/sprockell/blob/sprockell-2017/src/DemoMultipleSprockells.hs)
 shows communication between multiple Sprockell cores using the shared memory
