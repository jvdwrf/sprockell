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

## Using sprockell

You can use this Sprockell package in several ways:

1. Run a haskell file that uses sprockell directly
2. Compile a haskell file that uses sprockell, and then run the compiled Sprockell program
3. Add a binary to the Sprockell package, and instruct Stack to run that
4. Create a separate Stack project for your Haskell file, which depends on this Sprockell package

You can also find some practical usage of this in the [continuous integration](./runDemos.sh).

### Run directly

```bash
# Ensure sprockell libs are built
$ stack build
...
$ stack runhaskell demos/DemoMandelbrot.hs
...mandelbrot...
```

### Compile a haskell file

```bash
# Ensure sprockell libs are built
$ stack build
...
$ stack ghc demos/DemoMandelbrot.hs
[1 of 1] Compiling Main             ( demos/DemoMandelbrot.hs, demos/DemoMandelbrot.o )
Linking demos/DemoMandelbrot ...
$ ./demos/DemoMandelbrot
...mandelbrot...
```

### Add a binary

See the [Stack documentation] for instructions on how to add executables to projects. You can also generate an example/template project using Stack, and then see how it's done there.

### Create a separate stack project

You could automatically generate a Stack project, containing your output Haskell/Sprockell file, which depends via a git url on this Sprockell package. We again refer you to the [Stack documentation].

[HardwareTypes.hs]: src/Sprockell/HardwareTypes.hs#L115
[DemoFib.hs]: demos/fib/DemoFib.hs
[DemoCharIO.hs]: demos/char-io/DemoCharIO.hs
[DemoMultipleSprockells.hs]: demos/multiple-sprockells/DemoMultipleSprockells.hs
[Stack documentation]: https://docs.haskellstack.org/en/stable/README/
