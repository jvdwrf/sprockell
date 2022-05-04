#!/usr/bin/env bash

stack --version

echo 10 | stack runhaskell demos/DemoFib.hs

echo "Alice" | stack runhaskell demos/DemoCharIO.hs

stack runhaskell demos/DemoMandelbrot.hs

yes "\n" | stack runhaskell demos/DemoMultipleSprockells.hs
