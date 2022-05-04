stack --version

stack build

echo 10 | stack runhaskell demos/DemoFib.hs

echo "Alice" | stack runhaskell demos/DemoCharIO.hs

stack runhaskell demos/DemoMandelbrot.hs

stack runhaskell demos/DemoMultipleSprockells.hs
