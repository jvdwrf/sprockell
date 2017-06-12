import Sprockell
import Data.Char


{-|
    This program demonstrates the character IO system.

    We ask the users name.
    And print a custom greeting.
-}
prog :: [Instruction]
prog =
    writeString "What is your name? " ++
    [ Load (ImmValue $ ord '\n') regE   -- ASCII code newline in regE for later reference

    -- "beginInputLoop": 39
    , ReadInstr charIO                  -- Request a character from stdin
    , Receive regA                      -- Save it in regA (as ASCII code)
    , Branch regA (Rel 2)
    , Jump (Rel (-3))                   -- got 0, no input available, try again

    -- got input char
    , Compute Equal regA regE regC      -- check if it's a newline (remember: in regE)
    , Branch regC (Abs 48)              -- then jump to "inputDone"
    , Store regA (IndAddr regB)         -- else store character in local memory
    , Compute Incr regB regB regB
    , Jump (Abs 39)                     -- "beginInputLoop"
    ]
    -- "inputDone": 48
    ++ writeString "Hello "
    ++
    -- "beginLoopOutput"
    [ Load (IndAddr regD) regA
    , WriteInstr regA charIO
    , Compute Incr regD regD regD
    , Compute NEq regB regD regC
    , Branch regC (Rel (-4))            -- target "loopOutput"
    ]
    ++ writeString "!\n"
    ++ [EndProg]


-- | Generate code to print a (Haskell) String
writeString :: String -> [Instruction]
writeString str = concat [writeChar c | c <- str]

-- | Generate code to print a single character
writeChar :: Char -> [Instruction]
writeChar c =
    [ Load (ImmValue $ ord c) regA
    , WriteInstr regA charIO
    ]


main = run [prog]
