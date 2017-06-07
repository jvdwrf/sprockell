module Sprockell.Simulation where

import Control.DeepSeq
import Sprockell.BasicFunctions
import Sprockell.HardwareTypes
import Sprockell.Sprockell
import Sprockell.System
import Sprockell.Debugger

import System.IO         (BufferMode(..),stdin,hGetBuffering,hSetBuffering)
import Control.Exception (bracket)

-- ====================================================================================================
-- Sprockell Test
-- ====================================================================================================

sprockellSim :: [Instruction]
                -> SprockellState
                -> [Reply]
                -> [(Instruction, SprockellState, Request)]

sprockellSim instrs s []     = []
sprockellSim instrs s (i:is) | instr /= EndProg    = (instr,s',o) : sprockellSim instrs s' is
                             | otherwise           = []
                where
                  (s',o) = sprockell instrs s i
                  instr  = instrs ! pc s

localMemSize = 16 :: Int
regbankSize  = 8  :: Int

initSprockellState :: Value -> SprockellState
initSprockellState sprID = SprState
        { pc       = 0
        , sp       = localMemSize
        , regbank  = replicate regbankSize 0 <~ (regSprID,sprID)
        , localMem = replicate localMemSize 0
        }

sprTest :: Value -> [Instruction] -> [Reply] -> IO ()
sprTest sprID instrs input = putStr
                           $ unlines
                           $ map show
                           $ sprockellSim instrs (initSprockellState sprID) input

-- ====================================================================================================
-- System Test
-- ====================================================================================================
data Tick  = Tick        deriving (Eq,Show)
type Clock = [Tick]
clock = repeat Tick

systemSim :: [[Instruction]] -> SystemState -> Clock -> [([Instruction],SystemState)]
systemSim instrss s []     = []
systemSim instrss s (t:ts) | not sysHalted = deepseq s $ (instrs,s') : systemSim instrss s' ts
                           | otherwise     = []
                where
                  instrs    = zipWith (!) instrss (map pc $ sprStates s)
                  s'        = system instrss s t
                  sysHalted = and $ map (==EndProg) $ zipWith (!!) instrss $ map pc $ sprStates s

systemSimIO :: Debugger st -> [[Instruction]] -> SystemState -> Clock -> IO ()
systemSimIO (dbg,dbgSt) instrss s []     = return ()
systemSimIO (dbg,dbgSt) instrss s (t:ts) | sysHalted = return ()
                                         | otherwise = do
                                             s' <- deepseq s $ systemIO instrss s t
                                             (dbgSt',s'') <- dbg dbgSt s'
                                             systemSimIO (dbg,dbgSt') instrss s'' ts
                where
                  instrs    = zipWith (!) instrss (map pc $ sprStates s)
                  sysHalted = (and $ map (==EndProg) $ zipWith (!!) instrss $ map pc $ sprStates s)
                              && (and $ map and $ map (map (==NoRequest)) $ requestChnls s)
                              && (and $ map (\(_,r) -> r == NoRequest) $ requestFifo s)


shMemSize       = 8 :: Int
channelDelay    = 4 :: Int

initSystemState nrOfSprockells = SystemState
        { sprStates     = map initSprockellState [0 .. nrOfSprockells-1]
        , requestChnls  = replicate nrOfSprockells $ replicate channelDelay NoRequest
        , replyChnls    = replicate nrOfSprockells $ replicate channelDelay Nothing
        , requestFifo   = []
        , sharedMem     = replicate shMemSize 0
        }

myShow (instrs,s) = show instrs ++ "\n" ++
                    (unlines $ map show $ sprStates s) ++
                    show (requestChnls s) ++ "\n" ++
                    show (replyChnls s) ++"\n" ++
                    show (requestFifo s) ++ "\n" ++
                    show (sharedMem s)


sysTest :: [[Instruction]] -> IO ()                             -- instrss: list of instructions per Sprockell
sysTest instrss = putStr                                        -- putStr: standard Haskell IO-function
                $ unlines
                $ map (++"\n")
                $ map myShow                                    -- make your own show-function?
                $ systemSim instrss (initSystemState nrOfSprockells) clock
    where nrOfSprockells = length instrss

sysTestIO :: Debugger st -> [[Instruction]] -> IO ()                             -- instrss: list of instructions per Sprockell
sysTestIO dbg instrss = do
    bracket setupBuffering
            restoreBuffering
            (\_ -> systemSimIO dbg instrss (initSystemState nrOfSprockells) clock)
    return ()
    where nrOfSprockells = length instrss

setupBuffering :: IO BufferMode
setupBuffering = do
    oldbuffering <- hGetBuffering stdin
    hSetBuffering stdin  LineBuffering  -- needed to make line editing work for numberIO mode in ghci
    --hSetBuffering stdout LineBuffering
    return oldbuffering

restoreBuffering :: BufferMode -> IO ()
restoreBuffering = hSetBuffering stdin
