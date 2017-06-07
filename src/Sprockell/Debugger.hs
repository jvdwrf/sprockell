module Sprockell.Debugger where
import Sprockell.HardwareTypes
import Control.Monad (when, void)

-- | A DebuggerF represents a debugger for the Sprockell system
--
-- It's a function from a debugger state and a 'SystemState'
-- to an IO action returning an potentially updated debugger state and SystemState.
type DebuggerF st = st -> SystemState -> IO (st, SystemState)

-- |  A Debugger is a combination of a 'DebuggerF' and it's initial state.
type Debugger  st = (DebuggerF st, st)



-- | No debugging at all, just run the system.
noDebugger :: Debugger ()
noDebugger = (noDebuggerF,())
    where
        noDebuggerF st sys = return (st,sys)

-- | Given a show function for the SystemState,
--   this creates a 'Debugger' that prints the 'SystemState'
--   at each time step using the supplied show function.
debuggerSimplePrint :: (SystemState -> String) -> Debugger ()
debuggerSimplePrint showF = debuggerPrintCondWaitCond showF (const True) (const False)

-- | Like 'debuggerSimplePrint',
--   but pauses execution after each time step
--   and waits for you to hit enter to continue.
--   It let you step through the execution slowly.
debuggerSimplePrintAndWait :: (SystemState -> String) -> Debugger ()
debuggerSimplePrintAndWait showF = debuggerPrintCondWaitCond showF (const True) (const True)


debuggerPrintCondWaitCond :: (SystemState -> String) -- ^ show function used to show (parts of) the state
                          -> (SystemState -> Bool)   -- ^ when this condition is True the state is printed
                          -> (SystemState -> Bool)   -- ^ when this condition is True execution pauses
                          -> Debugger ()
debuggerPrintCondWaitCond showF showCond waitCond = (dbg, ())
    where
        dbg dbgSt sysSt = do
            when (showCond sysSt) (putStrLn $ showF sysSt)
            when (waitCond sysSt) (void getLine) -- wait for enter key
            return (dbgSt,sysSt)
