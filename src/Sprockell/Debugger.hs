module Sprockell.Debugger where
import Sprockell.HardwareTypes

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
debuggerSimplePrint showF = (dbg, ())
    where
        dbg dbgSt sysSt = do
            putStrLn $ showF sysSt
            return (dbgSt,sysSt)

-- | Like 'debuggerSimplePrint',
--   but pauses execution after each time step
--   and waits for you to hit enter to continue.
--   It let you step through the execution slowly.
debuggerSimplePrintAndWait :: (SystemState -> String) -> Debugger ()
debuggerSimplePrintAndWait showF = (dbg, ())
    where
        dbg dbgSt sysSt = do
            putStrLn $ showF sysSt
            getLine -- wait for enter key
            return (dbgSt,sysSt)
