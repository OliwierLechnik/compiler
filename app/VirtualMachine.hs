-- Oliwier Lechnik 279760

-- Kod maszyny wirtualnej zainspirowany kodem Prof. Macieja Gębali

module VirtualMachine where

import VMInterface
import Control.Monad.State
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array (Array)
import qualified Data.Array as Array

data VM = VM
    { input               :: [Integer]
    , output              :: [Integer]
    , registers           :: Map Register Integer
    , memory              :: Map Address Integer
    , instructions        :: Array Integer VMCommand
    , instruction_pointer :: Integer
    , time_elapsed        :: Integer
    , history             :: [(VMCommand, Integer)]
    }
    deriving Show

incIP :: VM -> VM -- increment  instruction pointer
incIP st = st { instruction_pointer = instruction_pointer st + 1 }

addTime :: VMCommand -> State VM ()
addTime cmd = do 
    time <- gets time_elapsed
    modify $ \st -> st { time_elapsed = vmCmdCost cmd + time }

switchCommand :: VMCommand -> State VM ()
switchCommand cmd = case cmd of

    WRITE -> do
        ra  <- gets (Map.findWithDefault 0 RegA . registers)
        modify $ \st -> incIP st
            { output = ra : output st }

    READ -> do
        x  <- gets (head . input)
        xs <- gets (tail . input)
        modify $ \st -> incIP st
            { registers = Map.insert RegA x (registers st)
            , input = xs
            }

    LOAD addr -> do
        val <- gets (Map.findWithDefault 0 addr . memory)
        modify $ \st -> incIP st
            { registers = Map.insert RegA val (registers st)
            }

    RLOAD reg -> do
        regs <- gets registers
        mem  <- gets memory
        let addr = Map.findWithDefault 0 reg regs
            val  = Map.findWithDefault 0 addr mem
        modify $ \st -> incIP st
            { registers = Map.insert RegA val regs
            }

    STORE addr -> do
        ra  <- gets (Map.findWithDefault 0 RegA . registers)
        mem <- gets memory
        modify $ \st -> incIP st
            { memory = Map.insert addr ra mem
            }

    RSTORE reg -> do
        regs <- gets registers
        mem  <- gets memory
        let addr = Map.findWithDefault 0 reg regs
            ra   = Map.findWithDefault 0 RegA regs
        modify $ \st -> incIP st
            { memory = Map.insert addr ra mem
            }

    ADD reg -> do
        regs <- gets registers
        let rx = Map.findWithDefault 0 reg regs
            ra = Map.findWithDefault 0 RegA regs
        modify $ \st -> incIP st
            { registers = Map.insert RegA (ra + rx) regs
            }

    SUB reg -> do
        regs <- gets registers
        let rx = Map.findWithDefault 0 reg regs
            ra = Map.findWithDefault 0 RegA regs
        modify $ \st -> incIP st
            { registers = Map.insert RegA (max (ra - rx) 0) regs
            }

    SWP reg -> do
        regs <- gets registers
        let rx = Map.findWithDefault 0 reg regs
            ra = Map.findWithDefault 0 RegA regs
            regs'  = Map.insert RegA rx regs
            regs'' = Map.insert reg ra regs'
        modify $ \st -> incIP st
            { registers = regs''
            }

    RST reg -> do
        regs <- gets registers
        modify $ \st -> incIP st
            { registers = Map.insert reg 0 regs
            }

    INC reg -> do
        regs <- gets registers
        let rx = Map.findWithDefault 0 reg regs
        modify $ \st -> incIP st
            { registers = Map.insert reg (rx + 1) regs
            }

    DEC reg -> do
        regs <- gets registers
        let rx = Map.findWithDefault 0 reg regs
        modify $ \st -> incIP st
            { registers = Map.insert reg (max (rx - 1) 0) regs
            }

    SHL reg -> do
        regs <- gets registers
        let rx = Map.findWithDefault 0 reg regs
        modify $ \st -> incIP st
            { registers = Map.insert reg (2 * rx) regs
            }

    SHR reg -> do
        regs <- gets registers
        let rx = Map.findWithDefault 0 reg regs
        modify $ \st -> incIP st
            { registers = Map.insert reg (rx `div` 2) regs
            }

    JUMP addr ->
        modify $ \st -> st { instruction_pointer = addr }

    JPOS addr -> do
        ra <- gets (Map.findWithDefault 0 RegA . registers)
        if ra > 0
            then modify $ \st -> st { instruction_pointer = addr }
            else modify incIP

    JZERO addr -> do
        ra <- gets (Map.findWithDefault 0 RegA . registers)
        if ra == 0
            then modify $ \st -> st { instruction_pointer = addr }
            else modify incIP

    CALL addr -> do
        ip   <- gets instruction_pointer
        regs <- gets registers
        modify $ \st -> st
            { instruction_pointer = addr
            , registers = Map.insert RegA (ip + 1) regs
            }

    RTRN -> do
        ra <- gets (Map.findWithDefault 0 RegA . registers)
        modify $ \st -> st { instruction_pointer = ra }

evalCmd :: VMCommand -> State VM ()
evalCmd cmd = do
    addTime cmd
    switchCommand cmd
    modify $ \st -> st
        { history = (cmd, time_elapsed st) : history st
        }

run :: State VM ()
run = do
    ip  <- gets instruction_pointer
    cmd <- gets ((Array.! ip) . instructions)
    case cmd of
        HALT -> pure ()
        _    -> do
            evalCmd cmd
            run

newRegisters :: Map Register Integer
newRegisters = Map.fromList
    [ (RegA, 0)
    , (RegB, 0)
    , (RegC, 0)
    , (RegD, 0)
    , (RegE, 0)
    , (RegF, 0)
    , (RegG, 0)
    , (RegH, 0)
    ]

testProgram1 :: [VMCommand] -- builds 975
testProgram1 = [INC RegA,SHL RegA,SHL RegA,SHL RegA,SHL RegA,DEC RegA,SHL RegA,SHL RegA,INC RegA,SHL RegA,SHL RegA,SHL RegA,SHL RegA,DEC RegA, WRITE, HALT]

testProgram2 :: [VMCommand] -- prints provided number in binary
testProgram2 = [READ, SWP RegB, RST RegA, ADD RegB, SHR RegB, SHL RegB, SUB RegB, WRITE, SHR RegB, RST RegA, ADD RegB, JPOS 4, HALT]

testProgram3 :: [VMCommand] -- prints provided number in binary (unoptimised)
testProgram3 = [READ, STORE 0, LOAD 0, SHR RegA, STORE 1, LOAD 1, SHL RegA, STORE 1, LOAD 1, SWP RegB, LOAD 0, SUB RegB, JZERO 17, RST RegA, INC RegA, WRITE, JUMP 19, RST RegA, WRITE, LOAD 0, SHR RegA, STORE 0, LOAD 0, JPOS 2, HALT]

runProgram :: [Integer] -> [VMCommand] -> VM
runProgram inp program =
    execState run VM
        { input = inp
        , output = []
        , registers = newRegisters
        , memory = Map.empty
        , instructions = Array.listArray (0, toInteger $ length program - 1) program
        , instruction_pointer = 0
        , time_elapsed = 0
        , history = []
        }