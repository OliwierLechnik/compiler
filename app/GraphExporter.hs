module GraphExporter where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List (intercalate)
import IR

-- | Escapes double quotes for JSON strings
escape :: String -> String
escape [] = []
escape ('"':xs) = "\\\"" ++ escape xs
escape ('\\':xs) = "\\\\" ++ escape xs
escape (x:xs) = x : escape xs

-- | Formats a VReg using the DebugMap
-- Returns "v1" or "v1(varName)"
showVReg :: DebugMap -> VReg -> String
showVReg dbg (VReg n) = 
    let base = "v" ++ show n
    in case Map.lookup n dbg of
        Just name -> base ++ "(" ++ T.unpack name ++ ")"
        Nothing   -> base

showOperand :: DebugMap -> Operand -> String
showOperand dbg (OpReg r) = showVReg dbg r
showOperand _   (OpImm i) = show i

-- | Formats Phi Nodes
showPhi :: DebugMap -> Phi -> String
showPhi dbg (Phi v sources) = 
    showVReg dbg v ++ " <- (" ++ intercalate ", " (map showSource sources) ++ ")"
  where
    showSource (lbl, op) = "[" ++ showOperand dbg op ++ ", " ++ T.unpack lbl ++ "]"

-- | Formats instructions for the graph
showMiddle :: DebugMap -> Middle -> String
showMiddle dbg (Move v op) = 
    showVReg dbg v ++ " <- " ++ showOperand dbg op
showMiddle dbg (Compute op v o1 o2) = 
    showVReg dbg v ++ " <- " ++ showOperand dbg o1 ++ " " ++ show op ++ " " ++ showOperand dbg o2
showMiddle dbg (Load v addr) = 
    showVReg dbg v ++ " <- Mem[" ++ show addr ++ "]"
showMiddle dbg (Store addr op) = 
    "Mem[" ++ show addr ++ "] <- " ++ showOperand dbg op
showMiddle dbg (LoadIndirect v va) = 
    showVReg dbg v ++ " <- Mem[" ++ showVReg dbg va ++ "]"
showMiddle dbg (StoreIndirect va op) = 
    "Mem[" ++ showVReg dbg va ++ "] <- " ++ showOperand dbg op
showMiddle dbg (Print op) = 
    "Print " ++ showVReg dbg op
showMiddle dbg (ReadInput v) = 
    "Read " ++ showVReg dbg v
showMiddle _   (Call l) = 
    "Call " ++ T.unpack l
showMiddle dbg (StoreRetAddr v) = 
    showVReg dbg v ++ " <- RegA (RetAddr)"
showMiddle dbg (LoadRetAddr v) = 
    "RegA <- " ++ showVReg dbg v ++ " (RetAddr)"
showMiddle _   (Comment t) = 
    "# " ++ T.unpack t

showTerminator :: DebugMap -> Terminator -> String
showTerminator _   (Jump l) = "Jump " ++ T.unpack l
showTerminator dbg (Branch c v1 v2 _ _) = 
    "Branch " ++ show c ++ " " ++ showVReg dbg v1 ++ " " ++ showOperand dbg v2
showTerminator dbg Return = 
    "Return "
showTerminator _   Halt = "Halt"

-- | Generates the list of edges (Target, Label)
getEdges :: Terminator -> [(String, String)]
getEdges (Jump l) = [(T.unpack l, "")]
getEdges (Branch _ _ _ lTrue lFalse) = [(T.unpack lTrue, "true"), (T.unpack lFalse, "false")]
getEdges Return = []
getEdges Halt = []

-- | Serializes a single block to JSON object
blockToJSON :: DebugMap -> Block -> String
blockToJSON dbg (Block lbl phis insns term) =
    let 
        lblStr = T.unpack lbl
        
        -- Pass dbg to formatters
        phisJson = "[" ++ intercalate ", " (map (\p -> "\"" ++ escape (showPhi dbg p) ++ "\"") phis) ++ "]"
        insnsJson = "[" ++ intercalate ", " (map (\i -> "\"" ++ escape (showMiddle dbg i) ++ "\"") insns) ++ "]"
        termJson = "\"" ++ escape (showTerminator dbg term) ++ "\""
        
        edgesJson = "[" ++ intercalate ", " (map edgeToObj (getEdges term)) ++ "]"
        edgeToObj (target, tag) = "{\"target\": \"" ++ target ++ "\", \"label\": \"" ++ tag ++ "\"}"
    in
        "  {\n" ++
        "    \"id\": \"" ++ lblStr ++ "\",\n" ++
        "    \"phis\": " ++ phisJson ++ ",\n" ++
        "    \"instructions\": " ++ insnsJson ++ ",\n" ++
        "    \"terminator\": " ++ termJson ++ ",\n" ++
        "    \"edges\": " ++ edgesJson ++ "\n" ++
        "  }"

-- | Serializes the whole program
-- Note: Pattern matching on ProgramIR to extract the DebugMap
exportToJSON :: ProgramIR -> String
exportToJSON (ProgramIR blockMap dbgMap) =
    let blocks = Map.elems blockMap
    in "{ \"blocks\": [\n" ++ intercalate ",\n" (map (blockToJSON dbgMap) blocks) ++ "\n]}"

-- | Write to file
saveGraph :: FilePath -> ProgramIR -> IO ()
saveGraph path prog = writeFile path (exportToJSON prog)