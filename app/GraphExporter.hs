-- Oliwier Lechnik 279760

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

-- | Formats a VReg
showVReg :: VReg -> String
showVReg (VReg n) = "v" ++ show n

showOperand :: Operand -> String
showOperand (OpReg r) = showVReg r
showOperand (OpImm i) = show i

-- | Formats Phi Nodes
showPhi :: Phi -> String
showPhi (Phi v sources) = 
    showVReg v ++ " <- (" ++ intercalate ", " (map showSource sources) ++ ")"
  where
    showSource (lbl, op) = "[" ++ showOperand op ++ ", " ++ T.unpack lbl ++ "]"

-- | Formats instructions for the graph
showMiddle :: Middle -> String
showMiddle (Move v op) = 
    showVReg v ++ " <- " ++ showOperand op
showMiddle (Compute op v o1 o2) = 
    showVReg v ++ " <- " ++ showOperand o1 ++ " " ++ show op ++ " " ++ showOperand o2
showMiddle (Load v addr) = 
    showVReg v ++ " <- Mem[" ++ show addr ++ "]"
showMiddle (Store addr op) = 
    "Mem[" ++ show addr ++ "] <- " ++ showOperand op
showMiddle (LoadIndirect v va) = 
    showVReg v ++ " <- Mem[" ++ showVReg va ++ "]"
showMiddle (StoreIndirect va op) = 
    "Mem[" ++ showVReg va ++ "] <- " ++ showOperand op
showMiddle (Print v) = 
    "Print " ++ showVReg v
showMiddle (ReadInput v) = 
    "Read " ++ showVReg v
showMiddle (Call l args) = 
    "Call " ++ T.unpack l ++ "(" ++ intercalate ", " (map showOperand args) ++ ")"
showMiddle (StoreRetAddr v) = 
    showVReg v ++ " <- RegA (RetAddr)"
showMiddle (LoadRetAddr v) = 
    "RegA <- " ++ showVReg v ++ " (RetAddr)"
showMiddle (Comment t) = 
    "# " ++ T.unpack t

showTerminator :: Terminator -> String
showTerminator (Jump l) = "Jump " ++ T.unpack l
showTerminator (Branch c v1 v2 _ _) = 
    "Branch " ++ show c ++ " " ++ showVReg v1 ++ " " ++ showOperand v2
showTerminator Return = "Return"
showTerminator Halt = "Halt"

-- | Generates the list of edges (Target, Label)
getEdges :: Terminator -> [(String, String)]
getEdges (Jump l) = [(T.unpack l, "")]
getEdges (Branch _ _ _ lTrue lFalse) = [(T.unpack lTrue, "true"), (T.unpack lFalse, "false")]
getEdges Return = []
getEdges Halt = []

-- | Serializes a single block to JSON object
blockToJSON :: Block -> String
blockToJSON (Block lbl args phis insns term) = -- UPDATED pattern match for args
    let 
        -- Format Label with Arguments
        lblBase = T.unpack lbl
        lblStr = if null args 
                 then lblBase
                 else lblBase ++ "(" ++ intercalate ", " (map showVReg args) ++ ")"
        
        phisJson = "[" ++ intercalate ", " (map (\p -> "\"" ++ escape (showPhi p) ++ "\"") phis) ++ "]"
        insnsJson = "[" ++ intercalate ", " (map (\i -> "\"" ++ escape (showMiddle i) ++ "\"") insns) ++ "]"
        termJson = "\"" ++ escape (showTerminator term) ++ "\""
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
exportToJSON :: ProgramIR -> String
exportToJSON (ProgramIR blockMap) =
    let blocks = Map.elems blockMap
    in "{ \"blocks\": [\n" ++ intercalate ",\n" (map blockToJSON blocks) ++ "\n]}"

-- | Write to file
saveGraph :: FilePath -> ProgramIR -> IO ()
saveGraph path prog = writeFile path (exportToJSON prog)