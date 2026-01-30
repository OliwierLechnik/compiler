{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Expression where

-- import Data.Equality.Graph (EGraph, ClassId, EClass(..))
-- import qualified Data.Equality.Graph as E
-- import Data.Equality.Language (Language)
-- import Data.Equality.Analysis (Analysis(..))
-- import Data.Equality.Saturation (equalitySaturation)
-- import Data.Equality.Saturation.Rewrites (Rewrite(..), RewriteRule(..))
-- import Data.Equality.Extraction (extractBest)
-- import Data.Equality.Utils (Fix(..))

-- import Data.Hashable
-- import GHC.Generics
-- import qualified Data.Map.Strict as M
-- import qualified Data.List.NonEmpty as NE
-- import Data.Foldable (toList)

-- import IR (Operand(..), BinOp(..), Expression(..), VReg)
-- import AST (IntegerVal(..))

-- --------------------------------------------------------------------------------
-- -- Language
-- --------------------------------------------------------------------------------

-- data Lang a
--   = Add a a | Sub a a | Mul a a | Div a a | Mod a a
--   | Var VReg
--   | Const IntegerVal
--   deriving (Eq, Ord, Show, Generic, Hashable, Functor, Foldable, Traversable)

-- instance Language Lang

-- --------------------------------------------------------------------------------
-- -- Affine analysis (sound)
-- --------------------------------------------------------------------------------

-- data Affine = Affine (M.Map VReg Integer) Integer | NonLinear
--   deriving (Eq, Show)

-- instance Analysis Lang Affine where
--   makeA = \case
--     Const (IntegerVal c) -> Affine M.empty c
--     Var v -> Affine (M.singleton v 1) 0
--     Add x y -> addA x y
--     Sub x y -> subA x y
--     Mul x y -> mulA x y
--     _       -> NonLinear

--   joinA a b | a == b    = a
--             | otherwise = NonLinear

-- addA (Affine m c) (Affine m' c') = Affine (M.unionWith (+) m m') (c+c')
-- addA _ _ = NonLinear

-- subA (Affine m c) (Affine m' c') =
--   Affine (M.unionWith (+) m (M.map negate m')) (c-c')
-- subA _ _ = NonLinear

-- mulA (Affine m c) (Affine m' c')
--   | M.null m  = scale c (Affine m' c')
--   | M.null m' = scale c' (Affine m c)
-- mulA _ _ = NonLinear

-- scale k (Affine m c) = Affine (M.map (*k) m) (k*c)
-- scale _ _ = NonLinear

-- --------------------------------------------------------------------------------
-- -- EGraph helpers
-- --------------------------------------------------------------------------------

-- lookupClass :: ClassId -> EGraph l a -> EClass l a
-- lookupClass = E.getClass

-- eNodes :: ClassId -> EGraph l a -> [l ClassId]
-- eNodes cid eg = NE.toList (eClassNodes (lookupClass cid eg))

-- isConst :: Integer -> ClassId -> EGraph Lang Affine -> Bool
-- isConst k cid eg =
--   any (\case Const (IntegerVal c) -> c == k; _ -> False) (eNodes cid eg)

-- --------------------------------------------------------------------------------
-- -- IR conversion
-- --------------------------------------------------------------------------------

-- toLang :: Expression -> EGraph Lang Affine -> (ClassId, EGraph Lang Affine)
-- toLang e eg = case e of
--   Imm (OpReg v) -> E.add (Var v) eg
--   Imm (OpImm i) -> E.add (Const i) eg
--   Bin op a b ->
--     let (x, eg1) = toLang a eg
--         (y, eg2) = toLang b eg1
--         n = case op of
--           OpAdd -> Add x y
--           OpSub -> Sub x y
--           OpMul -> Mul x y
--           OpDiv -> Div x y
--           OpMod -> Mod x y
--     in E.add n eg2

-- fromLang :: Lang Expression -> Expression
-- fromLang = \case
--   Add x y -> Bin OpAdd x y
--   Sub x y -> Bin OpSub x y
--   Mul x y -> Bin OpMul x y
--   Div x y -> Bin OpDiv x y
--   Mod x y -> Bin OpMod x y
--   Var v   -> Imm (OpReg v)
--   Const i -> Imm (OpImm i)

-- fmapExpr :: Fix Lang -> Expression
-- fmapExpr (Fix l) = fromLang (fmap fmapExpr l)

-- --------------------------------------------------------------------------------
-- -- Cost model (no negative constants assumed)
-- --------------------------------------------------------------------------------

-- type Cost = Int

-- costFn :: Lang Cost -> Cost
-- costFn = \case
--   Const (IntegerVal c) -> 1 + ilog2 c
--   Var _ -> 1
--   Add _ _ -> 5
--   Sub _ _ -> 5
--   Mul _ _ -> 20
--   Div _ _ -> 60
--   Mod _ _ -> 60

-- ilog2 :: Integer -> Int
-- ilog2 c | c <= 1 = 0
--         | otherwise = floor (logBase 2 (fromIntegral c :: Double))

-- --------------------------------------------------------------------------------
-- -- Typed Rewrite Rules (hegg 0.6.0.0 style)
-- --------------------------------------------------------------------------------

-- rw :: String -> (EGraph Lang Affine -> ClassId -> Maybe ClassId) -> Rewrite Lang Affine
-- rw name f = Rewrite name (RewriteRule f)

-- addZeroR = rw "add-zero-r" $ \eg cid ->
--   case eNodes cid eg of
--     Add x y : _ | isConst 0 y eg -> Just x
--     _ -> Nothing

-- addZeroL = rw "add-zero-l" $ \eg cid ->
--   case eNodes cid eg of
--     Add x y : _ | isConst 0 x eg -> Just y
--     _ -> Nothing

-- mulOneR = rw "mul-one-r" $ \eg cid ->
--   case eNodes cid eg of
--     Mul x y : _ | isConst 1 y eg -> Just x
--     _ -> Nothing

-- mulOneL = rw "mul-one-l" $ \eg cid ->
--   case eNodes cid eg of
--     Mul x y : _ | isConst 1 x eg -> Just y
--     _ -> Nothing

-- mulZeroR = rw "mul-zero-r" $ \eg cid ->
--   case eNodes cid eg of
--     Mul _ y : _ | isConst 0 y eg -> Just (fst (E.add (Const (IntegerVal 0)) eg))
--     _ -> Nothing

-- mulZeroL = rw "mul-zero-l" $ \eg cid ->
--   case eNodes cid eg of
--     Mul x _ : _ | isConst 0 x eg -> Just (fst (E.add (Const (IntegerVal 0)) eg))
--     _ -> Nothing

-- subZero = rw "sub-zero" $ \eg cid ->
--   case eNodes cid eg of
--     Sub x y : _ | isConst 0 y eg -> Just x
--     _ -> Nothing

-- subSelf = rw "sub-self" $ \eg cid ->
--   case eNodes cid eg of
--     Sub x y : _ | x == y -> Just (fst (E.add (Const (IntegerVal 0)) eg))
--     _ -> Nothing

-- addSelf = rw "add-self" $ \eg cid ->
--   case eNodes cid eg of
--     Add x y : _ | x == y ->
--       let (two, eg1) = E.add (Const (IntegerVal 2)) eg
--       in Just (fst (E.add (Mul two x) eg1))
--     _ -> Nothing

-- --------------------------------------------------------------------------------
-- -- Rule set
-- --------------------------------------------------------------------------------

-- rules :: [Rewrite Lang Affine]
-- rules =
--   [ addZeroR, addZeroL
--   , mulOneR, mulOneL
--   , mulZeroR, mulZeroL
--   , subZero, subSelf
--   , addSelf
--   ]

-- --------------------------------------------------------------------------------
-- -- Optimization entry point
-- --------------------------------------------------------------------------------

-- optimizeExpression :: Expression -> Expression
-- optimizeExpression expr =
--   let eg          = E.emptyEGraph
--       (root, eg1) = toLang expr eg
--       eg'         = equalitySaturation eg1 rules
--       (best, _)   = extractBest eg' costFn root
--   in fmapExpr best
