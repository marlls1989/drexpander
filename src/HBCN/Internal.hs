module HBCN.Internal where

import           Algebra.Graph.Labelled
import           Data.Function
import           Data.Monoid

data StructuralElement = Port String [(String, Double)]
                       | DataReg String [(String, Double)]
                       | NullReg String [(String, Double)]
                       deriving (Show, Read, Eq, Ord)

data Transition = DataTrans {nodeName :: String}
                | NullTrans {nodeName :: String}
                deriving (Show, Read, Eq, Ord)

data Place = Unconnected
           | Place {hasToken :: Bool, weight :: Double}
           deriving (Show, Read, Eq, Ord)

type HBCN = Graph Place Transition

instance Semigroup Place where
  Unconnected <> a = a
  a <> _ = a

instance Monoid Place where
  mempty = Unconnected

createHBCNFromStructure :: [StructuralElement] -> HBCN
createHBCNFromStructure = edges . concatMap go where
  go (Port src dst) = let bkw = 10 * (2 + logBase 2 (fromIntegral $ length dst))
    in concatMap (\(x, w) -> [(Place False w,   DataTrans src, DataTrans x)
                             ,(Place False w,   NullTrans src, NullTrans x)
                             ,(Place False bkw, DataTrans x,   NullTrans src)
                             ,(Place True  bkw, NullTrans x,   DataTrans src)]) dst
  go (NullReg src dst) = let bkw = 10 * (1.5 + logBase 2 (fromIntegral $ length dst))
    in concatMap (\(x, w) -> [(Place False w,   DataTrans src, DataTrans x)
                             ,(Place False w,   NullTrans src, NullTrans x)
                             ,(Place False bkw, DataTrans x,   NullTrans src)
                             ,(Place True  bkw, NullTrans x,   DataTrans src)]) dst
  go (DataReg src dst) =
    let
      slave = src ++ "/slave"
      bkw = 10 * (1.5 + logBase 2 (fromIntegral $ length dst))
    in [(Place False 10, DataTrans src,   DataTrans slave)
       ,(Place True  10, NullTrans src,   NullTrans slave)
       ,(Place False 15, DataTrans slave, NullTrans src)
       ,(Place False 15, NullTrans slave, DataTrans src)] ++
    concatMap (\(x, w) -> [(Place True  w,   DataTrans slave, DataTrans x)
                          ,(Place False w,   NullTrans slave, NullTrans x)
                          ,(Place False bkw, DataTrans x,     NullTrans slave)
                          ,(Place False bkw, NullTrans x,     DataTrans slave)]) dst
