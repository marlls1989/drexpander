module HBCN.Internal where

import           Algebra.Graph.Labelled
import           Data.Function
import           Data.Monoid
import           Data.Semigroup         as SG

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
  mappend = (SG.<>)

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
      sout = src ++ "/sout"
      sin = src ++ "/sin"
      bkw = 10 * (1.5 + logBase 2 (fromIntegral $ length dst))
    in -- Input Slave
      [(Place False 10, DataTrans src,  DataTrans sin)
      ,(Place True  10, NullTrans src,  NullTrans sin)
      ,(Place False 15, DataTrans sin,  NullTrans src)
      ,(Place False 15, NullTrans sin,  DataTrans src)
       -- Data stage
      ,(Place True  10, DataTrans sin,  DataTrans sout)
      ,(Place False 10, NullTrans sin,  NullTrans sout)
      ,(Place False 15, DataTrans sout, NullTrans sin)
      ,(Place False 15, NullTrans sout, DataTrans sin)] ++
       -- Output slave
    concatMap (\(x, w) -> [(Place False w,   DataTrans sout, DataTrans x)
                          ,(Place False w,   NullTrans sout, NullTrans x)
                          ,(Place False bkw, DataTrans x,    NullTrans sout)
                          ,(Place True  bkw, NullTrans x,    DataTrans sout)]) dst
