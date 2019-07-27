module HBCN.Internal where

import           Algebra.Graph.Labelled
import           Data.Function
import           Data.Monoid
import           Data.Semigroup         as SG

data StructuralElement = Port String [String]
                       | DataReg String [String]
                       | NullReg String [String]
                       deriving (Show, Read, Eq, Ord)

data Transition = DataTrans {nodeName :: String}
                | NullTrans {nodeName :: String}
                deriving (Show, Read, Eq, Ord)

data Place = Unconnected
           | Place {hasToken :: Bool}
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
  go (Port src dst) = concatMap
    (\x -> [(Place False, DataTrans src, DataTrans x)
           ,(Place False, NullTrans src, NullTrans x)
           ,(Place False, DataTrans x,   NullTrans src)
           ,(Place True,  NullTrans x,   DataTrans src)]) dst
  go (NullReg src dst) = concatMap
    (\x -> [(Place False, DataTrans src, DataTrans x)
           ,(Place False, NullTrans src, NullTrans x)
           ,(Place False, DataTrans x,   NullTrans src)
           ,(Place True,  NullTrans x,   DataTrans src)]) dst
  go (DataReg src dst) =
    let
      sout = src ++ "/sout"
      sin = src ++ "/sin"
    in -- Input Slave
      [(Place False, DataTrans src,  DataTrans sin)
      ,(Place True,  NullTrans src,  NullTrans sin)
      ,(Place False, DataTrans sin,  NullTrans src)
      ,(Place False, NullTrans sin,  DataTrans src)
       -- Data stage
      ,(Place True,  DataTrans sin,  DataTrans sout)
      ,(Place False, NullTrans sin,  NullTrans sout)
      ,(Place False, DataTrans sout, NullTrans sin)
      ,(Place False, NullTrans sout, DataTrans sin)] ++
       -- Output slave
    concatMap (\x -> [(Place False, DataTrans sout, DataTrans x)
                     ,(Place False, NullTrans sout, NullTrans x)
                     ,(Place False, DataTrans x,    NullTrans sout)
                     ,(Place True,  NullTrans x,    DataTrans sout)]) dst
