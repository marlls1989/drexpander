module HBCN.Internal where

import           Algebra.Graph.Labelled
import           Data.Semigroup         as SG

data StructuralElement = Port String [String]
                       | LoopReg String [String]
                       | DataReg String [String]
                       | NullReg String [String]
                       deriving (Show, Read, Eq, Ord)

data Transition = DataTrans {nodeName :: String}
                | NullTrans {nodeName :: String}
                deriving (Show, Read, Eq, Ord)

data Place = Unconnected
           | Place {hasToken :: Bool}
           | MindelayPlace {hasToken :: Bool}
           | StrictPlace {hasToken :: Bool}
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
  go (Port src dst) =
    [(Place False, DataTrans src, NullTrans src)
    ,(Place True,  NullTrans src, DataTrans src)] ++ concatMap
    (\x -> [(Place False, DataTrans src, DataTrans x)
           ,(Place False, NullTrans src, NullTrans x)
           ,(Place False, DataTrans x,   NullTrans src)
           ,(Place True,  NullTrans x,   DataTrans src)]) dst
  go (NullReg src dst) =
    [(Place False, DataTrans src, NullTrans src)
    ,(Place True,  NullTrans src, DataTrans src)] ++ concatMap
    (\x -> [(Place False, DataTrans src, DataTrans x)
           ,(Place False, NullTrans src, NullTrans x)
           ,(Place False, DataTrans x,   NullTrans src)
           ,(Place True,  NullTrans x,   DataTrans src)]) dst
  go (DataReg src dst) =
    let
      sout = src ++ "/sout"
      sin = src ++ "/sin"
    in -- Input Slave
      [(MindelayPlace False, DataTrans src,  DataTrans sin)
      ,(MindelayPlace True,  NullTrans src,  NullTrans sin)
      ,(MindelayPlace False, DataTrans sin,  NullTrans src)
      ,(MindelayPlace False, NullTrans sin,  DataTrans src)
       -- Data stage
      ,(MindelayPlace True,  DataTrans sin,  DataTrans sout)
      ,(MindelayPlace False, NullTrans sin,  NullTrans sout)
      ,(MindelayPlace False, DataTrans sout, NullTrans sin)
      ,(MindelayPlace False, NullTrans sout, DataTrans sin)
       -- Output slave
      ,(Place False, DataTrans sout, NullTrans sout)
      ,(Place True,  NullTrans sout, DataTrans sout)] ++ concatMap
      (\x -> [(Place False, DataTrans sout, DataTrans x)
             ,(Place False, NullTrans sout, NullTrans x)
             ,(Place False, DataTrans x,    NullTrans sout)
             ,(Place True,  NullTrans x,    DataTrans sout)]) dst
  go (LoopReg src dst) =
    let
      sout = src ++ "/sout"
      sin = src ++ "/sin"
    in -- Input Slave
      [(MindelayPlace False, DataTrans src,  DataTrans sin)
      ,(MindelayPlace True,  NullTrans src,  NullTrans sin)
      ,(MindelayPlace False, DataTrans sin,  NullTrans src)
      ,(MindelayPlace False, NullTrans sin,  DataTrans src)
       -- Data stage
      ,(MindelayPlace True,  DataTrans sin,  DataTrans sout)
      ,(MindelayPlace False, NullTrans sin,  NullTrans sout)
      ,(MindelayPlace False, DataTrans sout, NullTrans sin)
      ,(MindelayPlace False, NullTrans sout, DataTrans sin)
       -- Output slave
      ,(Place False, DataTrans sout, NullTrans sout)
      ,(Place True,  NullTrans sout, DataTrans sout)
      ,(Place False, DataTrans sout, DataTrans src)
      ,(Place False, NullTrans sout, NullTrans src)
      ,(Place False, DataTrans src,  NullTrans sout)
      ,(Place True,  NullTrans src,  DataTrans sout)] ++ concatMap
      (\x -> [(Place False, DataTrans sout, DataTrans x)
             ,(Place False, NullTrans sout, NullTrans x)
             ,(Place False, DataTrans x,    NullTrans sout)
             ,(Place True,  NullTrans x,    DataTrans sout)]) dst
