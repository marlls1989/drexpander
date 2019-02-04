module HBCN.Internal where

import           Algebra.Graph.Labelled
import           Data.Function
import           Data.Monoid

data StructuralElement = Port String [String]
                       | DataReg String [String]
                       | NullReg String [String]
                       deriving (Show, Read, Eq, Ord)

data Transition = DataTrans String
                | NullTrans String
                deriving (Show, Read, Eq, Ord)

newtype Place = Place {tokenCount :: Int}
              deriving (Show, Read, Eq, Ord)

type HBCN = Graph Place Transition

instance Semigroup Place where
  (<>) a = Place . ((+) `on` tokenCount) a

instance Monoid Place where
  mempty = Place 0

createHBCNFromStructure :: [StructuralElement] -> HBCN
createHBCNFromStructure = edges . concatMap go where
  go (Port src dst) =
    concatMap (\x -> [(Place 0, DataTrans src, DataTrans x)
                     ,(Place 0, NullTrans src, NullTrans x)
                     ,(Place 0, DataTrans x,   NullTrans src)
                     ,(Place 1, NullTrans x,   DataTrans src)]) dst
  go (NullReg src dst) =
    concatMap (\x -> [(Place 0, DataTrans src, DataTrans x)
                     ,(Place 0, NullTrans src, NullTrans x)
                     ,(Place 0, DataTrans x,   NullTrans src)
                     ,(Place 1, NullTrans x,   DataTrans src)]) dst
  go (DataReg src dst) =
    let slave = src ++ "_slave"
    in [(Place 1, DataTrans src,   DataTrans slave)
       ,(Place 0, NullTrans src,   NullTrans slave)
       ,(Place 0, DataTrans slave, NullTrans src)
       ,(Place 0, NullTrans slave, DataTrans src)] ++
    concatMap (\x -> [(Place 0, DataTrans slave, DataTrans x)
                     ,(Place 0, NullTrans slave, NullTrans x)
                     ,(Place 0, DataTrans x,     NullTrans slave)
                     ,(Place 1, NullTrans x,     DataTrans slave)]) dst
