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

data Place = Invalid
           | Token
           | Place
           deriving (Show, Read, Eq, Ord)

type HBCN = Graph Place Transition

instance Semigroup Place where
  Invalid <> a = a
  a <> Invalid = a
  Token <> _ = Token
  _ <> Token = Token
  _ <> _ = Place

instance Monoid Place where
  mempty = Invalid

createHBCNFromStructure :: [StructuralElement] -> HBCN
createHBCNFromStructure = edges . concatMap go where
  go (Port src dst) =
    concatMap (\x -> [(Place, DataTrans src, DataTrans x)
                     ,(Place, NullTrans src, NullTrans x)
                     ,(Place, DataTrans x,   NullTrans src)
                     ,(Token, NullTrans x,   DataTrans src)]) dst
  go (NullReg src dst) =
    concatMap (\x -> [(Place, DataTrans src, DataTrans x)
                     ,(Place, NullTrans src, NullTrans x)
                     ,(Place, DataTrans x,   NullTrans src)
                     ,(Token, NullTrans x,   DataTrans src)]) dst
  go (DataReg src dst) =
    let slave = src ++ "_slave"
    in [(Place, DataTrans src,   DataTrans slave)
       ,(Token, NullTrans src,   NullTrans slave)
       ,(Place, DataTrans slave, NullTrans src)
       ,(Place, NullTrans slave, DataTrans src)] ++
    concatMap (\x -> [(Token, DataTrans slave, DataTrans x)
                     ,(Place, NullTrans slave, NullTrans x)
                     ,(Place, DataTrans x,     NullTrans slave)
                     ,(Place, NullTrans x,     DataTrans slave)]) dst
