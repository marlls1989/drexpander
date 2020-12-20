module HBCN.Internal where

import           Algebra.Graph.Labelled
import           Data.List
import           Data.Semigroup         as SG
import           Data.Set               (Set)
import qualified Data.Set               as Set

data StructuralElement = Port String [String]
                       | NullReg String [String]
                       | DataReg String [String]
                       deriving (Show, Read, Eq, Ord)

data Transition = DataTrans {nodeName :: String}
                | NullTrans {nodeName :: String}
                deriving (Show, Read, Eq, Ord)

data ProtocolPhase = ReqData | AckData | ReqNull | AckNull
                   deriving (Show, Read, Eq, Ord)

data Channel = UnconnectedChannel
             | Channel
               { protocolPhase     :: ProtocolPhase
               , fixedChannelDelay :: Maybe Double
               , relaxChannelDelay :: Bool
               , reflexiveChannel  :: Bool
               }
             deriving (Show, Read, Eq, Ord)

data Place = Unconnected
           | Place
             { relaxedPlaceDelay :: Bool
             , fixedPlaceDelay   :: Maybe Double
             , hasToken          :: Bool
             }
           | ReflexivePlace
             { relaxedPlaceDelay  :: Bool
             , fixedPlaceDelay    :: Maybe Double
             , hasToken           :: Bool
             , relatedTransitions :: Set Transition
             }
           deriving (Show, Read, Eq, Ord)

type HBCN = Graph Place Transition
type StructuralGraph = Graph Channel String

instance Semigroup Channel where
  UnconnectedChannel <> a = a
  a <> UnconnectedChannel = a
  a <> b  = max a b

instance Monoid Channel where
  mempty = UnconnectedChannel
  mappend = (SG.<>)

instance Semigroup Place where
  Unconnected <> a = a
  a <> Unconnected = a
  a@ReflexivePlace {relatedTransitions = x} <> ReflexivePlace {relatedTransitions = y} = a {relatedTransitions = Set.union x y}
  a <> b  = max a b

instance Monoid Place where
  mempty = Unconnected
  mappend = (SG.<>)

structuralGraphFromElements :: Maybe Double -> [StructuralElement] -> StructuralGraph
structuralGraphFromElements minDelay = overlays . map go where
  go (Port src dst) = overlays $ map (\x -> src -< Channel AckNull Nothing True True >- x) dst
  go (NullReg src dst) = overlays $ map (\x -> src -< Channel AckNull Nothing True True >- x) dst
  go (DataReg src dst) =
    let
      s1 = src ++ "/s1"
      s0 = src ++ "/s0"
    in overlays $
       [(src -< Channel ReqNull minDelay False False >- s0)
       ,(s0 -< Channel ReqData minDelay False False >- s1)] ++
       map (\x ->  s1 -< Channel AckNull Nothing True True >-  x) dst

hbcnFromStructuralGraph :: StructuralGraph -> HBCN
hbcnFromStructuralGraph x = overlays [baseHBCNFromStructuralGraph x, reflexiveHBCNFromStructuralGraph x]

baseHBCNFromStructuralGraph :: StructuralGraph -> HBCN
baseHBCNFromStructuralGraph = overlays . map go . edgeList where
  go (Channel {protocolPhase = phase,  fixedChannelDelay = minDelay, relaxChannelDelay = relax} ,  src,  dst) =
    let
      place = Place {relaxedPlaceDelay = relax, fixedPlaceDelay = minDelay, hasToken = False}
    in overlays
       [(DataTrans src -< place {hasToken = phase == ReqData} >- DataTrans dst)
       ,(DataTrans dst -< place {hasToken = phase == AckData} >- NullTrans src)
       ,(NullTrans src -< place {hasToken = phase == ReqNull} >- NullTrans dst)
       ,(NullTrans dst -< place {hasToken = phase == AckNull} >- DataTrans src)]
  go _ = empty

reflexiveHBCNFromStructuralGraph :: StructuralGraph -> HBCN
reflexiveHBCNFromStructuralGraph =
  overlays . concatMap go .
  groupBy (\(_, _, x) (_, _, y) -> x == y) .
  sortBy (\(_, _, x) (_, _, y) -> compare x y).
  filter (\(c, _, _) -> reflexiveChannel c) . edgeList where
  go [] = []
  go ((Channel {protocolPhase = xphase, fixedChannelDelay = xminDelay, reflexiveChannel = xrelax }, src, related):xs) = let
    placex = ReflexivePlace { relaxedPlaceDelay = xrelax, fixedPlaceDelay = xminDelay, hasToken = False, relatedTransitions = Set.empty }
    related'd = Set.singleton (DataTrans related)
    related'n = Set.singleton (NullTrans related)
    in [overlay
         (DataTrans src -< placex { hasToken = xphase <= AckData, relatedTransitions = related'd } >- NullTrans src)
         (NullTrans src -< placex { hasToken = xphase >= ReqNull, relatedTransitions = related'n } >- DataTrans src)] ++
       map (\(Channel yphase yminDelay yrelax _, dst, _) -> let
               placey = ReflexivePlace { relaxedPlaceDelay = yrelax, fixedPlaceDelay = yminDelay, hasToken = False, relatedTransitions = Set.empty }
           in overlays [(DataTrans src -< placey { hasToken = xphase <= AckData, relatedTransitions = related'd } >- NullTrans dst)
                       ,(NullTrans src -< placey { hasToken = xphase >= ReqNull, relatedTransitions = related'n  } >- DataTrans dst)
                       ,(DataTrans dst -< placey { hasToken = yphase <= AckData, relatedTransitions = related'd  } >- NullTrans src)
                       ,(NullTrans dst -< placey { hasToken = yphase >= ReqNull, relatedTransitions = related'n  } >- DataTrans src)
                       ]) xs ++ go xs
  go (_:xs) = go xs

createHBCNFromStructure :: Maybe Double -> [StructuralElement] -> HBCN
createHBCNFromStructure minDelay = hbcnFromStructuralGraph . structuralGraphFromElements minDelay
