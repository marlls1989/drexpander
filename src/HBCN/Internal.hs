module HBCN.Internal where

import           Algebra.Graph.Labelled
import           Data.Semigroup         as SG
import Data.List

data StructuralElement = Port String [String]
                       | NullReg String [String]
                       | LoopReg String [String]
                       | DataReg String [String]
                       deriving (Show, Read, Eq, Ord)

data Transition = DataTrans {nodeName :: String}
                | NullTrans {nodeName :: String}
                deriving (Show, Read, Eq, Ord)

data ProtocolPhase = ReqData | AckData | ReqNull | AckNull
                   deriving (Show, Read, Eq, Ord)

data Channel = UnconnectedChannel
             | Channel { protocolPhase :: ProtocolPhase, fixedChannelDelay :: Maybe Double, relaxChannelDelay :: Bool, reflexiveChannel :: Bool}
             deriving (Show, Read, Eq, Ord)

data Place = Unconnected
           | Place {relaxedPlaceDelay :: Bool, fixedPlaceDelay :: Maybe Double, hasToken :: Bool}
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
      sout = src ++ "/sout"
      sin = src ++ "/sin"
    in overlays $
       [(src -< Channel ReqNull minDelay False False >- sin)
       ,(sin -< Channel ReqData minDelay False False >- sout)] ++
       map (\x ->  sout -< Channel AckNull Nothing True True >-  x) dst
  go (LoopReg src dst) =
    let
      sout = src ++ "/sout"
      sin = src ++ "/sin"
    in overlays $
       [(src  -< Channel ReqNull minDelay False False >- sin)
       ,(sin  -< Channel ReqData minDelay False False >- sout)
       ,(sout -< Channel AckNull Nothing True  True  >- src)] ++
       map (\x ->  sout -< Channel AckNull Nothing True True >-  x) dst

hbcnFromStructuralGraph :: StructuralGraph -> HBCN
hbcnFromStructuralGraph g = overlay (baseHBCNFromStructuralGraph g) (reflexiveHBCNFromStructuralGraph g)

baseHBCNFromStructuralGraph :: StructuralGraph -> HBCN
baseHBCNFromStructuralGraph = overlays . map go . edgeList where
  go (Channel phase minDelay relax _,  src,  dst) = overlays
    [(DataTrans src -< Place relax minDelay (phase == ReqData) >- DataTrans dst)
    ,(DataTrans dst -< Place relax minDelay (phase == AckData) >- NullTrans src)
    ,(NullTrans src -< Place relax minDelay (phase == ReqNull) >- NullTrans dst)
    ,(NullTrans dst -< Place relax minDelay (phase == AckNull) >- DataTrans src)]

reflexiveHBCNFromStructuralGraph :: StructuralGraph -> HBCN
reflexiveHBCNFromStructuralGraph = overlays . concatMap go . groupBy (\(_, _, x) (_, _, y) -> x == y) . filter (\(c, _, _) -> reflexiveChannel c) . edgeList where
  go [] = []
  go ((Channel xphase xminDelay xrelax _, src, _):xs) =
    [overlay
      (DataTrans src -< Place xrelax xminDelay (xphase <= AckData) >- NullTrans src)
      (NullTrans src -< Place xrelax xminDelay (xphase >= ReqNull) >- DataTrans src)] ++
    map (\(Channel yphase yminDelay yrelax _, dst, _) ->
            overlays [(DataTrans src -< Place xrelax xminDelay (xphase <= AckData) >- NullTrans dst)
                     ,(NullTrans src -< Place xrelax xminDelay (xphase >= ReqNull) >- DataTrans dst)
                     ,(DataTrans dst -< Place yrelax yminDelay (yphase <= AckData) >- NullTrans src)
                     ,(NullTrans dst -< Place yrelax yminDelay (yphase >= ReqNull) >- DataTrans src)
                     ]) xs ++ go xs

createHBCNFromStructure :: Maybe Double -> [StructuralElement] -> HBCN
createHBCNFromStructure minDelay = hbcnFromStructuralGraph . structuralGraphFromElements minDelay
