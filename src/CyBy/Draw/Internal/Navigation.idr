module CyBy.Draw.Internal.Navigation

import Chem.Util
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Role
import Data.Fin
import Data.Graph.Indexed
import Data.List
import Geom

%default total

public export
data Direction = N | W | S | E

dirAngle : Direction -> Angle
dirAngle N = threeHalfPi
dirAngle W = pi
dirAngle S = halfPi
dirAngle E = zero

-- Returns a list of pairs with bond angles and the corresponding
-- 'global' Fin k of all visible neighbours.
bondAnglesWithNodes : {g : CDGraph} -> Node g -> List (Angle, Node g)
bondAnglesWithNodes x =
  let p  := pointId (lab x)
      ns := visibleNeighbours x
  in mapMaybe (\fn => (,fn) <$> angle (pointId (lab fn) - p)) ns

-- Given an input angle and two nodes (with their positions and indices),
-- returns the Fin k of the node that lies in the direction most closely
-- matching the input angle.
bestPointId : Direction -> (Point Id, Node g) -> (Point Id, Node g) -> Node g
bestPointId E (p1, i1) (p2, i2) = if p1.x >= p2.x then i1 else i2
bestPointId S (p1, i1) (p2, i2) = if p1.y >= p2.y then i1 else i2
bestPointId W (p1, i1) (p2, i2) = if p1.x <= p2.x then i1 else i2
bestPointId N (p1, i1) (p2, i2) = if p1.y <= p2.y then i1 else i2

-- An angular direction margin that is slightly smaller than half pi, in order
-- to prevent the active node/edge to change perpendicular to the input angle.
DirectionMargin : Angle
DirectionMargin = Geom.Angle.angle (7 * pi / 16)

-- Returns the angle of the line connecting two nodes, if possible,
-- regardless of whether they are directly connected in the graph.
angleEdge : {g : CDGraph} -> (n1,n2 : Node g) -> Maybe Angle
angleEdge n1 n2 =
  let p1 := pointId (lab n1)
      p2 := pointId (lab n2)
   in angle (p2 - p1)

||| Given an input angle and a currently hovered node or edge,
||| attempts to transfer the role Hover to the most appropriate
||| neighboring node or edge based on angular proximity.
||| In the case of edge-to-node navigation, node positions are also
||| evaluated to determine the best match.
||| If no suitable candidate is found, the graph is returned unchanged.
export
moveActive : Direction -> CDGraph -> CDGraph
-- moveActive d g =
--   case hoveredItem g of
--     N (i, _) =>
--       case minBy (minDelta (dirAngle d) . fst) (bondAnglesWithNodes i) of
--         Just (b,j) =>
--           if minDelta b (dirAngle d) < DirectionMargin
--              then updateEdge j i (set Hover) (updateNode i (unset Hover))
--              else g
--         Nothing => g
--     E (LE x y _) =>
--       case angleEdge x y of
--         Just angl =>
--           if minDelta (dirAngle d) angl <= DirectionMargin || 
--              minDelta (dirAngle d) (angl + pi)  <= DirectionMargin 
--             then
--               let g := updateEdge x y (unset Hover) g
--                   n := bestPointId d (pointAt g x, x) (pointAt g y, y)
--               in updateNode n (set Hover) g
--             else g
--         Nothing => g
--     None => g
