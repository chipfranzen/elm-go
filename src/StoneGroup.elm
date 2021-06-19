module StoneGroup exposing (..)

import Coordinate exposing (Coordinate)
import Set exposing (Set)
import Stone exposing (Stone)
import String
import String.Interpolate


type alias StoneGroup =
    { color : Stone
    , locations : Set Coordinate
    }


newGroup : Stone -> Coordinate -> StoneGroup
newGroup color location =
    { color = color
    , locations = Set.singleton location
    }


addStone : StoneGroup -> Stone -> Coordinate -> Result String StoneGroup
addStone group color location =
    case group.color == color of
        False ->
            Err
                (String.Interpolate.interpolate
                    "Trying to add a {0} stone to a {1} group."
                    [ Stone.toString color, Stone.toString group.color ]
                )

        True ->
            case Set.member location (neigbors group) of
                False ->
                    Err
                        (String.join " "
                            [ "Trying to add disconnected stone to group at "
                            , Coordinate.toString location
                            ]
                        )

                True ->
                    Ok { group | locations = Set.insert location group.locations }


neighbors : StoneGroup -> Set Coordinate
neighbors group =
    let
        foldFunc stoneLocation stoneGroup =
            Coordinate.neighbors stoneLocation
                |> Set.union stoneGroup
    in
    Set.foldl foldFunc Set.empty group.locations
        |> reverseDiff group.locations


reverseDiff : Set comparable -> Set comparable -> Set comparable
reverseDiff set2 set1 =
    Set.diff set1 set2


shouldMerge : StoneGroup -> StoneGroup -> Bool
shouldMerge group1 group2 =
  let
      commonStones = Set.intersect group1 group2
      sameColor = group1.color == group2.color
  in
      sameColor && not (Set.isEmpty commonStones)


mergeGroups StoneGroup -> StoneGroup -> Result String StoneGroup
mergeGroups group1 group2 =
  let
      sameColor = group1.color == group2.color
  in
    case sameColor of
      False ->
        Err "Attempting to merge two groups of different colors."
      True ->
        Ok {group1 | locations = Set.union group1.locations group2.locations}
