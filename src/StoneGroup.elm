module StoneGroup exposing (..)

import Coordinate exposing (Coordinate)
import Dict exposing (Dict)
import List
import Set exposing (Set)
import Stone exposing (Stone)
import StoneDict exposing (StoneDict)
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


invert : StoneGroup -> StoneDict
invert group =
    Dict.fromList (List.map (\x -> ( x, group.color )) (Set.toList group.locations))


toDict : List StoneGroup -> StoneDict
toDict groups =
    let
        foldFunc group acc =
            Dict.union acc (invert group)
    in
    List.foldl foldFunc Dict.empty groups


contains : StoneGroup -> Stone -> Coordinate -> Bool
contains group color location =
    (color == group.color) && Set.member location group.locations


isNeighbor : StoneGroup -> Coordinate -> Int -> Bool
isNeighbor group location boardSize =
    Set.member location (neighbors group boardSize)


addStone : StoneGroup -> Stone -> Coordinate -> Int -> Result String StoneGroup
addStone group color location boardSize =
    case group.color == color of
        False ->
            Err
                (String.Interpolate.interpolate
                    "Trying to add a {0} stone to a {1} group."
                    [ Stone.toString color, Stone.toString group.color ]
                )

        True ->
            case Set.member location (neighbors group boardSize) of
                False ->
                    Err
                        (String.join " "
                            [ "Trying to add disconnected stone to group at "
                            , Coordinate.toString location
                            ]
                        )

                True ->
                    Ok { group | locations = Set.insert location group.locations }


neighbors : StoneGroup -> Int -> Set Coordinate
neighbors group boardSize =
    let
        foldFunc stoneLocation stoneGroup =
            Coordinate.neighbors stoneLocation boardSize
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
        commonStones =
            Set.intersect group1.locations group2.locations

        sameColor =
            group1.color == group2.color
    in
    sameColor && not (Set.isEmpty commonStones)


mergeGroups : StoneGroup -> StoneGroup -> StoneGroup
mergeGroups group1 group2 =
    { group1 | locations = Set.union group1.locations group2.locations }


updateGroups : List StoneGroup -> Stone -> Coordinate -> Int -> List StoneGroup
updateGroups stoneGroups color location boardSize =
    let
        newStoneGroup =
            newGroup color location

        partitionFunc =
            \group -> isNeighbor group location boardSize && group.color == color

        ( unionGroups, noOpGroups ) =
            List.partition partitionFunc stoneGroups

        nGroups =
            List.length stoneGroups
    in
    case nGroups == 0 of
        True ->
            List.singleton newStoneGroup

        False ->
            List.foldl mergeGroups newStoneGroup unionGroups :: noOpGroups


killGroups : List StoneGroup -> Int -> List StoneGroup
killGroups groups boardSize =
    let
        boardState =
            toDict groups
    in
    List.filter (\group -> libertyCount group boardState boardSize > 0) groups


libertyCount : StoneGroup -> StoneDict -> Int -> Int
libertyCount group boardState boardSize =
    let
        mapfunc =
            \x -> libertyToInt (StoneDict.isVacant boardState x)
    in
    List.sum (List.map mapfunc (Set.toList (neighbors group boardSize)))


libertyToInt : Bool -> Int
libertyToInt liberty =
    case liberty of
        True ->
            1

        False ->
            0
