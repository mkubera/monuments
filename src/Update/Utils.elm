module Update.Utils exposing (..)

import Model exposing (..)


getP1Bldgs : Faction -> Buildings
getP1Bldgs p1 =
    let
        (Faction _ fBuildings _) =
            p1
    in
    fBuildings


getP1Monument : Buildings -> Building
getP1Monument fBuildings =
    List.filter
        (\b ->
            case b of
                Building (MonumentOfUs _) _ _ ->
                    True

                _ ->
                    False
        )
        fBuildings
        |> List.head
        |> Maybe.withDefault dummyBuilding


getP1BldgLevel : Building -> BuildingLevel
getP1BldgLevel b =
    case b of
        Building _ _ bLevel ->
            bLevel

        NoBuilding ->
            Destroyed
