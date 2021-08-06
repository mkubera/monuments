module View.Board.Faction exposing (..)

import Element exposing (..)
import Model exposing (..)
import View.Board.Buildings exposing (..)
import View.Board.People exposing (..)


viewFaction : Faction -> Element Msg
viewFaction (Faction fName fBuildings fPeople) =
    row []
        [ column [] <|
            case fName of
                ThoseWhoLove ->
                    [ viewBuildings fName fBuildings
                    , viewPeople fPeople ThoseWhoLove
                    ]

                ThoseWhoPoison ->
                    [ viewPeople fPeople ThoseWhoPoison
                    , viewBuildings fName fBuildings
                    ]
        ]
