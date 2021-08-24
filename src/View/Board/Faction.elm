module View.Board.Faction exposing (..)

import Element exposing (..)
import Model exposing (..)
import View.Board.Buildings exposing (..)
import View.Board.People exposing (..)


viewFaction : Phase -> Faction -> Element Msg
viewFaction phase (Faction fName fBuildings fPeople) =
    row []
        [ column [] <|
            case fName of
                ThoseWhoLove ->
                    [ viewBuildings phase fName fBuildings
                    , viewPeople fPeople ThoseWhoLove
                    ]

                ThoseWhoPoison ->
                    [ viewPeople fPeople ThoseWhoPoison
                    , viewBuildings phase fName fBuildings
                    ]
        ]
