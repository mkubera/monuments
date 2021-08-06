module View.Board.People exposing (..)

import Element exposing (..)
import Element.Font as Font
import Model exposing (..)
import View.Model exposing (PaddingEach)


viewPeople : List Person -> FactionName -> Element msg
viewPeople people fName =
    let
        peopleText =
            case fName of
                ThoseWhoLove ->
                    "Us"

                ThoseWhoPoison ->
                    "Them"
    in
    column [ centerX, padding 12 ]
        [ row [ centerX, paddingEach (PaddingEach 0 0 5 0) ] [ el [ Font.size 14 ] (text peopleText) ]
        , row [] <|
            List.map
                (\p ->
                    column []
                        [ case p of
                            Person Love ->
                                text "💕 "

                            Person Poison ->
                                text "🍄 "

                            Person Neutral ->
                                text "🎭"
                        ]
                )
                people
        ]
