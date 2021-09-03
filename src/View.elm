module View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Model exposing (..)
import View.Board.Faction exposing (..)
import View.Model exposing (PaddingEach)
import View.Ui exposing (btn, btnAttrs, endTurnBtn, phaseToString)


view : Model -> Html Msg
view ({ round, p1, p2, attention, maxAttention, gameState, phase, title, log } as model) =
    Element.layout
        [ Background.color (rgb255 57 62 80)
        , Font.color (rgb255 255 255 255)
        , Font.family
            [ Font.typeface "RobotoCondensed"
            , Font.sansSerif
            ]
        ]
    <|
        column [ width fill, height fill ]
            [ row [ centerX, centerY, padding 5 ] [ text (String.toUpper title) ]
            , case gameState of
                GameWon ->
                    row [ centerX ]
                        [ column []
                            [ row [] [ text "GAME WON!" ]
                            , row [] [ btn [] StartGameOver "Start again" ]
                            ]
                        ]

                GameLost ->
                    row [ centerX ]
                        [ column []
                            [ row [] [ text "GAME LOST" ]
                            , row [] [ btn [] StartGameOver "Start again" ]
                            ]
                        ]

                GameStart ->
                    row [ centerX ]
                        [ column []
                            [ row [] [ text "Start menu" ]
                            , row [] [ btn [] (ChangeGameState GameLevel) "New game" ]
                            ]
                        ]

                GameLevel ->
                    column [ centerX, centerY ]
                        [ row [ Font.size 13, Font.italic, centerX, paddingEach (PaddingEach 0 0 20 0) ]
                            [ text "Destroy their Monument or convert all their people to Love."
                            ]
                        , row [ Font.size 18, centerX ]
                            [ el [] (text (phaseToString phase))
                            , btn btnAttrs.endPhase EndPhase "End Phase"
                            ]
                        , row [ padding 5, centerX ]
                            [ el [ Font.bold ] (text ("ROUND " ++ String.fromInt round))
                            ]
                        , row [ Font.size 14, centerX ]
                            [ text
                                ("attention pts: "
                                    ++ String.fromInt attention
                                    ++ "/"
                                    ++ String.fromInt maxAttention
                                )
                            ]
                        , row [ padding 5, centerX ]
                            [ endTurnBtn phase btnAttrs.endTurn EndRound "End Round"
                            ]
                        , row [ padding 5, centerX ]
                            [ text log
                            ]
                        , viewFaction phase p1
                        , viewFaction phase p2
                        ]
            ]
