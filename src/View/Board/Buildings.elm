module View.Board.Buildings exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Model exposing (..)
import View.Board.Utils exposing (..)
import View.Ui exposing (..)


bTypeToBgColor : BuildingType -> Element.Color
bTypeToBgColor bType =
    case bType of
        PsycheDancers ->
            rgb255 203 134 215

        ThirdEyeCleansers ->
            rgb255 84 245 187

        MonumentOfUs _ ->
            rgb255 134 157 215

        ChildrenOfNihil ->
            rgb255 59 150 117

        SoulEngineers ->
            rgb255 124 81 131

        MonumentOfThem _ ->
            rgb255 84 99 135


bTypeToString : BuildingType -> String
bTypeToString bType =
    case bType of
        PsycheDancers ->
            "Psyche Dancers"

        ThirdEyeCleansers ->
            "Third Eye Cleansers"

        MonumentOfUs _ ->
            "Monument Of Us"

        ChildrenOfNihil ->
            "Children Of Nihil"

        SoulEngineers ->
            "Soul Engineers"

        MonumentOfThem _ ->
            "Monument Of Them"


bAttentionToString : BuildingAttention -> BuildingAttention -> String
bAttentionToString btnAttenionType bAttention =
    case ( btnAttenionType, bAttention ) of
        ( Anima, Anima ) ->
            "\u{1F90D}"

        ( Anima, AnimaAnimus ) ->
            "\u{1F90D}"

        ( Animus, Animus ) ->
            "\u{1F5A4}"

        ( Animus, AnimaAnimus ) ->
            "\u{1F5A4}"

        ( _, _ ) ->
            "✖"


bLevelToString : BuildingLevel -> String
bLevelToString bLevel =
    ""
        ++ (case bLevel of
                Low ->
                    "1"

                Mid ->
                    "2"

                High ->
                    "3"

                Destroyed ->
                    "0"
           )


descriptions : { anima : { psycheDancers : String, thirdEyeCleansers : String, monumentOfUs : String, childrenOfNihil : String, soulEngineers : String, monumentOfThem : String }, animus : { psycheDancers : String, thirdEyeCleansers : String, monumentOfUs : String, childrenOfNihil : String, soulEngineers : String, monumentOfThem : String } }
descriptions =
    { anima =
        { psycheDancers = "Increase the building's level."
        , thirdEyeCleansers = "Increase the building's level."
        , monumentOfUs = "Increase the building's level. (+1 max Attention Point.)"
        , childrenOfNihil = "Increase the building's level."
        , soulEngineers = "Increase the building's level."
        , monumentOfThem = "Increase the building's level."
        }
    , animus =
        { psycheDancers = "Lower the level of opponent's Monument by 1 or kill a guard if present. (50% chance of success)."
        , thirdEyeCleansers = "Convert opponent's people (1 + 0-1)."
        , monumentOfUs = "Add a guard. (Guards kill Them when the monument is invaded.)"
        , childrenOfNihil = "Convert opponent's people (1 + 0-1)."
        , soulEngineers = "Lower the level of opponent's Monument by 1 or kill a guard if present. (50% chance of success)."
        , monumentOfThem = "Add a guard. (Guards kill Them when the monument is invaded.)"
        }
    }


bAttentionToDescription : BuildingType -> BuildingAttention -> String
bAttentionToDescription bType bAttention =
    let
        animaAnimusStringJoiner =
            "\nAND\n"
    in
    case ( bType, bAttention ) of
        ( _, NoAttention ) ->
            ""

        ( PsycheDancers, Anima ) ->
            descriptions.anima.psycheDancers

        ( PsycheDancers, Animus ) ->
            descriptions.animus.psycheDancers

        ( PsycheDancers, AnimaAnimus ) ->
            descriptions.anima.psycheDancers
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.psycheDancers

        ( ThirdEyeCleansers, Anima ) ->
            descriptions.anima.thirdEyeCleansers

        ( ThirdEyeCleansers, Animus ) ->
            descriptions.animus.thirdEyeCleansers

        ( ThirdEyeCleansers, AnimaAnimus ) ->
            descriptions.anima.thirdEyeCleansers
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.thirdEyeCleansers

        ( MonumentOfUs _, Anima ) ->
            descriptions.anima.monumentOfUs

        ( MonumentOfUs _, Animus ) ->
            descriptions.animus.monumentOfUs

        ( MonumentOfUs _, AnimaAnimus ) ->
            descriptions.anima.monumentOfUs
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.monumentOfUs

        ( ChildrenOfNihil, Anima ) ->
            descriptions.anima.childrenOfNihil

        ( ChildrenOfNihil, Animus ) ->
            descriptions.animus.childrenOfNihil

        ( ChildrenOfNihil, AnimaAnimus ) ->
            descriptions.anima.childrenOfNihil
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.childrenOfNihil

        ( SoulEngineers, Anima ) ->
            descriptions.anima.soulEngineers

        ( SoulEngineers, Animus ) ->
            descriptions.animus.soulEngineers

        ( SoulEngineers, AnimaAnimus ) ->
            descriptions.anima.soulEngineers
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.soulEngineers

        ( MonumentOfThem _, Anima ) ->
            descriptions.anima.monumentOfThem

        ( MonumentOfThem _, Animus ) ->
            descriptions.animus.monumentOfThem

        ( MonumentOfThem _, AnimaAnimus ) ->
            descriptions.anima.monumentOfThem
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.monumentOfThem


btnGiveAttention : Phase -> BuildingAttention -> BuildingType -> BuildingAttention -> Element Msg
btnGiveAttention phase btnType bType bAttention =
    case phase of
        BuildingPhase ->
            el [ attrCursorDefault, padding 4, Border.color (rgb255 0 0 0) ] (text (bAttentionToString btnType bAttention))

        AttentionPhase ->
            btn [] (GiveAttention bType btnType) (bAttentionToString btnType bAttention)

        ResolutionPhase ->
            btn [] Noop (bAttentionToString btnType bAttention)

        OpponentPhase ->
            btn [] Noop (bAttentionToString btnType bAttention)


viewBuildings : Phase -> FactionName -> List Building -> Element Msg
viewBuildings phase fName fBuildings =
    row
        [ width (px boardWidth)
        , centerX
        , spacing boardBldgSpacing
        , padding 10
        ]
    <|
        List.indexedMap
            (\index bldg ->
                let
                    bldgColor =
                        case fName of
                            ThoseWhoLove ->
                                rgb255 134 157 215

                            ThoseWhoPoison ->
                                rgb255 84 245 187

                    ( attentionBtn1, attentionBtn2, bDescription ) =
                        case fName of
                            ThoseWhoLove ->
                                case bldg of
                                    Building bType bAttention _ ->
                                        ( btnGiveAttention phase Anima bType bAttention
                                        , btnGiveAttention phase Animus bType bAttention
                                          --   btn [] (GiveAttention bType Anima) (bAttentionToString Anima bAttention)
                                          -- , btn [] (GiveAttention bType Animus) (bAttentionToString Animus bAttention)
                                        , row [ width fill, centerX, Font.size 14, Font.center ]
                                            [ paragraph [] [ text (bAttentionToDescription bType bAttention) ] ]
                                        )

                                    NoBuilding ->
                                        ( none, none, none )

                            ThoseWhoPoison ->
                                ( none, none, none )

                    bGuards =
                        let
                            guardsCols guards =
                                List.repeat guards (column [] [ text "💂\u{200D}♂️" ])
                        in
                        case bldg of
                            Building bType _ _ ->
                                case bType of
                                    MonumentOfUs guards ->
                                        row [ centerX ] (guardsCols guards)

                                    MonumentOfThem guards ->
                                        row [ centerX ] (guardsCols guards)

                                    _ ->
                                        none

                            NoBuilding ->
                                none
                in
                case bldg of
                    Building bType _ bLevel ->
                        column
                            [ width (px boardBldgWidth)
                            , height fill
                            , Font.size 16
                            ]
                            [ row [ width fill, spaceEvenly ]
                                [ column [ alignLeft ] [ attentionBtn1 ]
                                , column [ alignLeft ] [ attentionBtn2 ]
                                , column
                                    [ Border.color bldgColor
                                    , Border.solid
                                    , Border.width 1
                                    , alignRight
                                    , height fill
                                    , width (px 20)
                                    ]
                                    [ el
                                        [ centerX
                                        , centerY
                                        , Font.size 14
                                        , Font.bold
                                        , Font.color bldgColor
                                        ]
                                        (text (bLevelToString bLevel))
                                    ]
                                ]
                            , row
                                [ Border.color bldgColor
                                , Border.solid
                                , Border.width 1
                                , width fill
                                , height (px 100)
                                ]
                                [ column
                                    [ centerX
                                    , spacing 5
                                    , width fill
                                    ]
                                    [ row [ centerX, Font.color bldgColor ] [ text (bTypeToString bType) ]
                                    , bDescription
                                    , bGuards
                                    ]
                                ]
                            ]

                    NoBuilding ->
                        let
                            availableBuildingTypes : Buildings -> List BuildingType -> List BuildingType
                            availableBuildingTypes bldgs newBldgTypes =
                                case bldgs of
                                    [] ->
                                        newBldgTypes

                                    x :: xs ->
                                        case x of
                                            Building bType _ _ ->
                                                -- remove BuildingType if building already exists
                                                if List.member bType newBldgTypes then
                                                    availableBuildingTypes xs (List.filter (\newBType -> newBType /= bType) newBldgTypes)

                                                else
                                                    availableBuildingTypes xs newBldgTypes

                                            NoBuilding ->
                                                availableBuildingTypes xs newBldgTypes
                        in
                        column
                            [ width (px boardBldgWidth)
                            , spacing 5
                            , padding 20
                            , Background.color bldgColor
                            , Font.size 16
                            ]
                            [ row [ centerX ] [ text (String.toUpper "No Building") ]
                            , row [ centerX ]
                                [ column [] <|
                                    List.map
                                        (\building ->
                                            row [ centerX ] [ btn [] (Build building ThoseWhoLove index) (bTypeToString building) ]
                                        )
                                        (availableBuildingTypes fBuildings [ PsycheDancers, ThirdEyeCleansers ])
                                ]
                            ]
            )
            fBuildings
