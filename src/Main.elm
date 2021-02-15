module Main exposing (main)

import Browser
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , none
        , padding
        , paragraph
        , px
        , rgb255
        , rgba255
        , row
        , spaceEvenly
        , spacing
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Model exposing (..)
import Update.Attention exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EndRound ->
            ( model
                |> updateGameState
                |> updatePeopleToChange
                |> executeAttention
                |> resetAttentionInBuildings
                |> resetAttentionCount
                |> resetPeopleCounts
                |> incrRoundCount
                |> opponentActs
            , Cmd.batch [ rollCmd ]
            )

        GiveAttention buildingType buildingAttention ->
            ( model
                |> validateStart
                |> Maybe.andThen validateHasEnoughAttention
                |> Maybe.map (giveAttention buildingType buildingAttention)
                |> validateEnd model
            , noCmd
            )

        SaveRandomInt int ->
            ( { model | randomInt = int }, noCmd )

        Build bType fName ->
            case fName of
                ThoseWhoLove ->
                    let
                        (Faction fType fBuildings fPeople) =
                            model.p1
                    in
                    if List.any (\b -> b == NoBuilding) fBuildings then
                        let
                            newBuilding =
                                newBuildings fBuildings [] bType
                        in
                        case bType of
                            MonumentOfUs _ ->
                                ( model, noCmd )

                            _ ->
                                ( { model | p1 = Faction fType newBuilding fPeople }, noCmd )

                    else
                        ( model, noCmd )

                ThoseWhoPoison ->
                    ( model, noCmd )


opponentActs model =
    model


hasDestroyedMonument pType pBuildings =
    pBuildings
        |> List.any
            (\b ->
                case b of
                    Building bType _ bLevel ->
                        case ( pType, bType, bLevel ) of
                            ( ThoseWhoLove, MonumentOfUs _, Destroyed ) ->
                                True

                            ( ThoseWhoPoison, MonumentOfThem _, Destroyed ) ->
                                True

                            _ ->
                                False

                    NoBuilding ->
                        False
            )


updateGameState ({ p1, p2, gameState } as model) =
    let
        (Faction p1Type p1Buildings p1People) =
            p1

        (Faction p2Type p2Buildings p2People) =
            p2

        newGameState =
            if
                List.all (\p -> p == Person Poison) p1People
                    && List.all (\p -> p == Person Love) p2People
            then
                GameLevel
                -- p1 loses

            else if List.all (\p -> p == Person Poison) p1People then
                GameLost

            else if hasDestroyedMonument p1Type p1Buildings then
                GameLost
                -- P2 loses (p1 wins)

            else if List.all (\p -> p == Person Love) p2People then
                GameWon

            else if hasDestroyedMonument p2Type p2Buildings then
                GameWon

            else
                GameLevel
    in
    { model | gameState = newGameState }


updatePeopleToChange ({ p1, p2, randomInt } as model) =
    let
        (Faction p1Name p1Buildings p1People) =
            p1

        (Faction p2Name p2Buildings p2People) =
            p2

        newP2PeopleCount =
            List.map
                (\b ->
                    case b of
                        Building bType bAttention bLevel ->
                            case bType of
                                PsycheDancers ->
                                    0

                                ThirdEyeCleansers ->
                                    case bAttention of
                                        Anima ->
                                            0

                                        Animus ->
                                            1 + randomInt

                                        AnimaAnimus ->
                                            1 + randomInt

                                        NoAttention ->
                                            0

                                MonumentOfUs _ ->
                                    0

                                ChildrenOfNihil ->
                                    0

                                SoulEngineers ->
                                    0

                                MonumentOfThem _ ->
                                    0

                        NoBuilding ->
                            0
                )
                p1Buildings
                |> List.foldl (+) 0

        newP1PeopleCount =
            List.map
                (\b ->
                    case b of
                        Building bType bAttention bLevel ->
                            case bType of
                                PsycheDancers ->
                                    0

                                ThirdEyeCleansers ->
                                    0

                                MonumentOfUs _ ->
                                    0

                                ChildrenOfNihil ->
                                    case bAttention of
                                        Anima ->
                                            0

                                        Animus ->
                                            1 + randomInt

                                        AnimaAnimus ->
                                            1 + randomInt

                                        NoAttention ->
                                            0

                                SoulEngineers ->
                                    0

                                MonumentOfThem _ ->
                                    0

                        NoBuilding ->
                            0
                )
                p2Buildings
                |> List.foldl (+) 0
    in
    { model
        | p1PeopleToChange = newP1PeopleCount
        , p2PeopleToChange = newP2PeopleCount
    }


executeAttention ({ p1, p2, p1PeopleToChange, p2PeopleToChange, maxGuards } as model) =
    let
        (Faction p1Name p1Buildings p1People) =
            p1

        (Faction p2Name p2Buildings p2People) =
            p2

        newP1Buildings =
            p1Buildings
                |> newBuildingsLevelUp
                |> newBuildingsLevelDownMonument p2Name p2Buildings
                |> newBuildingsAddGuard maxGuards

        newP2Buildings =
            p2Buildings
                |> newBuildingsLevelUp
                |> newBuildingsLevelDownMonument p1Name p1Buildings
                |> newBuildingsAddGuard maxGuards

        newP1People =
            convertP1People ( p1People, p1PeopleToChange ) 0 []

        newP2People =
            convertP2People ( p2People, p2PeopleToChange ) 0 []
    in
    { model
        | p1 = Faction p1Name newP1Buildings newP1People
        , p2 = Faction p2Name newP2Buildings newP2People
    }


newBuildingsAddGuard maxGuards playerBuildings =
    List.map
        (\b ->
            case b of
                Building bType bAttention bLevel ->
                    let
                        newMonument guards =
                            Building (MonumentOfUs (guards + 1)) bAttention bLevel
                    in
                    case ( bType, bAttention ) of
                        ( MonumentOfUs guards, Animus ) ->
                            if guards < maxGuards then
                                newMonument guards

                            else
                                b

                        ( MonumentOfUs guards, AnimaAnimus ) ->
                            if guards < maxGuards then
                                newMonument guards

                            else
                                b

                        ( MonumentOfThem guards, Animus ) ->
                            if guards < maxGuards then
                                newMonument guards

                            else
                                b

                        ( MonumentOfThem guards, AnimaAnimus ) ->
                            if guards < maxGuards then
                                newMonument guards

                            else
                                b

                        _ ->
                            b

                NoBuilding ->
                    b
        )
        playerBuildings


resetAttentionInBuildings ({ p1 } as model) =
    let
        (Faction fName fBuildings fPeople) =
            p1

        newBuildings =
            List.map giveNoAttention fBuildings
    in
    { model
        | p1 = Faction fName newBuildings fPeople
    }


giveNoAttention building =
    case building of
        Building bType bAttention bLevel ->
            Building bType NoAttention bLevel

        NoBuilding ->
            building


validateHasEnoughAttention : Model -> Maybe Model
validateHasEnoughAttention model =
    if model.attention <= model.maxAttention then
        Just model

    else
        Nothing


validateStart : Model -> Maybe Model
validateStart model =
    Just model


validateEnd : Model -> Maybe Model -> Model
validateEnd oldModel mModel =
    Maybe.withDefault oldModel mModel


giveAttention : BuildingType -> BuildingAttention -> Model -> Model
giveAttention buildingType newAttentionType ({ p1, attention, maxAttention } as model) =
    let
        (Faction fName fBuildings fPeople) =
            p1

        newBuildings =
            -- CHANGE the targetted building's attention
            List.map
                (\b ->
                    case b of
                        Building bType bAttention bLevel ->
                            if bType == buildingType then
                                case ( newAttentionType, bAttention ) of
                                    ( Anima, NoAttention ) ->
                                        if attention < maxAttention then
                                            Building bType Anima bLevel

                                        else
                                            b

                                    ( Animus, NoAttention ) ->
                                        if attention < maxAttention then
                                            Building bType Animus bLevel

                                        else
                                            b

                                    ( Anima, Anima ) ->
                                        Building bType NoAttention bLevel

                                    ( Animus, Animus ) ->
                                        Building bType NoAttention bLevel

                                    ( Anima, Animus ) ->
                                        if attention < maxAttention then
                                            Building bType AnimaAnimus bLevel

                                        else
                                            b

                                    ( Animus, Anima ) ->
                                        if attention < maxAttention then
                                            Building bType AnimaAnimus bLevel

                                        else
                                            b

                                    ( Anima, AnimaAnimus ) ->
                                        Building bType Animus bLevel

                                    ( Animus, AnimaAnimus ) ->
                                        Building bType Anima bLevel

                                    ( _, _ ) ->
                                        Building bType NoAttention bLevel

                            else
                                b

                        NoBuilding ->
                            b
                )
                fBuildings

        targettedBuilding =
            -- GET the targetted building
            fBuildings
                |> List.filter
                    (\b ->
                        case b of
                            Building bType bAttention _ ->
                                bType == buildingType

                            NoBuilding ->
                                False
                    )
                |> List.head
                |> Maybe.withDefault dummyBuilding

        newAttention =
            case targettedBuilding of
                Building _ bAttention_ _ ->
                    case ( newAttentionType, bAttention_ ) of
                        ( Anima, NoAttention ) ->
                            if attention < maxAttention then
                                attention + 1

                            else
                                attention

                        ( Animus, NoAttention ) ->
                            if attention < maxAttention then
                                attention + 1

                            else
                                attention

                        ( Anima, Anima ) ->
                            attention - 1

                        ( Animus, Animus ) ->
                            attention - 1

                        ( Anima, Animus ) ->
                            if attention < maxAttention then
                                attention + 1

                            else
                                attention

                        ( Animus, Anima ) ->
                            if attention < maxAttention then
                                attention + 1

                            else
                                attention

                        ( Anima, AnimaAnimus ) ->
                            attention - 1

                        ( Animus, AnimaAnimus ) ->
                            attention - 1

                        _ ->
                            attention

                NoBuilding ->
                    attention
    in
    { model
        | p1 = Faction fName newBuildings fPeople
        , attention = newAttention
    }


incrRoundCount ({ round } as model) =
    { model | round = round + 1 }


resetAttentionCount model =
    { model | attention = initialAttention }


resetPeopleCounts ({ p1PeopleToChange, p2PeopleToChange } as model) =
    { model | p1PeopleToChange = 0, p2PeopleToChange = 0 }


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
            "PsycheDancers"

        ThirdEyeCleansers ->
            "ThirdEyeCleansers"

        MonumentOfUs _ ->
            "MonumentOfUs"

        ChildrenOfNihil ->
            "ChildrenOfNihil"

        SoulEngineers ->
            "SoulEngineers"

        MonumentOfThem _ ->
            "MonumentOfThem"


descriptions =
    { anima = "Increase the building's level."
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

        ( _, Anima ) ->
            descriptions.anima

        ( PsycheDancers, Animus ) ->
            descriptions.animus.psycheDancers

        ( PsycheDancers, AnimaAnimus ) ->
            descriptions.anima
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.psycheDancers

        ( ThirdEyeCleansers, Animus ) ->
            descriptions.animus.thirdEyeCleansers

        ( ThirdEyeCleansers, AnimaAnimus ) ->
            descriptions.anima
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.thirdEyeCleansers

        ( MonumentOfUs _, Animus ) ->
            descriptions.animus.monumentOfUs

        ( MonumentOfUs _, AnimaAnimus ) ->
            descriptions.anima
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.monumentOfUs

        ( ChildrenOfNihil, Animus ) ->
            descriptions.animus.childrenOfNihil

        ( ChildrenOfNihil, AnimaAnimus ) ->
            descriptions.anima
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.childrenOfNihil

        ( SoulEngineers, Animus ) ->
            descriptions.animus.soulEngineers

        ( SoulEngineers, AnimaAnimus ) ->
            descriptions.anima
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.soulEngineers

        ( MonumentOfThem _, Animus ) ->
            descriptions.animus.monumentOfThem

        ( MonumentOfThem _, AnimaAnimus ) ->
            descriptions.anima
                ++ animaAnimusStringJoiner
                ++ descriptions.animus.monumentOfThem


bLevelToString : BuildingLevel -> String
bLevelToString bLevel =
    "lvl "
        ++ (case bLevel of
                Low ->
                    "1"

                Mid ->
                    "2"

                High ->
                    "3"

                Destroyed ->
                    "destroyed"
           )


view : Model -> Html Msg
view ({ round, p1, p2, attention, maxAttention, gameState, title } as model) =
    Element.layout [] <|
        column []
            [ row [ centerX, padding 5 ] [ text (String.toUpper title) ]
            , case gameState of
                GameWon ->
                    row [] [ text "GAME WON!" ]

                GameLost ->
                    row [] [ text "GAME LOST!" ]

                GameStart ->
                    row [] [ text "Start menu" ]

                GameLevel ->
                    column [ width fill, centerX ]
                        [ row [ Font.size 14, centerX ] [ text ("attention: " ++ String.fromInt attention ++ "/" ++ String.fromInt maxAttention) ]
                        , row [ Font.size 13, Font.italic, centerX, padding 5 ] [ text "Destroy their Monument or convert all their people to Love." ]
                        , viewFaction p1
                        , viewFaction p2
                        , row [ padding 5, centerX ] [ el [ Font.bold ] (text ("ROUND " ++ String.fromInt round)) ]
                        , row [ padding 5, centerX ] [ btn [] EndRound "End Round" ]
                        ]
            ]


viewFaction (Faction fName fBuildings fPeople) =
    row []
        [ column [] <|
            case fName of
                ThoseWhoLove ->
                    [ viewBuildings fName fBuildings
                    , viewPeople fPeople
                    ]

                ThoseWhoPoison ->
                    [ viewPeople fPeople
                    , viewBuildings fName fBuildings
                    ]
        ]


viewPeople people =
    row [ centerX, padding 12 ] <|
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


boardBldgNo =
    3


boardBldgWidth =
    220


boardBldgSpacing =
    10


boardWidth =
    boardBldgWidth * boardBldgNo + boardBldgSpacing * boardBldgNo


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


viewBuildings : FactionName -> List Building -> Element Msg
viewBuildings fName fBuildings =
    row
        [ width (px boardWidth)
        , centerX
        , spacing boardBldgSpacing
        , padding 10
        ]
    <|
        List.map
            (\bldg ->
                let
                    bldgBgColor =
                        case bldg of
                            Building bType _ _ ->
                                bTypeToBgColor bType

                            NoBuilding ->
                                rgba255 150 150 150 0.3

                    ( attentionBtn1, attentionBtn2, bDescription ) =
                        case fName of
                            ThoseWhoLove ->
                                case bldg of
                                    Building bType bAttention _ ->
                                        ( btn [] (GiveAttention bType Anima) (bAttentionToString Anima bAttention)
                                        , btn [] (GiveAttention bType Animus) (bAttentionToString Animus bAttention)
                                        , row [ width fill, centerX, Font.size 14 ]
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
                            , spacing 5
                            , padding 20
                            , Background.color bldgBgColor
                            , Font.size 16
                            ]
                            [ row [ width fill, spaceEvenly ]
                                [ column [ alignLeft ] [ attentionBtn1 ]
                                , column [ alignLeft ] [ attentionBtn2 ]
                                , column [ alignRight ] [ el [] (text (bLevelToString bLevel)) ]
                                ]
                            , row [ centerX ] [ text (bTypeToString bType) ]
                            , bDescription
                            , bGuards
                            ]

                    NoBuilding ->
                        column
                            [ width (px boardBldgWidth)
                            , spacing 5
                            , padding 20
                            , Background.color bldgBgColor
                            , Font.size 16
                            ]
                            [ row [ centerX ] [ text (String.toUpper "No Building") ]
                            , row [ centerX ]
                                [ column []
                                    [ row [ centerX ] [ btn [] (Build PsycheDancers ThoseWhoLove) "PsycheDancers" ]
                                    , row [ centerX ] [ btn [] (Build ThirdEyeCleansers ThoseWhoLove) "ThirdEyeCleansers" ]
                                    ]
                                ]
                            ]
            )
            fBuildings


btn attrs msg txt =
    Input.button attrs
        { onPress = Just msg
        , label = text txt
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


dummyBuilding =
    NoBuilding
