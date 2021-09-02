module Main exposing (main)

import Browser
import Html exposing (b)
import Model exposing (..)
import Update.Attention exposing (..)
import Update.Utils exposing (getP1BldgLevel, getP1Bldgs, getP1Monument)
import View
import View.Board.Faction exposing (..)
import View.Ui exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EndRound ->
            ( model
                |> updatePeopleToConvert
                |> executeAttention
                |> updateGameState
                |> updateMaxAttention
                |> updateAttention
                |> resetAttentionInBuildings
                |> resetPeopleCounts
                |> incrRoundCount
                |> opponentActs
            , Cmd.batch [ rollCmd ]
            )

        EndPhase ->
            let
                nextPhase =
                    case model.phase of
                        BuildingPhase ->
                            AttentionPhase

                        AttentionPhase ->
                            ResolutionPhase

                        ResolutionPhase ->
                            OpponentPhase

                        OpponentPhase ->
                            BuildingPhase
            in
            ( { model | phase = nextPhase }, noCmd )

        GiveAttention buildingType buildingAttention ->
            ( if playerHasEnoughAttention model then
                model
                    |> giveAttention buildingType buildingAttention

              else
                model
              -- |> validateStart
              -- |> Maybe.andThen validateHasEnoughAttention
              -- |> Maybe.map (giveAttention buildingType buildingAttention)
              -- |> validateEnd model
            , noCmd
            )

        SaveRandomInt int ->
            ( { model | randomInt = int }, noCmd )

        Build bType fName bldgIndex ->
            case fName of
                ThoseWhoLove ->
                    let
                        (Faction fType fBuildings fPeople) =
                            model.p1
                    in
                    if List.any (\b -> b == NoBuilding) fBuildings then
                        let
                            newFactionBuildings =
                                newBuildings fBuildings bType bldgIndex
                        in
                        case bType of
                            MonumentOfUs _ ->
                                ( model, noCmd )

                            _ ->
                                ( { model | p1 = Faction fType newFactionBuildings fPeople }, noCmd )

                    else
                        ( model, noCmd )

                ThoseWhoPoison ->
                    ( model, noCmd )

        ChangeGameState newGameState ->
            ( { model | gameState = newGameState }, noCmd )

        StartGameOver ->
            ( initialData, noCmd )

        Noop ->
            ( model, noCmd )


updateMaxAttention : Model -> Model
updateMaxAttention model =
    let
        monumentLevel =
            model.p1
                |> getP1Bldgs
                |> getP1Monument
                |> getP1BldgLevel
                |> convertMonumentLevelToMaxAttention
    in
    { model | maxAttention = monumentLevel }


opponentActs : a -> a
opponentActs model =
    -- TODO
    model


hasDestroyedMonument : FactionName -> Buildings -> Bool
hasDestroyedMonument fName pBuildings =
    List.all
        (\b ->
            case ( fName, b ) of
                ( ThoseWhoLove, Building (MonumentOfUs _) _ _ ) ->
                    False

                ( ThoseWhoPoison, Building (MonumentOfThem _) _ _ ) ->
                    False

                _ ->
                    True
        )
        pBuildings


updateGameState : { a | p1 : Faction, p2 : Faction, gameState : GameState } -> { a | p1 : Faction, p2 : Faction, gameState : GameState }
updateGameState ({ p1, p2 } as model) =
    let
        (Faction p1FactionName p1Buildings p1People) =
            p1

        (Faction p2FactionName p2Buildings p2People) =
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

            else if hasDestroyedMonument p1FactionName p1Buildings then
                GameLost
                -- P2 loses (p1 wins)

            else if List.all (\p -> p == Person Love) p2People then
                GameWon

            else if hasDestroyedMonument p2FactionName p2Buildings then
                GameWon

            else
                GameLevel
    in
    { model | gameState = newGameState }


updatePeopleToConvert : { a | p1 : Faction, p2 : Faction, randomInt : number, p1PeopleToChange : number, p2PeopleToChange : number } -> { a | p1 : Faction, p2 : Faction, randomInt : number, p1PeopleToChange : number, p2PeopleToChange : number }
updatePeopleToConvert ({ p1, p2, randomInt } as model) =
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


executeAttention : { a | p1 : Faction, p2 : Faction, p1PeopleToChange : number, p2PeopleToChange : number, maxGuards : Guards } -> { a | p1 : Faction, p2 : Faction, p1PeopleToChange : number, p2PeopleToChange : number, maxGuards : Guards }
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


newBuildingsAddGuard : Guards -> List Building -> List Building
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


resetAttentionInBuildings : { a | p1 : Faction } -> { a | p1 : Faction }
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


giveNoAttention : Building -> Building
giveNoAttention building =
    case building of
        Building bType bAttention bLevel ->
            Building bType NoAttention bLevel

        NoBuilding ->
            building


playerHasEnoughAttention : Model -> Bool
playerHasEnoughAttention model =
    model.attention <= model.maxAttention


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

        wtf =
            getP1Bldgs p1
                |> getP1Monument
                |> (\b ->
                        case b of
                            Building bType bAttention _ ->
                                case ( bType, newAttentionType, bAttention ) of
                                    ( MonumentOfUs _, Anima, NoAttention ) ->
                                        if attention < maxAttention then
                                            "ok"

                                        else
                                            "not ok"

                                    _ ->
                                        "nope"

                            NoBuilding ->
                                "no bldg"
                   )

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
        , log = wtf
    }


incrRoundCount : { a | round : number } -> { a | round : number }
incrRoundCount ({ round } as model) =
    { model | round = round + 1 }


updateAttention : Model -> Model
updateAttention model =
    { model | attention = model.maxAttention }


resetPeopleCounts : { a | p1PeopleToChange : number, p2PeopleToChange : number } -> { a | p1PeopleToChange : number, p2PeopleToChange : number }
resetPeopleCounts ({ p1PeopleToChange, p2PeopleToChange } as model) =
    { model | p1PeopleToChange = 0, p2PeopleToChange = 0 }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }
