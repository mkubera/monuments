module Update.Attention exposing (..)

import Model exposing (..)


convertP1People ( people, peopleCount ) acc newPeople =
    case people of
        [] ->
            newPeople

        ((Person Love) as x) :: xs ->
            if acc >= peopleCount then
                convertP1People ( xs, peopleCount ) acc (x :: newPeople)

            else
                convertP1People ( xs, peopleCount ) (acc + 1) (Person Poison :: newPeople)

        ((Person Neutral) as x) :: xs ->
            convertP1People ( xs, peopleCount ) acc (x :: newPeople)

        ((Person Poison) as x) :: xs ->
            convertP1People ( xs, peopleCount ) acc (x :: newPeople)


convertP2People ( people, peopleCount ) acc newPeople =
    case people of
        [] ->
            newPeople

        ((Person Love) as x) :: xs ->
            convertP2People ( xs, peopleCount ) acc (x :: newPeople)

        ((Person Neutral) as x) :: xs ->
            convertP2People ( xs, peopleCount ) acc (x :: newPeople)

        ((Person Poison) as x) :: xs ->
            if acc >= peopleCount then
                convertP2People ( xs, peopleCount ) acc (x :: newPeople)

            else
                convertP2People ( xs, peopleCount ) (acc + 1) (Person Love :: newPeople)


newBuildingsLevelUp buildings =
    List.map
        (\b ->
            case b of
                Building bType bAttention bLevel ->
                    case bAttention of
                        Anima ->
                            case bLevel of
                                Low ->
                                    Building bType bAttention Mid

                                Mid ->
                                    Building bType bAttention High

                                High ->
                                    Building bType bAttention High

                                Destroyed ->
                                    b

                        Animus ->
                            b

                        AnimaAnimus ->
                            case bLevel of
                                Low ->
                                    Building bType bAttention Mid

                                Mid ->
                                    Building bType bAttention High

                                High ->
                                    Building bType bAttention High

                                Destroyed ->
                                    b

                        NoAttention ->
                            b

                NoBuilding ->
                    b
        )
        buildings


newBuildingsLevelDownMonument oppType opponentBuildings playerBuildings =
    let
        ( oppAttackingBuilding, oppMonument, pMonument ) =
            case oppType of
                ThoseWhoLove ->
                    ( PsycheDancers, MonumentOfUs, MonumentOfThem )

                ThoseWhoPoison ->
                    ( SoulEngineers, MonumentOfThem, MonumentOfUs )

        oppHasAnimusToLevelDownMonument =
            List.any
                (\b ->
                    case b of
                        Building bType bAttention _ ->
                            bType
                                == oppAttackingBuilding
                                && List.member bAttention [ Animus, AnimaAnimus ]

                        NoBuilding ->
                            False
                )
                opponentBuildings
    in
    if oppHasAnimusToLevelDownMonument then
        List.map
            (\b ->
                case b of
                    Building bType bAttention bLevel ->
                        let
                            newB =
                                case bLevel of
                                    High ->
                                        Building bType bAttention Mid

                                    Mid ->
                                        Building bType bAttention Low

                                    Low ->
                                        NoBuilding

                                    Destroyed ->
                                        b
                        in
                        case ( oppType, bType ) of
                            ( ThoseWhoLove, MonumentOfThem guards ) ->
                                if guards > 0 then
                                    Building (MonumentOfThem (guards - 1)) bAttention bLevel

                                else
                                    newB

                            ( ThoseWhoPoison, MonumentOfUs guards ) ->
                                if guards > 0 then
                                    Building (MonumentOfUs (guards - 1)) bAttention bLevel

                                else
                                    newB

                            _ ->
                                b

                    NoBuilding ->
                        b
            )
            playerBuildings

    else
        playerBuildings


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
