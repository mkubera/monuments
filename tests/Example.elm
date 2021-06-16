module Example exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import Model exposing (..)
import Test exposing (..)
import Update.Attention exposing (..)


suite : Test
suite =
    -- todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    describe "Update.Attention module"
        [ describe "Update.Attention.newBuildings"
            [ test "Creates a new building" <|
                \_ ->
                    let
                        (Faction _ fBuildings _) =
                            initP1

                        newBuildingType =
                            ThirdEyeCleansers

                        index =
                            0

                        newBuilding =
                            Building newBuildingType NoAttention Low

                        expectedBuildings =
                            [ newBuilding
                            , Building (MonumentOfUs 1) NoAttention Low
                            , NoBuilding

                            -- , Building PsycheDancers NoAttention Low
                            ]
                    in
                    newBuildings fBuildings newBuildingType index
                        |> Expect.equal expectedBuildings
            ]
        ]
