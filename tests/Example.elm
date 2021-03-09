module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Update.Attention exposing (..)
import Model exposing (..)

suite : Test
suite =
    -- todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    describe "Update.Attention module" [
      describe "Update.Attention.newBuildings" [
        test "Creates a new building" <|
        \_ ->
          let
            (Faction _ fBuildings _) = initP1
            newBuildingType = ThirdEyeCleansers
            newBuilding = (Building newBuildingType NoAttention Low)
            expectedBuildings = [ newBuilding
                , Building (MonumentOfUs 1) NoAttention High
                , Building PsycheDancers NoAttention Low
                ]
          in
            newBuildings fBuildings [] newBuildingType
            |> Expect.equal expectedBuildings
      ]
    ]