module View.Ui exposing (attrCursorDefault, btn, btnAttrs, phaseToString)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes exposing (attribute)
import Model exposing (..)
import View.Model exposing (..)


colors =
    { neutralBg = rgb255 133 133 170
    }


btnAttrs =
    { endTurn = [ Background.color colors.neutralBg ]
    }


btn : List (Element.Attribute msg) -> msg -> String -> Element msg
btn attrs msg txt =
    Input.button (attrs ++ [ padding 4, Border.color (rgb255 0 0 0) ])
        { onPress = Just msg
        , label = text txt
        }


attrCursorDefault : Element.Attribute msg
attrCursorDefault =
    Element.htmlAttribute (Html.Attributes.attribute "cursor" "default")


phaseToString phase =
    let
        phaseString =
            case phase of
                BuildingPhase ->
                    "Building"

                AttentionPhase ->
                    "Attention"

                ResolutionPhase ->
                    "Resolution"

                OpponentPhase ->
                    "Opponent"
    in
    "Phase: " ++ phaseString
