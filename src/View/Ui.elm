module View.Ui exposing (attrCursorDefault, btn, btnAttrs, endTurnBtn, phaseToString)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes exposing (attribute)
import Model exposing (..)
import View.Model exposing (..)


colors : { neutralBg : Color }
colors =
    { neutralBg = rgb255 133 133 170
    }


btnAttrs : { endTurn : List (Attr decorative msg), endPhase : List (Attr a b) }
btnAttrs =
    { endTurn = [ Background.color colors.neutralBg ]
    , endPhase = [ Background.color colors.neutralBg ]
    }


btn : List (Element.Attribute msg) -> msg -> String -> Element msg
btn attrs msg txt =
    Input.button (attrs ++ [ padding 4, Border.color (rgb255 0 0 0) ])
        { onPress = Just msg
        , label = text txt
        }


endTurnBtn : Phase -> List (Element.Attribute msg) -> msg -> String -> Element msg
endTurnBtn phase attrs msg txt =
    case phase of
        AttentionPhase ->
            Input.button (attrs ++ [ padding 4, Border.color (rgb255 0 0 0) ])
                { onPress = Just msg
                , label = text txt
                }

        _ ->
            none


attrCursorDefault : Element.Attribute msg
attrCursorDefault =
    Element.htmlAttribute (Html.Attributes.attribute "cursor" "default")


phaseToString : Phase -> String
phaseToString phase =
    let
        phaseString =
            case phase of
                BuildingPhase ->
                    "Building"

                AttentionPhase ->
                    "Attention"

        -- ResolutionPhase ->
        --     "Resolution"
        -- OpponentPhase ->
        --     "Opponent"
    in
    "Phase: " ++ phaseString
