module View.Ui exposing (btn, btnAttrs)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
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
