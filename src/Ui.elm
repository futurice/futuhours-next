module Ui exposing (..)

import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

roundButton : Element.Color -> Element.Color -> msg -> String -> Element msg
roundButton bkgColor txtColor msg label =
    Input.button
        [ Background.color bkgColor
        , Font.color txtColor
        , Font.size 30
        , Font.extraLight
        , width <| px 35
        , height <| px 35
        , Border.rounded 50
        , Border.width 1
        ]
        { onPress = Just msg
        , label = el [ centerX, centerY ] (text label)
        }