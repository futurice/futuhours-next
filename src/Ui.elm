module Ui exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as HA


colors =
    { topBarBackground = rgb255 0 54 67
    , bodyBackground = rgb255 227 236 236
    , darkText = rgb255 133 178 190
    , white = rgb 1 1 1
    , gray = rgb255 96 96 96
    , lightGray = rgb 0.75 0.75 0.75
    , holidayGray = rgb255 205 219 220
    , holidayYellow = rgb255 254 254 230
    , black = rgb 0 0 0
    }


faIcon : String -> Element msg
faIcon c =
    html <| Html.i [ HA.class c ] []


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


stepper : Float -> msg -> msg -> Element msg
stepper val up down =
    row
        [ spacing 10
        , Border.width 1
        , Border.rounded 5
        , padding 10
        , width (px 80)
        ]
        [ Input.button [ alignLeft ] { onPress = Just down, label = el [ ] <| faIcon "fa fa-angle-left" }
        , el [ Font.size 16, Font.center ] (text <| String.fromFloat val)
        , Input.button [ alignRight ] { onPress = Just up, label = el [ ] <| faIcon "fa fa-angle-right" }
        ]


dropdown = 
    row 
        [ width (shrink |> minimum 200)
        ] 
        [ html dropdownRaw ]


dropdownRaw =
    Html.select 
        [ HA.class "dropdown" ]
        [ Html.optgroup [ HA.attribute "label" "Most Frequent"] 
            [ Html.option [] [ Html.text "IT" ]
            ] 
        , Html.optgroup [ HA.attribute "label" "All" ]
            [ Html.option [] [ Html.text "IT" ]
            , Html.option [] [ Html.text "Marketing" ]
            , Html.option [] [ Html.text "Sales" ]
            ]
        ]
        