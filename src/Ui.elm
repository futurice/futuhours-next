module Ui exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Events.Extra as HEX
import Types as T exposing (Msg(..))
import Json.Decode as Json


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


faIcon : String -> Element Msg
faIcon c =
    html <| Html.i [ HA.class c ] []


scButton : List (Element.Attribute Msg) -> Msg -> String -> Element Msg
scButton attrs msg label =
    Input.button
        ([ Font.size 14, width (px 100), height (px 40), Border.rounded 5 ] ++ attrs)
        { onPress = Just msg, label = text label }


roundButton : Bool -> Bool -> Element.Color -> Element.Color -> Msg -> Element Msg -> Element Msg
roundButton disabled hasBorder bkgColor txtColor msg label =
    Input.button
        [ Background.color (if disabled then colors.lightGray else bkgColor)
        , Font.color (if disabled then colors.gray else txtColor)
        , Font.size 30
        , Font.extraLight
        , width <| px 35
        , height <| px 35
        , Border.rounded 50
        , Border.width (if hasBorder then 1 else 0)
        ]
        { onPress = if disabled then Nothing else Just msg
        , label = el [ centerX, centerY ] label
        }


dropdown : Bool -> (Int -> Msg) -> T.Identifier -> T.Identifier -> Dict T.Identifier String -> Element Msg
dropdown disabled handler latest value options = 
    row 
        [ width fill
        ] 
        [ html <| dropdownRaw disabled handler latest value options ]


dropdownRaw : Bool -> (Int -> Msg) -> T.Identifier -> T.Identifier -> Dict T.Identifier String -> Html Msg
dropdownRaw disabled handler latest value options =
    Html.select 
        [ HA.class "dropdown"
        , HE.on "change" <| Json.map handler HEX.targetValueIntParse
        , HA.disabled disabled
        ]
        [ Html.optgroup [ HA.attribute "label" "Most Recent"] 
            [ Html.option [ HA.value <| String.fromInt latest ] [ Html.text <| Maybe.withDefault "" <| Dict.get latest options ]
            ] 
        , Html.optgroup [ HA.attribute "label" "All" ]
            (Dict.map (\id name -> Html.option [ HA.value <| String.fromInt id ] [ Html.text name ] ) options
                |> Dict.values)
        ]

numberDropdown : Bool -> (Float -> Msg) -> T.Entry -> Element Msg
numberDropdown disabled handler entry =
    el 
        [ width fill ]
        (html <| numberDropdownRaw disabled handler entry)


numberDropdownRaw : Bool -> (Float -> Msg) -> T.Entry -> Html Msg
numberDropdownRaw disabled handler entry =
    let
        options =
            List.range 1 36
                |> List.map toFloat
                |> List.map (\x -> x * 0.5)

        optEl opt =
            Html.option [ HA.value <| String.fromFloat opt, HA.selected (entry.hours == opt) ] [ Html.text <| String.fromFloat opt ]
    in    
    Html.select
        [ HA.class "dropdown"
        , HE.on "change" <| Json.map handler HEX.targetValueFloatParse 
        , HA.disabled disabled
        ]
        (List.map optEl options)