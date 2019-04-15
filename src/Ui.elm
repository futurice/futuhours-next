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


roundButton : Bool -> Element.Color -> Element.Color -> Msg -> String -> Element Msg
roundButton disabled bkgColor txtColor msg label =
    Input.button
        [ Background.color (if disabled then colors.lightGray else bkgColor)
        , Font.color (if disabled then colors.gray else txtColor)
        , Font.size 30
        , Font.extraLight
        , width <| px 35
        , height <| px 35
        , Border.rounded 50
        , Border.width 1
        ]
        { onPress = if disabled then Nothing else Just msg
        , label = el [ centerX, centerY ] (text label)
        }


stepper : Bool -> T.Entry -> Element Msg
stepper disabled entry =
    let
        down =
           if disabled then Nothing else Just <| EditEntry entry.day { entry | hours = max 0.5 (entry.hours - 0.5) }

        up =
            if disabled then Nothing else Just <| EditEntry entry.day { entry | hours = min 18 (entry.hours + 0.5) } 
    in    
    row
        [ spacing 10
        , Border.width 1
        , Border.rounded 5
        , Border.color (if disabled then colors.lightGray else colors.black)
        , padding 10
        , width (px 100)
        , Font.color (if disabled then colors.gray else colors.black)
        ]
        [ Input.button [ alignLeft ] { onPress = down, label = el [ ] <| faIcon "fa fa-angle-left" }
        , numberDropdown disabled entry
        , Input.button [ alignRight ] { onPress = up, label = el [ ] <| faIcon "fa fa-angle-right" }
        ]


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

numberDropdown : Bool -> T.Entry -> Element Msg
numberDropdown disabled entry =
    el 
        [ width fill ]
        (html <| numberDropdownRaw disabled entry)


numberDropdownRaw : Bool -> T.Entry -> Html Msg
numberDropdownRaw disabled entry =
    let
        options =
            List.range 1 36
                |> List.map toFloat
                |> List.map (\x -> x * 0.5)

        handler val =
            EditEntry entry.day { entry | hours = val }

        optEl opt =
            Html.option [ HA.value <| String.fromFloat opt, HA.selected (entry.hours == opt) ] [ Html.text <| String.fromFloat opt ]
    in    
    Html.select
        [ HA.class "dropdown"
        , HE.on "change" <| Json.map handler HEX.targetValueFloatParse 
        , HA.disabled disabled
        ]
        (List.map optEl options)