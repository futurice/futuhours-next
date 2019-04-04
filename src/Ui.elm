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


roundButton : Element.Color -> Element.Color -> Msg -> String -> Element Msg
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


stepper : T.Entry -> Element Msg
stepper entry =
    let
        down =
            EditEntry entry.day { entry | hours = max 0.5 (entry.hours - 0.5) }

        up =
            EditEntry entry.day { entry | hours = min 18 (entry.hours + 0.5) }
    in    
    row
        [ spacing 10
        , Border.width 1
        , Border.rounded 5
        , padding 10
        , width (px 100)
        ]
        [ Input.button [ alignLeft ] { onPress = Just down, label = el [ ] <| faIcon "fa fa-angle-left" }
        , el [ Font.size 16, centerX, Font.center ] (text <| String.fromFloat entry.hours)
        , Input.button [ alignRight ] { onPress = Just up, label = el [ ] <| faIcon "fa fa-angle-right" }
        ]


dropdown : (Int -> Msg) -> T.Identifier -> T.Identifier -> Dict T.Identifier String -> Element Msg
dropdown handler latest value options = 
    row 
        [ width (shrink |> minimum 200)
        ] 
        [ html <| dropdownRaw handler latest value options ]


dropdownRaw : (Int -> Msg) -> T.Identifier -> T.Identifier -> Dict T.Identifier String -> Html Msg
dropdownRaw handler latest value options =
    Html.select 
        [ HA.class "dropdown"
        , HE.on "change" <| Json.map handler HEX.targetValueIntParse
        ]
        [ Html.optgroup [ HA.attribute "label" "Most Recent"] 
            [ Html.option [ HA.value <| String.fromInt latest ] [ Html.text <| Maybe.withDefault "" <| Dict.get latest options ]
            ] 
        , Html.optgroup [ HA.attribute "label" "All" ]
            (Dict.map (\id name -> Html.option [ HA.value <| String.fromInt id ] [ Html.text name ] ) options
                |> Dict.values)
        ]
        