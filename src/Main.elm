module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import HoursList exposing (hoursList)
import Html exposing (Html)
import Html.Attributes as HA exposing (class, style)
import Model exposing (Flags, Model, isMobile)
import Msg exposing (Msg(..))
import TopBar exposing (topBar)
import Types as T
import Ui exposing (colors)
import Update exposing (update)



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResize ]



---- VIEW ----


mainLayout : Model -> Element Msg
mainLayout model =
    let
        errorElem =
            case model.hasError of
                Just err ->
                    Ui.errorMsg err

                Nothing ->
                    if List.isEmpty model.saveQueue && not model.isLoading then
                        none

                    else
                        Ui.waiting
    in
    column
        [ Background.color colors.bodyBackground
        , width fill
        , height fill
        , htmlAttribute <| style "height" "100vh"
        , Element.inFront errorElem
        ]
        [ topBar model
        , hoursList model
        ]


view : Model -> Html Msg
view model =
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                -- This shadow will disappear in Windows High-Contrast mode
                -- We add a transparent outline on .focusable in main.css
                , shadow =
                    Just
                        { color = colors.focus
                        , offset = ( 0, 0 )
                        , blur = 0
                        , size = 3
                        }
                }
            ]
        }
        [ Font.family [ Font.typeface "FuturiceSans" ]
        , Font.extraLight
        ]
        (mainLayout model)



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = Model.init
        , update = update
        , subscriptions = subscriptions
        }
