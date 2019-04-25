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
    Element.layout
        [ Font.family [ Font.typeface "Work Sans" ]
        , Font.light
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
