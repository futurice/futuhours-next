module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Types as T



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


statGroup : Model -> Element Msg
statGroup model =
    row
        [ spacing 40
        , centerX
        ]
        [ el [] (text "⏲ -20 h")
        , el [] (text "📊 0 %")
        , el [] (text "☀ 6 days")
        ]


avatarDrop : Model -> Element Msg
avatarDrop model =
    el [ alignRight ] (text "\u{1F913}")


topBar : Model -> Element Msg
topBar model =
    row
        [ width fill
        , height <| px 80
        , paddingXY 50 20
        , spacing 20
        , Background.color <| rgb255 0 54 67
        , Font.color <| rgb 1 1 1
        ]
        [ el [ alignLeft ] (text "FutuHours")
        , statGroup model
        , avatarDrop model
        ]


hoursList : Model -> Element Msg
hoursList model =
    row
        [ Background.color <| rgb255 227 236 236
        , width fill
        , height fill
        , scrollbars
        , padding 20
        ]
        [ text "I'm the body " ]


mainLayout : Model -> Element Msg
mainLayout model =
    column
        [ width fill
        , height fill
        ]
        [ topBar model
        , hoursList model
        ]


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family [ Font.sansSerif ] ]
        (mainLayout model)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
