module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style, class)
import Element exposing (..)
import Element.Events as Event
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Types as T



---- MODEL ----


type alias Model =
    { isMenuOpen : Bool }


init : ( Model, Cmd Msg )
init =
    ( { isMenuOpen = False }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | ToggleMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )
    
        NoOp ->
            ( model, Cmd.none )



---- VIEW ----

faIcon : String -> Element Msg
faIcon c =
    html <| Html.i [ class c ] []


statGroup : Model -> Element Msg
statGroup model =
    row
        [ spacing 40
        , centerX
        , Font.color <| rgb255 133 178 190
        ]
        [ row 
            [] 
            [ faIcon "far fa-clock"
            , (text " -20 h") 
            ]
        , row 
            [] 
            [ faIcon "far fa-chart-bar"
            , (text " 0 %") 
            ]
        , row 
            [] 
            [ faIcon "far fa-sun"
            , (text " 6 days") 
            ]
        ]


avatarDrop : Model -> Element Msg
avatarDrop model =
    row 
        [ Event.onClick ToggleMenu
        , Font.color <| rgb 133 178 190
        , spacing 10
        ] 
        [ image 
            [ alignRight
            , width <| px 40
            , height <| px 40
            , htmlAttribute <| style "clip-path" "circle(20px at center)"
            ] 
            { src = "https://proxy.duckduckgo.com/iu/?u=http%3A%2F%2Fwww.popjam.com%2Fsticker%2FDefault%2Favatar_kitten&f=1" 
            , description = "User profile image" 
            }
        , el 
            [ if model.isMenuOpen then rotate Basics.pi else rotate 0 
            , Font.color <| rgb 1 1 1 ] 
            (html <| Html.i [ class "fa fa-angle-down" ] [])
        ]


topBar : Model -> Element Msg
topBar model =
    row
        [ width fill
        , height <| px 70
        , paddingXY 50 20
        , spacing 20
        , Background.color <| rgb255 0 54 67
        , Font.color <| rgb 1 1 1
        , Font.size 16
        ]
        [ image [ alignLeft ] { src = "futuhours.svg", description = "FutuHours" }
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
