module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Http
import Types as T
import Iso8601 as Date
import Task
import Time
import Util


---- API ----


fetchUser : Cmd Msg
fetchUser =
    Http.get
        { url = "/api/v1/user/"
        , expect = Http.expectJson UserResponse T.userDecoder
        }


fetchHours : Time.Posix -> Time.Posix -> Cmd Msg
fetchHours start end =
    let
        startISO = String.left 10 <| Date.fromTime start
        endISO = String.left 10 <| Date.fromTime end
    in
        Http.get
            { url = "/api/v1/hours?start-date=" ++ startISO ++"&end-date=" ++ endISO
            , expect = Http.expectJson HoursResponse T.hoursResponseDecoder 
            }


---- MODEL ----


type alias Model =
    { isMenuOpen : Bool
    , user : Maybe T.User
    , hours : Maybe T.HoursResponse
    , projectNames : Maybe (Dict T.Identifier String)
    , taskNames : Maybe (Dict T.Identifier String)
    , hasError : Maybe String
    }


init : Int -> ( Model, Cmd Msg )
init now =
    let
        today = Time.millisToPosix now
        thirtyDaysAgo = Time.millisToPosix <| now - 2592000000
    in    
    ( { isMenuOpen = False
      , user = Nothing
      , hours = Nothing
      , projectNames = Nothing
      , taskNames = Nothing
      , hasError = Nothing
      }
    , Cmd.batch 
        [ fetchUser
        , fetchHours thirtyDaysAgo today
        ]
    )



---- UPDATE ----


type Msg
    = NoOp
    | CloseError
    | UserResponse (Result Http.Error T.User)
    | HoursResponse (Result Http.Error T.HoursResponse)
    | ToggleMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseError ->
            ( { model | hasError = Nothing }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        UserResponse result ->
            case result of
                Ok user ->
                    ( { model | user = Just user }, Cmd.none )

                Err err ->
                    ( { model | hasError = Just <| Debug.toString err }, Cmd.none )

        HoursResponse result ->
            case result of 
                Ok hours ->
                    ( { model | hours = Just hours }, Cmd.none )
                
                Err err -> 
                    ( { model | hasError = Just <| Debug.toString err }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


colors =
    { topBarBackground = rgb255 0 54 67
    , bodyBackground = rgb255 227 236 236
    , darkText = rgb255 133 178 190
    , white = rgb 1 1 1
    , gray = rgb 0.5 0.5 0.5
    , lightGray = rgb 0.75 0.75 0.75
    , black = rgb 0 0 0
    }


faIcon : String -> Element Msg
faIcon c =
    html <| Html.i [ class c ] []


statGroup : Model -> Element Msg
statGroup model =
    let
        user =
            Maybe.withDefault T.emptyUser model.user

        statElement icon value label =
            row [ spacing 10 ]
                [ el [] (faIcon icon)
                , text <| String.fromFloat value
                , text label
                ]
    in
    row
        [ spacing 40
        , centerX
        , Font.color colors.darkText
        , Font.size 18
        ]
        [ statElement "far fa-clock" user.balance "h"
        , text "|"
        , statElement "far fa-chart-bar" user.utilizationRate "%"
        , text "|"
        , statElement "far fa-sun" user.holidaysLeft "days"
        ]


avatarDrop : Model -> Element Msg
avatarDrop model =
    let
        img =
            case model.user of
                Just user ->
                    user.profilePicture

                Nothing ->
                    ""
    in
    row
        [ Event.onClick ToggleMenu
        , Font.color colors.darkText
        , spacing 10
        ]
        [ image
            [ alignRight
            , width <| px 40
            , height <| px 40
            , htmlAttribute <| style "clip-path" "circle(20px at center)"
            ]
            { src = img
            , description = "User profile image"
            }
        , el
            [ if model.isMenuOpen then
                rotate Basics.pi

              else
                rotate 0
            , Font.color colors.white
            ]
            (faIcon "fa fa-angle-down")
        ]


profileDropdown : Model -> Element Msg
profileDropdown model =
    let
        name =
            case model.user of
                Just user ->
                    user.firstName

                Nothing ->
                    "Noname"

        itemElement attrs elem =
            el ([ paddingXY 40 0 ] ++ attrs) elem
    in
    column
        [ alignRight
        , paddingXY 0 30
        , spacing 20
        , moveLeft 40
        , Font.color colors.white
        , Font.light
        , Font.size 16
        , Background.color colors.topBarBackground
        ]
        ([ itemElement [ Font.color colors.darkText ] (text name)
         , el [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }, width fill ] none
         ]
            ++ List.map (itemElement [])
                [ newTabLink [] { url = "https://online.planmill.com/futurice/", label = text "Planmill" }
                , newTabLink [] { url = "https://confluence.futurice.com/pages/viewpage.action?pageId=43321030", label = text "Help" }
                , newTabLink [] { url = "https://hours-api.app.futurice.com/debug/users", label = text "Debug: users" }
                , link [] { url = "https://login.futurice.com/?logout=true", label = text "Logout" }
                ]
        )


topBar : Model -> Element Msg
topBar model =
    let
        dropdown =
            if model.isMenuOpen then
                profileDropdown model

            else
                none
    in
    row
        [ width fill
        , height <| px 70
        , paddingXY 50 20
        , spacing 20
        , Background.color colors.topBarBackground
        , Font.color colors.white
        , Font.size 16
        , below dropdown
        ]
        [ image [ alignLeft ] { src = "futuhours.svg", description = "FutuHours" }
        , statGroup model
        , avatarDrop model
        ]


entryRow : T.Entry -> Element Msg
entryRow entry =
    row
        [ spacing 75
        , width fill
        , Font.color colors.gray
        ]
        [ text <| String.fromFloat entry.hours
        , text <| String.fromInt entry.projectId
        , text <| String.fromInt entry.taskId
        , text entry.description
        ]


entryColumn : Model -> List T.Entry -> Element Msg
entryColumn model entries =
    column
        [ centerX
        , width fill
        ]
        (List.map (entryRow) entries)


dayRow : Model -> T.Day -> T.HoursDay -> Element Msg
dayRow model day hoursDay =
    row
        [ width fill
        , paddingXY 20 25
        , spacing 75
        , Font.size 16
        , Border.shadow { offset = (2, 2), size = 1, blur = 3, color = colors.lightGray }
        , Background.color colors.white
        ]
        [ text (Util.formatDate day)
        , entryColumn model hoursDay.entries
        , el [ alignRight ] (text <| String.fromFloat hoursDay.hours) 
        ]


monthHeader : T.Month -> T.HoursMonth -> Element Msg
monthHeader month hoursMonth =
    row
        [ width fill
        , padding 20
        , Font.size 24
        , Font.extraLight
        ]
        [ el [] (text <| Util.formatMonth month)
        , el [ alignRight ]
            (row
                [ spacing 10 ]
                [ paragraph []
                    [ text <| String.fromFloat hoursMonth.hours
                    , text "/"
                    , text <| String.fromFloat hoursMonth.capacity
                    ]
                , el [] (faIcon "far fa-chart-bar")
                , paragraph [] [ text <| String.fromFloat hoursMonth.utilizationRate, text "%" ]
                ]
            )
        ]


monthColumn : Model -> T.Month -> T.HoursMonth -> Element Msg
monthColumn model month hoursMonth =
    column
        [ width fill ]
        ([ monthHeader month hoursMonth ]
            ++ (Dict.map (dayRow model) hoursMonth.days |> Dict.values)
        )


hoursList : Model -> Element Msg
hoursList model =
    let 
        months = model.hours
            |> Maybe.map (.months)
            |> Maybe.withDefault Dict.empty
    in
    column
        [ Background.color colors.bodyBackground
        , width fill
        , height fill
        , scrollbars
        , paddingXY 50 20
        ]
        (Dict.map (monthColumn model) months |> Dict.values)


errorMsg : String -> Element Msg
errorMsg error =
    let
        closeButton =
            el [ Event.onClick CloseError, paddingXY 4 3 ] (faIcon "fa fa-times")
    in
    el
        [ centerX
        , centerY
        , padding 20
        , Border.solid
        , Border.width 2
        , Border.rounded 10
        , Border.shadow { offset = ( 4, 4 ), size = 1, blur = 5, color = colors.gray }
        , Background.color colors.white
        , behindContent closeButton
        ]
        (paragraph [] [ text "FutuHours encountered an error: ", text error ])


mainLayout : Model -> Element Msg
mainLayout model =
    let
        errorElem =
            case model.hasError of
                Just err ->
                    errorMsg err

                Nothing ->
                    none
    in
    column
        [ width fill
        , height fill
        , Element.inFront errorElem
        ]
        [ topBar model
        , hoursList model
        ]


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.family [ Font.typeface "Work Sans", Font.sansSerif ] ]
        (mainLayout model)



---- PROGRAM ----


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = \now -> init now
        , update = update
        , subscriptions = always Sub.none
        }
