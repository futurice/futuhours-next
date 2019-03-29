module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Http
import Iso8601 as Date
import Task
import Time
import Types as T
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
        startISO =
            String.left 10 <| Date.fromTime start

        endISO =
            String.left 10 <| Date.fromTime end
    in
    Http.get
        { url = "/api/v1/hours?start-date=" ++ startISO ++ "&end-date=" ++ endISO
        , expect = Http.expectJson HoursResponse T.hoursResponseDecoder
        }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResize ]



---- MODEL ----


type alias Flags =
    { now : Int
    , width : Int
    , height : Int
    }


type alias Window =
    { width : Int
    , height : Int
    , device : Device
    }


isMobile : Window -> Bool
isMobile win =
    let
        device =
            win.device
    in
    case device.class of
        Phone ->
            True

        _ ->
            False


type alias Model =
    { isMenuOpen : Bool
    , user : Maybe T.User
    , hours : Maybe T.HoursResponse
    , projectNames : Maybe (Dict T.Identifier String)
    , taskNames : Maybe (Dict T.Identifier String)
    , hasError : Maybe String
    , today : Time.Posix
    , window : Window
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        today =
            Time.millisToPosix flags.now

        thirtyDaysAgo =
            Time.millisToPosix <| flags.now - 2592000000
    in
    ( { isMenuOpen = False
      , user = Nothing
      , hours = Nothing
      , projectNames = Nothing
      , taskNames = Nothing
      , hasError = Nothing
      , today = today
      , window = { width = flags.width, height = flags.height, device = classifyDevice flags }
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
    | WindowResize Int Int
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
                    ( { model
                        | hours = Just hours
                        , projectNames = Just <| T.hoursToProjectDict hours
                        , taskNames = Just <| T.hoursToTaskDict hours
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | hasError = Just <| Debug.toString err }, Cmd.none )

        WindowResize width height ->
            let
                newWindow =
                    { height = height
                    , width = width
                    , device = classifyDevice { height = height, width = width }
                    }
            in
            ( { model | window = newWindow }, Cmd.none )

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

        deskOptions =
            [ spacing 40
            , centerX
            , Font.color colors.darkText
            , Font.size 18
            ]

        mobileOptions =
            [ spacing 20
            , centerX 
            , Font.color colors.darkText 
            , Font.size 16
            ]
    in
    row
        (if isMobile model.window then mobileOptions else deskOptions)
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
        , (if isMobile model.window then moveLeft 0 else moveLeft 40)
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

        commonOptions =
            [ width fill 
            , spacing 20
            , Background.color colors.topBarBackground
            , Font.color colors.white
            , below dropdown
            ]

        deskOptions = 
            [ height <| px 70
            , paddingXY 50 20
            , Font.size 16
            ] ++ commonOptions

        mobileOptions = 
            [ paddingXY 20 15
            , spacing 20
            , Font.size 12
            ] ++ commonOptions

        futuLogo = image [ alignLeft ] { src = "futuhours.svg", description = "FutuHours" }
    in
    if (isMobile model.window) then 
        column
            mobileOptions
            [ row [ width fill ] 
                [ futuLogo
                , el [ alignRight ] (avatarDrop model)
                ]
            , statGroup model
            ]
    else 
        row
            deskOptions
            [ futuLogo
            , statGroup model
            , avatarDrop model
            ]


entryRow : Model -> T.Entry -> Element Msg
entryRow model entry =
    let
        projectName =
            model.projectNames
                |> Maybe.andThen (\names -> Dict.get entry.projectId names)
                |> Maybe.withDefault "PROJECT NOT FOUND"

        taskName =
            model.taskNames
                |> Maybe.andThen (\names -> Dict.get entry.taskId names)
                |> Maybe.withDefault "TASK NOT FOUND"

        displayIfDesk el =
            if isMobile model.window then Element.none else el
    in
    row
        [ spacing (if isMobile model.window then 10 else 75)
        , width fill
        , Font.color colors.gray
        ]
        [ text <| String.fromFloat entry.hours
        , text projectName
        , displayIfDesk (text taskName)
        , displayIfDesk (text entry.description)
        ]


entryColumn : Model -> List T.Entry -> Element Msg
entryColumn model entries =
    column
        [ centerX
        , width fill
        ]
        (List.map (entryRow model) entries)


dayRow : Model -> T.Day -> T.HoursDay -> Element Msg
dayRow model day hoursDay =
    row
        [ width fill
        , paddingXY 20 25
        , spacing (if isMobile model.window then 10 else 75)
        , Font.size 16
        , Border.shadow { offset = ( 2, 2 ), size = 1, blur = 3, color = colors.lightGray }
        , Background.color colors.white
        ]
        [ el [ width <| px 50 ] (text (Util.formatDate day))
        , el [ centerX ] (entryColumn model hoursDay.entries)
        , el [ alignRight ] (text <| String.fromFloat hoursDay.hours)
        ]


monthHeader : Model -> T.Month -> T.HoursMonth -> Element Msg
monthHeader model month hoursMonth =
    row
        [ width fill
        , padding 20
        , Font.size (if isMobile model.window then 20 else 24)
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
    let
        days =
            hoursMonth.days
                |> Dict.toList
                |> List.sortBy (\( k, _ ) -> Time.posixToMillis <| Result.withDefault (Time.millisToPosix 0) <| Date.toTime k)
                |> List.reverse
    in
    column
        [ width fill ]
        ([ monthHeader model month hoursMonth ]
            ++ List.map (\( d, hd ) -> dayRow model d hd) days
        )


hoursList : Model -> Element Msg
hoursList model =
    let
        months =
            model.hours
                |> Maybe.map .months
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.sortBy (\( k, _ ) -> Time.posixToMillis <| Result.withDefault (Time.millisToPosix 0) <| Date.toTime k)
                |> List.reverse
    in
    column
        [ Background.color colors.bodyBackground
        , width fill
        , height fill
        , scrollbars
        , (if isMobile model.window then paddingXY 0 0 else paddingXY 50 20)
        ]
        (List.map (\( m, hm ) -> monthColumn model m hm) months)


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


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
