module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Http
import Iso8601 as Date
import Task
import Time
import Time.Extra as TE
import Types as T
import Ui exposing (colors)
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
    , editingHours : Dict T.Day T.HoursDay
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        today =
            Time.millisToPosix flags.now

        thirtyDaysAgo =
            flags.now
                |> Time.millisToPosix
                |> TE.add TE.Day -30 Time.utc
    in
    ( { isMenuOpen = False
      , user = Nothing
      , hours = Nothing
      , projectNames = Nothing
      , taskNames = Nothing
      , hasError = Nothing
      , today = today
      , window = { width = flags.width, height = flags.height, device = classifyDevice flags }
      , editingHours = Dict.empty
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
    | LoadMoreNext
    | LoadMorePrevious
    | OpenDay T.Day T.HoursDay
    | EditEntry T.Day T.Entry
    | CloseDay T.Day
    | UserResponse (Result Http.Error T.User)
    | HoursResponse (Result Http.Error T.HoursResponse)
    | WindowResize Int Int
    | ToggleMenu


send : Msg -> Cmd Msg
send msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseError ->
            ( { model | hasError = Nothing }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

        LoadMoreNext ->
            let
                latestDate =
                    model.hours
                        |> Maybe.andThen T.latestEntry
                        |> Maybe.andThen (\e -> e.day |> Date.toTime |> Result.toMaybe)
                        |> Maybe.withDefault model.today

                nextThirtyDays =
                    TE.add TE.Day 30 Time.utc latestDate
            in
            ( model
            , fetchHours model.today nextThirtyDays
            )

        LoadMorePrevious ->
            let
                oldestDate =
                    model.hours
                        |> Maybe.andThen T.oldestEntry
                        |> Maybe.andThen (\e -> e.day |> Date.toTime |> Result.toMaybe)
                        |> Maybe.withDefault model.today

                oldestMinus30 =
                    TE.add TE.Day -30 Time.utc oldestDate
            in
            ( model
            , fetchHours oldestMinus30 oldestDate
            )

        OpenDay date hoursDay ->
            let
                latest =
                    Maybe.andThen T.latestEntry model.hours

                addEntryIfEmpty =
                    if List.isEmpty hoursDay.entries then
                        { hoursDay
                            | entries =
                                Maybe.map List.singleton latest
                                    |> Maybe.withDefault []
                            , hours =
                                Maybe.map .hours latest
                                    |> Maybe.withDefault 0
                        }

                    else
                        hoursDay
            in
            ( { model | editingHours = Dict.insert date addEntryIfEmpty model.editingHours }
            , Cmd.none
            )

        EditEntry date newEntry ->
            let
                updateEntries : Maybe T.HoursDay -> Maybe T.HoursDay
                updateEntries =
                    Maybe.map
                        (\hd ->
                            { hd
                                | entries =
                                    List.map
                                        (\e ->
                                            if e.id == newEntry.id then
                                                newEntry

                                            else
                                                e
                                        )
                                        hd.entries
                            }
                        )
            in
            ( { model
                | editingHours = Dict.update date updateEntries model.editingHours
              }
            , Cmd.none
            )

        CloseDay date ->
            ( { model | editingHours = Dict.remove date model.editingHours }
            , Cmd.none
            )

        UserResponse result ->
            case result of
                Ok user ->
                    ( { model | user = Just user }, Cmd.none )

                Err err ->
                    ( { model | hasError = Just <| Debug.toString err }, Cmd.none )

        HoursResponse result ->
            case result of
                Ok hoursResponse ->
                    let
                        newHours =
                            case model.hours of
                                Just oldHours ->
                                    T.mergeHoursResponse oldHours hoursResponse

                                Nothing ->
                                    hoursResponse
                    in
                    ( { model
                        | hours = Just newHours
                        , projectNames = Just <| T.hoursToProjectDict newHours
                        , taskNames = Just <| T.hoursToTaskDict newHours
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


statGroup : Model -> Element Msg
statGroup model =
    let
        user =
            Maybe.withDefault T.emptyUser model.user

        statElement icon value label =
            row [ spacing 10 ]
                [ el [] (Ui.faIcon icon)
                , text <| String.fromFloat value
                , text label
                ]

        commonOptions =
            [ Font.regular
            , centerX
            , Font.color colors.darkText
            ]

        deskOptions =
            [ spacing 40
            , Font.size 18
            ]
                ++ commonOptions

        mobileOptions =
            [ spacing 20
            , Font.size 16
            ]
                ++ commonOptions
    in
    row
        (if isMobile model.window then
            mobileOptions

         else
            deskOptions
        )
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
            (Ui.faIcon "fa fa-angle-down")
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
        , if isMobile model.window then
            moveLeft 0

          else
            moveLeft (model.window |> .width |> (\x -> (x - 920) // 2) |> toFloat)
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
            , Background.color colors.topBarBackground
            , Font.color colors.white
            , below dropdown
            ]

        deskOptions =
            [ height <| px 70
            , paddingXY 50 20
            , Font.size 16
            ]
                ++ commonOptions

        mobileOptions =
            [ paddingXY 20 15
            , spacing 20
            , Font.size 12
            ]
                ++ commonOptions

        futuLogo =
            image [ alignLeft ] { src = "futuhours.svg", description = "FutuHours" }
    in
    if isMobile model.window then
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
            [ row [ centerX, width (fill |> maximum 900) ]
                [ futuLogo
                , statGroup model
                , avatarDrop model
                ]
            ]


entryRow : Model -> T.Entry -> Element Msg
entryRow model entry =
    let
        projectName =
            model.projectNames
                |> Maybe.andThen (\names -> Dict.get entry.projectId names)
                |> Maybe.withDefault "PROJECT NOT FOUND"
                |> (\n ->
                        if isMobile model.window then
                            String.slice 0 18 n ++ "..."

                        else
                            n
                   )

        taskName =
            model.taskNames
                |> Maybe.andThen (\names -> Dict.get entry.taskId names)
                |> Maybe.withDefault "TASK NOT FOUND"

        displayIfDesk el =
            if isMobile model.window then
                Element.none

            else
                el

        textElem t =
            el [ width (px 180) ] (text t)
    in
    row
        [ spacing
            (if isMobile model.window then
                10

             else
                50
            )
        , width fill
        , Font.color colors.gray
        , Font.alignLeft
        ]
        [ el [ width (px 30), Font.center ] (text <| String.fromFloat entry.hours)
        , textElem projectName
        , displayIfDesk <| textElem taskName
        , displayIfDesk <| textElem entry.description
        ]


entryColumn : Model -> List T.Entry -> Element Msg
entryColumn model entries =
    column
        [ width fill
        , spacing 15
        ]
        (List.map (entryRow model) entries)


editEntry : Model -> T.Day -> T.Entry -> Element Msg
editEntry model day entry =
    row
        [ width fill
        , spacing 10
        ]
        [ Ui.stepper entry.hours NoOp NoOp
        , Ui.dropdown
        , Ui.dropdown
        , Input.text
            [ Border.width 1
            , Border.rounded 5
            , Border.color colors.black
            , Font.size 16
            , padding 10
            ]
            { onChange = \_ -> NoOp
            , text = entry.description
            , placeholder = Nothing
            , label = Input.labelHidden "description"
            }
        , Ui.roundButton colors.white colors.black NoOp "-"
        ]


dayEdit : Model -> T.Day -> T.HoursDay -> Element Msg
dayEdit model day hoursDay =
    let
        scButton attrs msg label =
            Input.button
                ([ Font.size 14, width (px 100), height (px 40), Border.rounded 5 ] ++ attrs)
                { onPress = Just msg, label = text label }

        editingControls =
            row
                [ width fill
                , spacing 15
                , Font.size 16
                ]
                [ Ui.roundButton colors.white colors.black NoOp "+"
                , text "Add row"
                , row [ alignRight, spacing 10 ]
                    [ scButton
                        [ Background.color colors.holidayGray ]
                        (CloseDay day)
                        "Cancel"
                    , scButton
                        [ Background.color colors.topBarBackground, Font.color colors.white ]
                        NoOp
                        "Save"
                    ]
                ]
    in
    column
        [ width fill
        , Font.extraLight
        , Border.shadow { offset = ( 2, 2 ), size = 1, blur = 3, color = colors.lightGray }
        ]
        [ row
            [ width fill
            , Background.color colors.topBarBackground
            , Font.color colors.white
            , Font.size 16
            , paddingXY 20 25
            , Event.onClick <| CloseDay day
            , pointer
            ]
            [ el [ alignLeft, centerY ] (text <| Util.formatDate day)
            , el [ alignRight, centerY ] (text <| String.fromFloat hoursDay.hours ++ " h")
            ]
        , column
            [ width fill
            , Background.color colors.white
            , padding 30
            , spacing 20
            ]
            (List.map (editEntry model day) hoursDay.entries ++ [ editingControls ])
        ]


dayRow : Model -> T.Day -> T.HoursDay -> Element Msg
dayRow model day hoursDay =
    let
        backgroundColor =
            case hoursDay.type_ of
                T.Normal ->
                    colors.white

                T.Holiday _ ->
                    colors.holidayYellow

                T.Weekend ->
                    colors.holidayGray

        hoursElem =
            el [ alignTop, alignRight, Font.medium ] (text <| String.fromFloat hoursDay.hours)

        showButton =
            case hoursDay.type_ of
                T.Normal ->
                    hoursDay.hours == 0

                _ ->
                    False

        openButton =
            Ui.roundButton
                colors.topBarBackground
                colors.white
                (OpenDay day hoursDay)
                "+"
    in
    case Dict.get day model.editingHours of
        Just hd ->
            dayEdit model day hd

        Nothing ->
            row
                [ width fill
                , paddingXY 15 15
                , spaceEvenly
                , Font.size 16
                , Font.color colors.gray
                , Border.shadow { offset = ( 2, 2 ), size = 1, blur = 3, color = colors.lightGray }
                , Background.color backgroundColor
                , Event.onClick <| OpenDay day hoursDay
                , pointer
                ]
                [ row [ paddingXY 5 10, width fill ]
                    [ el [ Font.alignLeft, alignTop, width (px 100) ] (text (Util.formatDate day))
                    , case hoursDay.type_ of
                        T.Holiday name ->
                            el [ Font.alignLeft, width fill ] (text name)

                        _ ->
                            entryColumn model hoursDay.entries
                    , if hoursDay.hours == 0 then
                        Element.none

                      else
                        hoursElem
                    ]
                , if showButton then
                    openButton

                  else
                    Element.none
                ]


monthHeader : Model -> T.Month -> T.HoursMonth -> Element Msg
monthHeader model month hoursMonth =
    row
        [ width fill
        , paddingEach { left = 20, right = 20, top = 20, bottom = 0 }
        , Font.size
            (if isMobile model.window then
                20

             else
                24
            )
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
                , el [] (Ui.faIcon "far fa-chart-bar")
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
        [ width fill
        , spacing 15
        ]
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

        loadMoreButton msg =
            Input.button
                [ Background.color colors.topBarBackground
                , Font.color colors.white
                , Font.size 12
                , paddingXY 25 15
                , Border.rounded 5
                , centerX
                ]
                { onPress = Just msg, label = text "Load More" }
    in
    el [ scrollbarY, width fill, height fill ] <|
        column
            [ centerX
            , width (fill |> maximum 900)
            , height fill
            , if isMobile model.window then
                paddingXY 0 0

              else
                paddingXY 0 20
            ]
            (case months of
                [] ->
                    [ none ]

                _ ->
                    loadMoreButton LoadMoreNext
                        :: List.map (\( m, hm ) -> monthColumn model m hm) months
                        ++ [ loadMoreButton LoadMorePrevious ]
            )


errorMsg : String -> Element Msg
errorMsg error =
    let
        closeButton =
            el [ Event.onClick CloseError, paddingXY 4 3 ] (Ui.faIcon "fa fa-times")
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
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
