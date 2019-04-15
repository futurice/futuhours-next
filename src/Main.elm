module Main exposing (main)

import Api exposing (..)
import Browser
import Browser.Events
import Date
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import AnySet exposing (AnySet)
import Html exposing (Html)
import Html.Attributes as HA exposing (class, style)
import Http
import Iso8601 as Iso
import Model exposing (Flags, Model, isMobile)
import Task
import Time
import Time.Extra as TE
import Types as T exposing (Msg(..), Workday(..))
import Ui exposing (colors)
import Util



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize WindowResize ]



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.saveQueue of
        x :: xs ->
            ( { model | saveQueue = xs }, x )

        [] ->
            case msg of
                CloseError ->
                    ( { model | hasError = Nothing }, Cmd.none )

                ToggleMenu ->
                    ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )

                LoadMoreNext ->
                    let
                        latestDate =
                            model.hours
                                |> Maybe.andThen T.latestDay
                                |> Maybe.andThen (Iso.toTime >> Result.toMaybe)
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
                                |> Maybe.andThen T.oldestDay
                                |> Maybe.andThen (Iso.toTime >> Result.toMaybe)
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
                            Maybe.andThen T.latestEditableEntry model.hours
                                |> Maybe.map (\e -> { e | id = e.id + 1, day = date, age = T.New })

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

                AddEntry date ->
                    let
                        mostRecentEdit =
                            model.editingHours
                                |> Dict.get date
                                |> Maybe.map .entries
                                |> Maybe.map (List.filter (not << .closed))
                                |> Maybe.map (List.sortBy .id)
                                |> Maybe.map List.reverse
                                |> Maybe.andThen List.head

                        newEntry =
                            Util.maybeOr mostRecentEdit (Maybe.andThen T.latestEditableEntry model.hours)
                                |> Maybe.map (\e -> { e | id = e.id + 1, day = date, age = T.New })
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []

                        insertNew =
                            model.editingHours
                                |> Dict.update date
                                    (Maybe.map (\hd -> { hd | entries = hd.entries ++ newEntry }))
                    in
                    ( { model | editingHours = insertNew }
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

                DeleteEntry date id ->
                    let
                        removeByID xs =
                            List.map
                                (\x ->
                                    if x.id == id then
                                        T.markDeletedEntry x

                                    else
                                        x
                                )
                                xs

                        filteredEntries =
                            model.editingHours
                                |> Dict.update date (Maybe.map (\hd -> { hd | entries = removeByID hd.entries }))
                    in
                    ( { model | editingHours = filteredEntries }
                    , Cmd.none
                    )

                CloseDay date ->
                    ( { model | editingHours = Dict.remove date model.editingHours }
                    , Cmd.none
                    )

                SaveDay day hoursDay ->
                    case updateHoursDay hoursDay of
                        [] ->
                            ( { model | hasError = Just "Saved day had no hours entries" }, Cmd.none )

                        s :: saves ->
                            ( { model
                                | editingHours = Dict.remove day model.editingHours
                                , saveQueue = saves
                              }
                            , s
                            )

                OpenWeek wk -> 
                    let
                        days = AnySet.fromList [Mon, Tue, Wed, Thu, Fri]

                        latest =
                            Maybe.andThen T.latestEditableEntry model.hours
                                |> Maybe.map (\e -> { e | id = e.id + 1, age = T.New })
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []
                    in                                   
                    ( { model | editingWeek = Just <| T.EditingWeek wk days latest }, Cmd.none )

                EditWeek ewk ->
                    ( { model | editingWeek = Just ewk }, Cmd.none )

                AddWeekEntry ->
                    let
                        latest =
                            Maybe.andThen T.latestEditableEntry model.hours                                

                        newEntry = 
                            model.editingWeek
                                |> Maybe.map .entries
                                |> Maybe.map List.reverse
                                |> Maybe.andThen List.head
                                |> (\e -> Util.maybeOr e latest)
                                |> Maybe.map (\e -> { e | id = e.id + 1, age = T.New })
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []

                        newWeek =
                            model.editingWeek
                                |> Maybe.map (\ewk -> { ewk | entries = ewk.entries ++ newEntry })
                    in
                        ( { model | editingWeek = newWeek }, Cmd.none )

                CloseWeek ->
                    ( { model | editingWeek = Nothing }, Cmd.none )

                UserResponse result ->
                    case result of
                        Ok user ->
                            ( { model | user = Just user }, Cmd.none )

                        Err err ->
                            ( { model | hasError = Just <| Util.httpErrToString err }, Cmd.none )

                HandleHoursResponse result ->
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
                                , allDays = T.allDaysAsDict newHours
                              }
                            , Cmd.none
                            )

                        Err err ->
                            ( { model | hasError = Just <| Util.httpErrToString err }, Cmd.none )

                HandleEntryUpdateResponse result ->
                    case result of
                        Ok resp ->
                            let
                                newHours =
                                    model.hours
                                        |> Maybe.map (T.mergeHoursResponse resp.hours)

                                newDays =
                                    Maybe.map T.allDaysAsDict newHours
                                        |> Maybe.withDefault Dict.empty
                            in
                            ( { model | hours = newHours, user = Just resp.user, allDays = newDays }, Cmd.none )

                        Err err ->
                            ( { model | hasError = Just <| Util.httpErrToString err }, Cmd.none )

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
            ++ [ newTabLink [ paddingXY 40 0 ] { url = "https://online.planmill.com/futurice/", label = text "Planmill" }
               , newTabLink [ paddingXY 40 0 ] { url = "https://confluence.futurice.com/pages/viewpage.action?pageId=43321030", label = text "Help" }
               , newTabLink [ paddingXY 40 0 ] { url = "https://hours-api.app.futurice.com/debug/users", label = text "Debug: users" }
               , link [ paddingXY 40 0 ] { url = "https://login.futurice.com/?logout=true", label = text "Logout" }
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
            el
                [ width (px 180)
                ]
                (html <| Html.div [ HA.class "truncate" ] [ Html.text t ])
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


type alias EntryHandlers =
    { hours : Float -> Msg
    , project : Int -> Msg 
    , task : Int -> Msg 
    , desc : String -> Msg 
    , delete : Msg
    }


editEntry : Model -> T.Entry -> EntryHandlers -> Element Msg
editEntry model entry handlers =
    let
        latestEntry =
            Maybe.andThen T.latestEditableEntry model.hours
                |> Maybe.withDefault entry

        reportableProjects =
            model.hours
                |> Maybe.map .reportableProjects
                |> Maybe.withDefault []

        reportableProjectNames =
            reportableProjects
                |> List.map (\p -> ( p.id, p.name ))
                |> Dict.fromList

        allProjectNames =
            Maybe.withDefault Dict.empty model.projectNames

        reportableTaskNames =
            reportableProjects
                |> List.filter (\p -> p.id == entry.projectId)
                |> List.concatMap .tasks
                |> List.map (\t -> ( t.id, t.name ))
                |> Dict.fromList

        allTaskNames =
            Maybe.withDefault Dict.empty model.taskNames

        disabled =
            not <| List.member entry.projectId <| List.map .id reportableProjects

        projectNames =
            if disabled then
                allProjectNames

            else
                reportableProjectNames

        taskNames =
            if disabled then
                allTaskNames

            else
                reportableTaskNames

        latestProjectId =
            if disabled then
                entry.projectId

            else
                latestEntry.projectId

        latestTaskId =
            if disabled then
                entry.taskId

            else
                latestEntry.taskId

        minusButton =
            Ui.roundButton disabled colors.white colors.black handlers.delete (text "-")
    in
    (if isMobile model.window then
        column

     else
        row
    )
        [ width fill
        , spacing 10
        ]
        [ if isMobile model.window then
            el [ alignRight ] minusButton

          else
            none
        , el
            [ width
                (if isMobile model.window then
                    fill

                 else
                    px 75
                )
            ]
            (Ui.numberDropdown disabled handlers.hours entry)
        , Ui.dropdown disabled handlers.project latestProjectId entry.projectId projectNames
        , Ui.dropdown disabled handlers.task latestTaskId entry.taskId taskNames
        , Input.text
            [ Border.width 1
            , Border.rounded 5
            , Border.color
                (if disabled then
                    colors.lightGray

                 else
                    colors.black
                )
            , Font.size 16
            , padding 10
            , htmlAttribute <| HA.disabled disabled
            ]
            { onChange = handlers.desc
            , text = entry.description
            , placeholder = Nothing
            , label = Input.labelHidden "description"
            }
        , if isMobile model.window then
            none

          else
            minusButton
        , if isMobile model.window then
            html <| Html.hr [ HA.style "width" "100%" ] []

          else
            none
        ]


editEntryForDay : Model -> T.Day -> T.Entry -> Element Msg
editEntryForDay model day entry =
    editEntry model entry
        { hours = \val -> EditEntry entry.day { entry | hours = val }
        , project = \i -> EditEntry day { entry | projectId = i }
        , task = \i -> EditEntry day { entry | taskId = i }
        , desc = \t -> EditEntry day { entry | description = t }
        , delete = DeleteEntry day entry.id
        }


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
                [ Ui.roundButton False colors.white colors.black (AddEntry day) (text "+")
                , text "Add row"
                , row [ alignRight, spacing 10 ]
                    [ scButton
                        [ Background.color colors.holidayGray ]
                        (CloseDay day)
                        "Cancel"
                    , scButton
                        [ Background.color colors.topBarBackground, Font.color colors.white ]
                        (SaveDay day hoursDay)
                        "Save"
                    ]
                ]

        filteredEntries =
            hoursDay.entries
                |> List.filter (not << T.isEntryDeleted)
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
            (List.map (editEntryForDay model day) filteredEntries ++ [ editingControls ])
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
                False
                colors.topBarBackground
                colors.white
                (OpenDay day hoursDay)
                (text "+")
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


editEntryForWeek : Model -> T.Entry -> Element Msg
editEntryForWeek model entry =
    editEntry model entry
        { hours = \hrs -> EditWeekEntry entry.id { entry | hours = hrs }
        , project = \id -> EditWeekEntry entry.id { entry | projectId = id }
        , task = \id -> EditWeekEntry entry.id { entry | taskId = id }
        , desc = \desc -> EditWeekEntry entry.id { entry | description = desc }
        , delete = DeleteWeekEntry entry.id
        }


weekEdit : Model -> T.EditingWeek -> Element Msg
weekEdit model ewk =
    let
        dayButton day =
            let
                isOn = AnySet.member day ewk.days
                bkgColor = if isOn then colors.darkText else colors.bodyBackground
                txtColor = if isOn then colors.white else colors.black
                msg = EditWeek <| { ewk | days = AnySet.toggle day ewk.days }
                label = el [ Font.size 12 ] <| text <| T.workdayToString day
            in            
            Ui.roundButton False bkgColor txtColor msg label

        dayButtons =
            row [ paddingXY 25 15, spacing 10 ] (List.map dayButton [Mon, Tue, Wed, Thu, Fri]  )
    in    
    column
        [ width fill 
        , Background.color colors.white
        , Border.shadow { offset = ( 2, 2 ), size = 1, blur = 3, color = colors.lightGray }
        ]
        [ row 
            [ width fill
            , Background.color colors.topBarBackground
            , Font.color colors.white
            , Font.size 16
            , paddingXY 20 15
            ]
            [ el [ alignLeft, centerY ] (text <| (++) "Week " <| String.fromInt ewk.week) 
            , row 
                [ alignRight, centerY, spacing 10 ]
                [ Ui.scButton
                    [ Background.color colors.holidayGray
                    , Font.color colors.black 
                    ]
                    CloseWeek
                    "Cancel"
                , Ui.scButton
                    [ Background.color colors.white
                    , Font.color colors.black 
                    ]
                    NoOp
                    "Apply"
                ]
            ]
        , dayButtons
        , column [ width fill, paddingXY 25 0 ] <| List.map (editEntryForWeek model) ewk.entries
        , row 
            [ width fill, padding 25, spacing 15, Font.size 16 ] 
            [ Ui.roundButton False colors.white colors.black AddWeekEntry (text "+")
            , text "Add row" 
            ]
        ]


weekHeader : Model -> Int -> Element Msg
weekHeader model wk =
    let
        days =
            model.allDays
                |> Dict.toList
                |> List.sortBy (\( k, _ ) -> T.dayToMillis k)
                |> List.reverse

        daysForWeek =
            days
                |> List.filter (\( d, _ ) -> wk == T.getWeekNumber d)
                |> List.map Tuple.second

        weekDisplay =
            row 
                [ width fill, paddingXY 20 0, spacing 15 ]
                [ el [] (text <| "Week " ++ String.fromInt wk)
                , Input.button [ Font.underline, Font.size 14 ] { onPress = Just <| OpenWeek wk, label = text "Add a whole week" }
                , row [ alignRight ]
                    [ text <| String.fromFloat <| List.foldl (+) 0 <| List.map .hours daysForWeek
                    , text " h"
                    ]
                ]
    in
    case model.editingWeek of
        Just ewk ->
            if ewk.week == wk then weekEdit model ewk else weekDisplay
    
        Nothing ->
            weekDisplay


dayElements : Model -> List (Element Msg)
dayElements model =
    let
        makeElem ( d, hd ) =
            { month = T.getMonthNumber d, week = T.getWeekNumber d, day = d, elem = dayRow model d hd }

        getHoursMonth e =
            model.hours
                |> Maybe.map .months
                |> Maybe.withDefault Dict.empty
                |> Dict.get (String.left 7 e.day)
                |> Maybe.withDefault { hours = 0, capacity = 0, utilizationRate = 0.0, days = Dict.empty }

        mkMonthHeader e =
            monthHeader model (String.left 7 e.day) (getHoursMonth e)

        isLastDay e =
            getHoursMonth e
                |> .days
                |> Dict.keys
                |> List.sortBy T.dayToMillis
                |> List.reverse
                |> List.head
                |> Maybe.map ((==) e.day)
                |> Maybe.withDefault False
    in
    model.allDays
        |> Dict.toList
        |> List.sortBy (\( d, _ ) -> T.dayToMillis d)
        |> List.reverse
        |> List.map makeElem
        |> List.map (\e -> ( e.week, e ))
        |> List.foldl
            (\( w, el ) dict ->
                if Dict.member w dict then
                    Dict.update w (Maybe.map ((++) [ el ])) dict

                else
                    Dict.insert w [ el ] dict
            )
            Dict.empty
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.reverse
        |> List.map (\( wk, ds ) -> ( wk, ds |> List.sortBy (\d -> T.dayToMillis d.day) |> List.reverse ))
        |> List.concatMap
            (\( wk, ds ) ->
                let
                    markMonth d =
                        if isLastDay d then
                            [ mkMonthHeader d, d.elem ]

                        else
                            [ d.elem ]
                in
                case ds of
                    [] ->
                        []

                    x :: xs ->
                        if isLastDay x then
                            mkMonthHeader x :: weekHeader model wk :: List.map .elem (x :: xs)

                        else
                            weekHeader model wk :: List.concatMap markMonth (x :: xs)
            )


hoursList : Model -> Element Msg
hoursList model =
    let
        months =
            model.hours
                |> Maybe.map .months
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.sortBy (\( k, _ ) -> T.dayToMillis k)
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
            , width
                (if isMobile model.window then
                    fill

                 else
                    fill |> maximum 900
                )
            , height fill
            , spacing 15
            , if isMobile model.window then
                paddingXY 0 0

              else
                paddingXY 0 20
            ]
            (case months of
                [] ->
                    [ waiting ]

                _ ->
                    loadMoreButton LoadMoreNext
                        :: dayElements model
                        ++ [ el [ paddingXY 0 20, centerX ] <| loadMoreButton LoadMorePrevious ]
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


waiting : Element Msg
waiting =
    el
        [ centerX
        , centerY
        , padding 20
        , Border.solid
        , Border.width 2
        , Border.rounded 10
        , Border.shadow { offset = ( 4, 4 ), size = 1, blur = 5, color = colors.gray }
        , Background.color colors.white
        ]
        (text "Waiting ...")


mainLayout : Model -> Element Msg
mainLayout model =
    let
        errorElem =
            case model.hasError of
                Just err ->
                    errorMsg err

                Nothing ->
                    if List.isEmpty model.saveQueue then
                        none

                    else
                        waiting
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
