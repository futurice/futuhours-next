module HoursList exposing (hoursList)

import AnySet
import AssocList
import Date
import Dict
import EditEntry exposing (editEntry, getNewDefaultTaskId)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as HA
import Model exposing (Model, isMobile)
import Msg exposing (Msg(..))
import Time exposing (Weekday(..))
import Types as T
import Ui exposing (colors)
import Util


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
                [ width fill
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
        , displayIfDesk <| textElem <| T.entryDescToString entry.description
        ]


entryColumn : Model -> List T.Entry -> Element Msg
entryColumn model entries =
    column
        [ width fill
        , spacing 15
        ]
        (List.map (entryRow model) entries)


editEntryForDay : Model -> T.Day -> T.Entry -> Element Msg
editEntryForDay model day entry =
    editEntry model
        entry
        { hours = \val -> EditEntry entry.day { entry | hours = val }
        , project = \i -> EditEntry day { entry | projectId = i, taskId = getNewDefaultTaskId model i }
        , task = \i -> EditEntry day { entry | taskId = i }
        , desc = \t -> EditEntry day { entry | description = T.Filled t }
        , delete = DeleteEntry day entry.id
        }


dayEdit : Model -> T.Day -> T.HoursDay -> Element Msg
dayEdit model day hoursDay =
    let
        editingControls =
            row
                [ width fill
                , spacing 15
                , Font.size 16
                ]
                [ Ui.roundButton False True colors.white colors.black (AddEntry day) (Ui.faIcon "fa fa-plus")
                , text "Add row"
                , row [ alignRight, spacing 10 ]
                    [ Ui.scButton
                        [ Background.color colors.cancel ]
                        (CloseDay day)
                        "Cancel"
                    , Ui.scButton
                        [ Background.color colors.save, Font.color colors.white ]
                        (SaveDay day hoursDay)
                        "Save"
                    ]
                ]

        filteredEntries =
            hoursDay.entries
                |> List.filter (not << T.isEntryDeleted)

        hours =
            List.map .hours filteredEntries
                |> List.foldl (+) 0
    in
    column
        [ width fill
        , Font.extraLight
        , Border.rounded 5
        , Border.shadow { offset = ( 2, 2 ), size = 1, blur = 3, color = colors.lightGray }
        ]
        [ row
            [ width fill
            , Background.color colors.topBarBackground
            , Font.color colors.white
            , Font.size 16
            , paddingXY 20 25
            , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
            , Event.onClick <| CloseDay day
            , pointer
            ]
            [ el [ alignLeft, centerY ] (text <| Util.formatDate day)
            , el [ alignRight, centerY ] (text <| String.fromFloat hours ++ " h")
            ]
        , column
            [ width fill
            , Background.color colors.white
            , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
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

        isToday =
            Date.fromIsoString day
                |> Result.map (\d -> Date.compare d <| Date.fromPosix Time.utc model.today)
                |> Result.map ((==) EQ)
                |> Result.withDefault False

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
                False
                colors.topBarBackground
                colors.white
                (OpenDay day hoursDay)
                (Ui.faIcon "fa fa-plus")
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
                , Border.rounded 5
                , Border.shadow { offset = ( 2, 2 ), size = 1, blur = 3, color = colors.lightGray }
                , Background.color backgroundColor
                , Event.onClick <| OpenDay day hoursDay
                , pointer
                ]
                [ row [ paddingXY 5 10, width fill ]
                    [ el 
                        [ Font.alignLeft
                        , alignTop, width (px 100)
                        , if isToday then Font.bold else Font.medium 
                        ] 
                        (text (Util.formatDate day))
                    , case hoursDay.type_ of
                        T.Holiday name ->
                            if hoursDay.hours == 0 then
                                el [ Font.alignLeft, width fill ] (text name)
                            else
                                entryColumn model hoursDay.entries
                        
                        T.Weekend ->
                            entryColumn model hoursDay.entries

                        _ ->
                            if List.isEmpty hoursDay.entries then
                                el [ Font.color colors.warningRed ] (text " Entries missing")
                            else
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
                , paragraph [] [ text <| String.fromInt <| round hoursMonth.utilizationRate, text "%" ]
                ]
            )
        ]


editEntryForWeek : Model -> T.Entry -> Element Msg
editEntryForWeek model entry =
    editEntry model
        entry
        { hours = \hrs -> EditWeekEntry { entry | hours = hrs }
        , project = \id -> EditWeekEntry { entry | projectId = id, taskId = getNewDefaultTaskId model id }
        , task = \id -> EditWeekEntry { entry | taskId = id }
        , desc = \desc -> EditWeekEntry { entry | description = T.Filled desc }
        , delete = DeleteWeekEntry entry.id
        }


weekEdit : Model -> T.EditingWeek -> Element Msg
weekEdit model ewk =
    let
        dayButton day =
            let
                isOn =
                    AnySet.member day ewk.days

                bkgColor =
                    if isOn then
                        colors.save

                    else
                        colors.bodyBackground

                txtColor =
                    if isOn then
                        colors.white

                    else
                        colors.black

                msg =
                    EditWeek <| { ewk | days = AnySet.toggle day ewk.days }

                label =
                    el [ Font.size 12 ] <| text <| Util.toEnglishWeekday day
            in
            Ui.roundButton False False bkgColor txtColor msg label

        dayButtons =
            row [ paddingXY 25 15, spacing 10 ] (List.map dayButton [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ])
    in
    column
        [ width fill
        , Background.color colors.white
        , Border.rounded 5
        , Border.shadow { offset = ( 2, 2 ), size = 1, blur = 3, color = colors.lightGray }
        ]
        [ row
            [ width fill
            , Background.color colors.topBarBackground
            , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
            , Font.color colors.white
            , Font.size 16
            , paddingXY 20 15
            ]
            [ el [ alignLeft, centerY ] (text <| (++) "Week " <| String.fromInt <| .weekNum ewk.week)
            , row
                [ alignRight, centerY, spacing 10 ]
                [ Ui.scButton
                    [ Background.color colors.cancel
                    , Font.color colors.black
                    ]
                    CloseWeek
                    "Cancel"
                , Ui.scButton
                    [ Background.color colors.save
                    , Font.color colors.white
                    ]
                    SaveWeek
                    "Apply"
                ]
            ]
        , dayButtons
        , column [ width fill, paddingXY 25 0, spacing 15 ] <| List.map (editEntryForWeek model) ewk.entries
        , row
            [ width fill, padding 25, spacing 15, Font.size 16 ]
            [ Ui.roundButton False True colors.white colors.black AddWeekEntry (Ui.faIcon "fa fa-plus")
            , text "Add row"
            ]
        ]


weekHeader : Model -> T.Week -> Element Msg
weekHeader model wk =
    let
        days =
            model.allDays
                |> Dict.toList
                |> List.sortBy (\( k, _ ) -> T.dayToMillis k)
                |> List.reverse

        daysForWeek =
            days
                |> List.filter (\( d, _ ) -> wk == T.dayToWeek d)
                |> List.map Tuple.second

        weekDisplay =
            row
                [ width fill, paddingXY 20 0, spacing 15 ]
                [ el [] (text <| "Week " ++ String.fromInt wk.weekNum)
                , Input.button [ Font.underline, Font.size 14 ] { onPress = Just <| OpenWeek wk, label = text "Add a whole week" }
                , row [ alignRight ]
                    [ text <| String.fromFloat <| List.foldl (+) 0 <| List.map .hours daysForWeek
                    , text " h"
                    ]
                ]
    in
    case model.editingWeek of
        Just ewk ->
            if ewk.week == wk then
                weekEdit model ewk

            else
                weekDisplay

        Nothing ->
            weekDisplay


dayElements : Model -> List (Element Msg)
dayElements model =
    let
        makeElem ( d, hd ) =
            { month = T.getMonthNumber d, week = T.dayToWeek d, day = d, elem = dayRow model d hd }

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
                if AssocList.member w dict then
                    AssocList.update w (Maybe.map ((++) [ el ])) dict

                else
                    AssocList.insert w [ el ] dict
            )
            AssocList.empty
        |> AssocList.toList
        |> List.sortBy (.weekNum << Tuple.first)
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
    el 
        [ scrollbarY
        , width fill
        , height fill 
        , htmlAttribute <| HA.style "overflow-y" "scroll"
        , htmlAttribute <| HA.style "-webkit-overflow-scrolling" "touch"
        ] <|
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
                    [ Ui.waiting ]

                _ ->
                    loadMoreButton LoadMoreNext
                        :: dayElements model
                        ++ [ el [ paddingXY 0 20, centerX ] <| loadMoreButton LoadMorePrevious ]
            )
