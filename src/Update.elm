module Update exposing (update)

import AnySet
import Api
import AssocList
import Date
import Dict
import Element
import Iso8601 as Iso
import Maybe.Extra as MX
import Model exposing (Model)
import Msg exposing (Msg(..))
import Time exposing (Weekday(..))
import Time.Extra as TE
import Types as T
import Util


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
                    , Api.fetchHours model.today nextThirtyDays
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
                    , Api.fetchHours oldestMinus30 oldestDate
                    )

                OpenDay date hoursDay ->
                    let
                        defaultHours =
                            model.hours
                                |> Maybe.map .defaultWorkHours
                                |> Maybe.withDefault 7.5

                        projects =
                            model.hours
                                |> Maybe.map .reportableProjects
                                |> Maybe.withDefault []
                                |> List.filter (\rp -> String.contains "Absence" rp.name)

                        latest =
                            Maybe.andThen T.latestEditableEntry model.hours
                                |> Maybe.map (\e -> { e | id = e.id + 1, day = date, age = T.New, description = T.filledToDefault e.description })
                                |> Maybe.withDefault (T.makeEntry date defaultHours projects)

                        addEntryIfEmpty =
                            if List.isEmpty hoursDay.entries then
                                { hoursDay
                                    | entries =
                                        List.singleton latest
                                    , hours =
                                        latest.hours
                                }

                            else
                                hoursDay
                    in
                    ( { model | editingHours = Dict.insert date addEntryIfEmpty model.editingHours }
                    , Cmd.none
                    )

                AddEntry date ->
                    let
                        defaultHours =
                            model.hours
                                |> Maybe.map .defaultWorkHours
                                |> Maybe.withDefault 7.5

                        projects =
                            model.hours
                                |> Maybe.map .reportableProjects
                                |> Maybe.withDefault []
                                |> List.filter (\rp -> not <| String.contains "Absence" rp.name)
                                
                        mostRecentEdit =
                            model.editingHours
                                |> Dict.get date
                                |> Maybe.map .entries
                                |> Maybe.map (List.sortBy .id)
                                |> Maybe.map List.reverse
                                |> Maybe.andThen List.head
                                |> MX.filter (T.entryEditable model.hours)

                        newEntry =
                            Util.maybeOr mostRecentEdit (Maybe.andThen T.latestEditableEntry model.hours)
                                |> Maybe.map (\e -> { e | id = e.id + 1, day = date, age = T.New, description = T.filledToDefault e.description })
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault [T.makeEntry date defaultHours projects]

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
                    let
                        addHours hours =
                            { hours
                                | months =
                                    hours.months
                                        |> Dict.map
                                            (\mo hmo ->
                                                if mo == String.left 7 day then
                                                    { hmo | days = Dict.insert day hoursDay hmo.days }

                                                else
                                                    hmo
                                            )
                            }

                        tempHours =
                            Maybe.map addHours model.hours
                    in
                    case Api.updateHoursDay model.hours hoursDay of
                        [] ->
                            ( { model | hasError = Just "Saved day had no hours entries" }, Cmd.none )

                        s :: saves ->
                            ( { model
                                | editingHours = Dict.remove day model.editingHours
                                , hours = tempHours
                                , saveQueue = saves
                                , isLoading = True
                              }
                            , s
                            )

                OpenWeek wk ->
                    let
                        date day =
                            Date.fromWeekDate wk.year wk.weekNum day
                                |> Date.toIsoString
                                
                        hasHours day =
                            Dict.get (date day) model.allDays
                                |> Maybe.map .entries
                                |> Maybe.map (not << List.isEmpty)
                                |> Maybe.withDefault False
                        
                        isHoliday day =
                            Dict.get (date day) model.allDays
                                |> Maybe.map (\d -> case d.type_ of
                                    T.Holiday _ ->
                                        True
                                
                                    _ ->
                                        False)
                                |> Maybe.withDefault False

                        days =
                            AnySet.fromList <| List.filter (\d -> not <| hasHours d || isHoliday d) [ Mon, Tue, Wed, Thu, Fri ]

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
                                |> MX.filter (T.entryEditable model.hours)
                                |> (\e -> Util.maybeOr e latest)
                                |> Maybe.map (\e -> { e | id = e.id + 1, age = T.New, description = T.filledToDefault e.description })
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []

                        newWeek =
                            model.editingWeek
                                |> Maybe.map (\ewk -> { ewk | entries = ewk.entries ++ newEntry })
                    in
                    ( { model | editingWeek = newWeek }, Cmd.none )

                EditWeekEntry entry ->
                    let
                        newEntries =
                            model.editingWeek
                                |> Maybe.map .entries
                                |> Maybe.withDefault []
                                |> List.map
                                    (\e ->
                                        if e.id == entry.id then
                                            entry

                                        else
                                            e
                                    )

                        newWeek =
                            model.editingWeek
                                |> Maybe.map (\ewk -> { ewk | entries = newEntries })
                    in
                    ( { model | editingWeek = newWeek }, Cmd.none )

                DeleteWeekEntry id ->
                    let
                        newEntries =
                            model.editingWeek
                                |> Maybe.map .entries
                                |> Maybe.withDefault []
                                |> List.filter (\e -> e.id /= id)

                        newWeek =
                            model.editingWeek
                                |> Maybe.map (\ewk -> { ewk | entries = newEntries })
                    in
                    ( { model | editingWeek = newWeek }, Cmd.none )

                SaveWeek ->
                    case model.editingWeek of
                        Nothing ->
                            ( { model | hasError = Just "Edting week is empty" }, Cmd.none )

                        Just ewk ->
                            ( { model | editingWeek = Nothing, isLoading = True }, Cmd.batch (Api.updateWeek ewk) )

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
                                , isLoading = False
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

                                isLoading =
                                    if List.isEmpty model.saveQueue then
                                        False

                                    else
                                        True
                            in
                            ( { model | hours = newHours, user = Just resp.user, allDays = newDays, isLoading = isLoading }, Cmd.none )

                        Err err ->
                            ( { model | hasError = Just <| Util.httpErrToString err }, Cmd.none )

                WindowResize width height ->
                    let
                        newWindow =
                            { height = height
                            , width = width
                            , device = Element.classifyDevice { height = height, width = width }
                            }
                    in
                    ( { model | window = newWindow }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
