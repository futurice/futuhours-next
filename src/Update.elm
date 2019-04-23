module Update exposing (update)

import AnySet
import Api
import AssocList
import Dict
import Element
import Iso8601 as Iso
import Model exposing (Model)
import Time exposing (Weekday(..))
import Time.Extra as TE
import Types as T exposing (Msg(..))
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
                    case Api.updateHoursDay hoursDay of
                        [] ->
                            ( { model | hasError = Just "Saved day had no hours entries" }, Cmd.none )

                        s :: saves ->
                            ( { model
                                | editingHours = Dict.remove day model.editingHours
                                , saveQueue = saves
                                , isLoading = True
                              }
                            , s
                            )

                OpenWeek wk ->
                    let
                        days =
                            AnySet.fromList [ Mon, Tue, Wed, Thu, Fri ]

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
                                    if List.isEmpty model.saveQueue then False else True
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
