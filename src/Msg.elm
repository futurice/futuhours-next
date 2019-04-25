module Msg exposing (..)

import Http
import Task
import Types exposing (..)


type Msg
    = NoOp
    | CloseError
    | LoadMoreNext
    | LoadMorePrevious
    | OpenDay Day HoursDay
    | AddEntry Day
    | EditEntry Day Entry
    | DeleteEntry Day Identifier
    | CloseDay Day
    | SaveDay Day HoursDay
    | OpenWeek Week
    | AddWeekEntry
    | EditWeek EditingWeek
    | EditWeekEntry Entry
    | DeleteWeekEntry Int
    | SaveWeek
    | CloseWeek
    | UserResponse (Result Http.Error User)
    | HandleHoursResponse (Result Http.Error HoursResponse)
    | HandleEntryUpdateResponse (Result Http.Error EntryUpdateResponse)
    | WindowResize Int Int
    | ToggleMenu


send : Msg -> Cmd Msg
send msg =
    Task.succeed msg
        |> Task.perform identity