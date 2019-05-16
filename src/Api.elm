module Api exposing (deleteEntry, fetchHours, fetchUser, postNewEntry, putEntryUpdate, rootUrl, updateHoursDay, updateWeek)

import AnySet
import Date
import Http
import Iso8601 as Iso
import Process
import Task
import Time
import Types as T
import Msg exposing (Msg(..))



---- API ----


rootUrl : String
rootUrl =
    "/api/v1"


fetchUser : Cmd Msg
fetchUser =
    Http.get
        { url = rootUrl ++ "/user/"
        , expect = Http.expectJson UserResponse T.userDecoder
        }


fetchHours : Time.Posix -> Time.Posix -> Cmd Msg
fetchHours start end =
    let
        startISO =
            String.left 10 <| Iso.fromTime start

        endISO =
            String.left 10 <| Iso.fromTime end
    in
    Http.get
        { url = rootUrl ++ "/hours?start-date=" ++ startISO ++ "&end-date=" ++ endISO
        , expect = Http.expectJson HandleHoursResponse T.hoursResponseDecoder
        }


postNewEntry : T.Entry -> Cmd Msg
postNewEntry e =
    Http.request
        { method = "POST"
        , headers = []
        , url = rootUrl ++ "/entry"
        , expect = Http.expectJson HandleEntryUpdateResponse T.entryUpdateResponseDecoder
        , body = Http.jsonBody <| T.entryToJsonBody e
        , timeout = Just 3000
        , tracker = Nothing
        }


putEntryUpdate : T.Entry -> Cmd Msg
putEntryUpdate e =
    Http.request
        { method = "PUT"
        , headers = []
        , url = rootUrl ++ "/entry/" ++ String.fromInt e.id
        , body = Http.jsonBody <| T.entryToJsonBody e
        , expect = Http.expectJson HandleEntryUpdateResponse T.entryUpdateResponseDecoder
        , timeout = Just 3000
        , tracker = Nothing
        }


deleteEntry : T.Entry -> Cmd Msg
deleteEntry e =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = rootUrl ++ "/entry/" ++ String.fromInt e.id
        , body = Http.emptyBody
        , expect = Http.expectJson HandleEntryUpdateResponse T.entryUpdateResponseDecoder
        , timeout = Just 3000
        , tracker = Nothing
        }


wait : Cmd Msg
wait =
    Process.sleep 200
        |> Task.perform (\_ -> NoOp)


updateHoursDay : T.HoursDay -> List (Cmd Msg)
updateHoursDay hoursDay =
    let
        whichCmd e =
            case e.age of
                T.New ->
                    postNewEntry e

                T.Old ->
                    putEntryUpdate e

                T.Deleted ->
                    deleteEntry e

                T.DeletedNew ->
                    Cmd.none
    in
    List.map whichCmd hoursDay.entries
        |> List.intersperse wait


updateWeek : T.EditingWeek -> List (Cmd Msg)
updateWeek ewk =
    let
        markEntriesByDay day =
            let
                year =
                    .year ewk.week

                week =
                    .weekNum ewk.week
            in
            ewk.entries
                |> List.map (\e -> { e | day = Date.fromWeekDate year week day |> Date.toIsoString })

        entries =
            ewk.days
                |> AnySet.toList
                |> List.concatMap markEntriesByDay
    in
    List.map postNewEntry entries
