module Types exposing (..)

import AnySet exposing (AnySet)
import Date
import Dict exposing (Dict)
import Http
import Iso8601 as Iso
import Json.Decode as Decode exposing (Decoder, bool, dict, field, float, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Set
import Task
import Time


type alias Week =
    { year : Int
    , weekNum : Int
    }


type alias EditingWeek =
    { week : Week
    , days : AnySet Date.Weekday
    , entries : List Entry
    }


type alias Identifier =
    Int


type alias NDTh =
    Float


type alias NDTd =
    Float


type alias Day =
    String


dayToWeek : Day -> Week
dayToWeek d =
    let
        date =
            Date.fromIsoString d

        weekNum =
            Result.map Date.weekNumber date
                |> Result.withDefault 0

        year =
            Result.map Date.weekYear date
                |> Result.withDefault 1970
    in
    Week year weekNum


getMonthNumber : Day -> Int
getMonthNumber d =
    Date.fromIsoString d
        |> Result.map Date.monthNumber
        |> Result.withDefault 0


getDay : Day -> Int
getDay d =
    Date.fromIsoString d
        |> Result.map Date.day
        |> Result.withDefault 0


dayToMillis : Day -> Int
dayToMillis d =
    Iso.toTime d
        |> Result.map Time.posixToMillis
        |> Result.withDefault 0


type alias Month =
    String


type alias Login =
    String


type alias Project task =
    { id : Identifier
    , name : String
    , tasks : List task
    , closed : Bool
    }


projectDecoder : Decoder task -> Decoder (Project task)
projectDecoder taskDecoder =
    Decode.succeed Project
        |> required "id" int
        |> required "name" string
        |> required "tasks" (list taskDecoder)
        |> required "closed" bool


type alias ReportableTask =
    { id : Identifier
    , name : String
    , closed : Bool
    , hoursRemaining : Maybe NDTh
    }


reportableTaskDecoder : Decoder ReportableTask
reportableTaskDecoder =
    Decode.succeed ReportableTask
        |> required "id" int
        |> required "name" string
        |> required "closed" bool
        |> optional "hoursRemaining" (Decode.maybe float) Nothing


type alias MarkableTask =
    { id : Identifier
    , name : String
    , closed : Bool
    , absence : Bool
    }


markableTaskDecoder : Decoder MarkableTask
markableTaskDecoder =
    Decode.succeed MarkableTask
        |> required "id" int
        |> required "name" string
        |> required "closed" bool
        |> required "absence" bool


type alias User =
    { firstName : String
    , lastName : String
    , balance : NDTh
    , holidaysLeft : NDTd
    , utilizationRate : Float
    , profilePicture : String
    }


emptyUser : User
emptyUser =
    { firstName = ""
    , lastName = ""
    , balance = 0.0
    , holidaysLeft = 0.0
    , utilizationRate = 0.0
    , profilePicture = ""
    }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "firstName" string
        |> required "lastName" string
        |> required "balance" float
        |> required "holidaysLeft" float
        |> required "utilizationRate" float
        |> required "profilePicture" string


type alias HoursResponse =
    { defaultWorkHours : NDTh
    , reportableProjects : List (Project ReportableTask)
    , markedProjects : List (Project MarkableTask)
    , months : Dict Month HoursMonth
    }


hoursResponseDecoder : Decoder HoursResponse
hoursResponseDecoder =
    Decode.succeed HoursResponse
        |> required "defaultWorkHours" float
        |> required "reportableProjects" (list (projectDecoder reportableTaskDecoder))
        |> required "markedProjects" (list (projectDecoder markableTaskDecoder))
        |> required "months" (dict hoursMonthDecoder)


mergeHoursResponse : HoursResponse -> HoursResponse -> HoursResponse
mergeHoursResponse newHours oldHours =
    let
        mergeLists : List (Project a) -> List (Project a) -> List (Project a)
        mergeLists new old =
            AnySet.union (AnySet.fromList new) (AnySet.fromList old)
                |> AnySet.toList
    in
    HoursResponse
        newHours.defaultWorkHours
        (mergeLists newHours.reportableProjects oldHours.reportableProjects)
        (mergeLists newHours.markedProjects oldHours.markedProjects)
        (Dict.merge
            Dict.insert
            (\key a b ->
                Dict.insert key
                    { a | days = Dict.union a.days b.days }
            )
            Dict.insert
            newHours.months
            oldHours.months
            Dict.empty
        )


sanitizeHoursResponse: HoursResponse -> HoursResponse
sanitizeHoursResponse hours =
    let
        cleanProjects =
            hours.reportableProjects
                |> List.filter (not << String.contains "Absence" << .name)
    in
        { hours | reportableProjects = cleanProjects }


hoursToProjectDict : HoursResponse -> Dict Identifier String
hoursToProjectDict hours =
    let
        toDict projects =
            projects
                |> List.map (\t -> ( t.id, t.name ))
                |> Dict.fromList

        reportable =
            toDict hours.reportableProjects

        marked =
            toDict hours.markedProjects
    in
    Dict.union marked reportable


hoursToTaskDict : HoursResponse -> Dict Identifier String
hoursToTaskDict hours =
    let
        toTasks projects =
            List.map .tasks projects
                |> List.foldl (++) []
                |> List.map (\t -> ( t.id, t.name ))

        reportableTasks =
            toTasks hours.reportableProjects

        markedTasks =
            toTasks hours.markedProjects
    in
    (reportableTasks ++ markedTasks)
        |> Dict.fromList


allEntries : HoursResponse -> List Entry
allEntries hours =
    hours.months
        |> Dict.values
        |> List.concatMap (\m -> m.days |> Dict.values)
        |> List.concatMap .entries
        |> List.sortBy (\e -> dayToMillis e.day)


allEntriesAsDict : HoursResponse -> Dict Day (List Entry)
allEntriesAsDict hours =
    let
        blankDict =
            hours.months
                |> Dict.values
                |> List.concatMap (Dict.keys << .days)
                |> List.foldl (\d dic -> Dict.insert d [] dic) Dict.empty

        insertOrAdd e dic =
            if Dict.member e.day dic then
                Dict.update e.day (Maybe.map ((::) e)) dic

            else
                Dict.insert e.day [ e ] dic
    in
    allEntries hours
        |> List.foldl insertOrAdd blankDict


latestEntry : HoursResponse -> Maybe Entry
latestEntry hours =
    allEntries hours
        |> List.reverse
        |> List.head


entryEditable : Maybe HoursResponse -> Entry -> Bool
entryEditable hours e =
    case e.billable of
        Absence ->
            False

        Unknown ->
            False

        _ ->
            Maybe.map (\h -> List.member e.projectId <| List.map .id h.reportableProjects) hours
                |> Maybe.withDefault False


latestEditableEntries : HoursResponse -> List Entry
latestEditableEntries hours =
    allEntries hours
        |> List.filter (entryEditable <| Just hours)
        |> List.reverse


latestEditableEntry : HoursResponse -> Maybe Entry
latestEditableEntry hours =
    latestEditableEntries hours
        |> List.head


oldestEntry : HoursResponse -> Maybe Entry
oldestEntry hours =
    allEntries hours
        |> List.head


allDays : HoursResponse -> List Day
allDays hours =
    hours.months
        |> Dict.values
        |> List.map .days
        |> List.concatMap Dict.keys


allDaysAsDict : HoursResponse -> Dict Day HoursDay
allDaysAsDict hours =
    hours.months
        |> Dict.values
        |> List.map .days
        |> List.foldl Dict.union Dict.empty


latestDay : HoursResponse -> Maybe Day
latestDay hours =
    allDays hours
        |> List.sortBy dayToMillis
        |> List.reverse
        |> List.head


oldestDay : HoursResponse -> Maybe Day
oldestDay hours =
    allDays hours
        |> List.sortBy dayToMillis
        |> List.head


type alias HoursMonth =
    { hours : NDTh
    , capacity : NDTh
    , utilizationRate : Float
    , days : Dict Day HoursDay
    }


hoursMonthDecoder : Decoder HoursMonth
hoursMonthDecoder =
    Decode.succeed HoursMonth
        |> required "hours" float
        |> required "capacity" float
        |> required "utilizationRate" float
        |> required "days" (dict hoursDayDecoder)


type alias HoursDay =
    { type_ : DayType
    , hours : NDTh
    , entries : List Entry
    , closed : Bool
    }


hoursDayDecoder : Decoder HoursDay
hoursDayDecoder =
    Decode.succeed HoursDay
        |> required "type" dayTypeDecoder
        |> required "hours" float
        |> required "entries" (list entryDecoder)
        |> required "closed" bool


type DayType
    = Normal
    | Weekend
    | Holiday String


dayTypeDecoder : Decoder DayType
dayTypeDecoder =
    Decode.oneOf
        [ bool
            |> Decode.andThen
                (\b ->
                    if b then
                        Decode.succeed Weekend

                    else
                        Decode.succeed Normal
                )
        , string |> Decode.andThen (\a -> Decode.succeed (Holiday a))
        ]


type EntryAge
    = Old
    | New
    | Deleted
    | DeletedNew


type EntryDesc
    = Filled String
    | Default String


entryDescToString : EntryDesc -> String
entryDescToString desc =
    case desc of
        Filled str ->
            str
    
        Default str ->
            str


filledToDefault : EntryDesc -> EntryDesc
filledToDefault desc =
    case desc of
        Filled str ->
            Default str
    
        Default str ->
            Default str


type alias Entry =
    { id : Identifier
    , projectId : Identifier
    , taskId : Identifier
    , day : Day
    , description : EntryDesc
    , closed : Bool
    , hours : NDTh
    , billable : EntryType
    , age : EntryAge
    }


entryDecoder : Decoder Entry
entryDecoder =
    Decode.succeed Entry
        |> required "id" int
        |> required "projectId" int
        |> required "taskId" int
        |> required "day" string
        |> required "description" (Decode.map Filled string)
        |> required "closed" bool
        |> required "hours" float
        |> required "billable" entryTypeDecoder
        |> hardcoded Old


makeEntry : Day -> NDTh -> List (Project ReportableTask) -> Entry
makeEntry date defaultHours projects =
    let
        project =
            List.head projects

        task =
            project
                |> Maybe.map .tasks
                |> Maybe.andThen List.head    
    in    
    { id = 0
    , projectId = Maybe.map .id project |> Maybe.withDefault 0
    , taskId = Maybe.map .id task |> Maybe.withDefault 0
    , day = date
    , description = Default "description here"
    , closed = False
    , hours = defaultHours
    , billable = Billable
    , age = New
    }


isEntryDeleted : Entry -> Bool
isEntryDeleted e =
    case e.age of
        Deleted ->
            True

        DeletedNew ->
            True

        _ ->
            False


markDeletedEntry : Entry -> Entry
markDeletedEntry e =
    case e.age of
        New ->
            { e | age = DeletedNew }

        Old ->
            { e | age = Deleted }

        _ ->
            e


type EntryType
    = Billable
    | NonBillable
    | Absence
    | Unknown


entryTypeDecoder : Decoder EntryType
entryTypeDecoder =
    Decode.oneOf
        [ string
            |> Decode.andThen
                (\str ->
                    case str of
                        "billable" ->
                            Decode.succeed Billable

                        "non-billable" ->
                            Decode.succeed NonBillable

                        "absence" ->
                            Decode.succeed Absence

                        _ ->
                            Decode.succeed Unknown
                )
        , Decode.succeed Unknown
        ]


type alias EntryUpdateResponse =
    { user : User
    , hours : HoursResponse
    }


entryUpdateResponseDecoder : Decoder EntryUpdateResponse
entryUpdateResponseDecoder =
    Decode.succeed EntryUpdateResponse
        |> required "user" userDecoder
        |> required "hours" hoursResponseDecoder


type alias EntryUpdate =
    { taskId : Identifier
    , projectId : Identifier
    , description : String
    , date : Day
    , hours : NDTh
    , closed : Bool
    }


entryUpdateEncoder : EntryUpdate -> Encode.Value
entryUpdateEncoder eu =
    Encode.object
        [ ( "taskId", Encode.int eu.taskId )
        , ( "projectId", Encode.int eu.projectId )
        , ( "description", Encode.string eu.description )
        , ( "date", Encode.string eu.date )
        , ( "hours", Encode.float eu.hours )
        , ( "closed", Encode.bool eu.closed )
        ]


entryToUpdate : Entry -> EntryUpdate
entryToUpdate e =
    { taskId = e.taskId
    , projectId = e.projectId
    , description = entryDescToString e.description
    , date = e.day
    , hours = e.hours
    , closed = e.closed
    }


entryToJsonBody : Entry -> Encode.Value
entryToJsonBody e =
    entryToUpdate e
        |> entryUpdateEncoder
