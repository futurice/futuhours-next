module Types exposing (..)

import Dict exposing (Dict)
import EverySet as Set
import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, float, string, int, bool, list, dict)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Task
import Time
import Iso8601 as Date


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
    | UserResponse (Result Http.Error User)
    | HandleHoursResponse (Result Http.Error HoursResponse)
    | WindowResize Int Int
    | ToggleMenu


send : Msg -> Cmd Msg
send msg =
    Task.succeed msg
        |> Task.perform identity


type alias Identifier =
    Int


type alias NDTh =
    Float


type alias NDTd =
    Float


type alias Day =
    String


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
mergeHoursResponse h1 h2 =
    let
        mergeLists : List (Project a) -> List (Project a) -> List (Project a)
        mergeLists l1 l2 =
            Set.union (Set.fromList l1) (Set.fromList l2)
                |> Set.toList
    in
    HoursResponse
        h2.defaultWorkHours
        ( mergeLists h1.reportableProjects h2.reportableProjects )
        ( mergeLists h1.markedProjects h2.markedProjects )
        ( Dict.merge
            Dict.insert
            (\key a b -> 
                Dict.insert key
                    { b | days = Dict.union a.days b.days }
            )
            Dict.insert
            h1.months 
            h2.months 
            Dict.empty
        )    


hoursToProjectDict : HoursResponse -> Dict Identifier String
hoursToProjectDict hours =
    let
        toDict projects =
            projects
                |> List.map (\t -> (t.id, t.name)) 
                |> Dict.fromList
        
        reportable = toDict hours.reportableProjects
        marked = toDict hours.markedProjects
    in
        Dict.union marked reportable


hoursToTaskDict : HoursResponse -> Dict Identifier String
hoursToTaskDict hours =
    let
        toTasks projects = 
            List.map .tasks projects
                |> List.foldl (++) []
                |> List.map (\t -> (t.id, t.name))
                
        reportableTasks = toTasks hours.reportableProjects
        markedTasks = toTasks hours.markedProjects
    in
        (reportableTasks ++ markedTasks)
            |> Dict.fromList


allEntries : HoursResponse -> List Entry
allEntries hours =
    hours.months
        |> Dict.values
        |> List.concatMap (\m -> m.days |> Dict.values)
        |> List.concatMap .entries
        |> List.sortBy (\e -> e.day |> Date.toTime |> Result.map Time.posixToMillis |> Result.withDefault 0)


latestEntry : HoursResponse -> Maybe Entry
latestEntry hours =
    allEntries hours
        |> List.reverse
        |> List.head


oldestEntry : HoursResponse -> Maybe Entry
oldestEntry hours =
    allEntries hours
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
        [ bool |> Decode.andThen (\b -> if b then Decode.succeed Weekend else Decode.succeed Normal)
        , string |> Decode.andThen (\a -> Decode.succeed (Holiday a))  
        ]


type EntryAge
    = Old
    | New
    | Deleted
    | DeletedNew


type alias Entry =
    { id : Identifier
    , projectId : Identifier
    , taskId : Identifier
    , day : Day
    , description : String
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
        |> required "description" string
        |> required "closed" bool
        |> required "hours" float
        |> required "billable" entryTypeDecoder
        |> hardcoded Old


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
            { e | age = DeletedNew}
    
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
        [string
            |> Decode.andThen
                (\str -> case str of
                    "billable" -> Decode.succeed Billable
                    "non-billable" -> Decode.succeed NonBillable
                    "absence" -> Decode.succeed Absence
                    _ -> Decode.succeed Unknown
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
        [ ("taskId", Encode.int eu.taskId)
        , ("projectId", Encode.int eu.projectId)
        , ("description", Encode.string eu.description)
        , ("date", Encode.string eu.date)
        , ("hours", Encode.float eu.hours)
        , ("closed", Encode.bool eu.closed) 
        ]
