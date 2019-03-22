module Types exposing
    ( Day
    , Entry
    , EntryType
    , EntryUpdate
    , EntryUpdateResponse
    , HoursDay
    , HoursMonth
    , HoursResponse
    , Identifier
    , LatestEntry
    , Login
    , Month
    , NDTd
    , NDTh
    , Project
    , ReportableTask
    , User
    , emptyUser
    , userDecoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, field, float, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


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


type alias Project =
    { id : Identifier
    , name : String
    , tasks : List ReportableTask
    , closed : Bool
    }


type alias ReportableTask =
    { id : Identifier
    , name : String
    , closed : Bool
    , latestEntry : LatestEntry
    , hoursRemaining : NDTh
    }


type alias LatestEntry =
    { description : String
    , date : Day
    , hours : NDTh
    }


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
    , reportableProjects : List Project
    , markedProjects : List Project
    , months : Dict Month HoursMonth
    }


type alias HoursMonth =
    { hours : NDTh
    , capacity : NDTh
    , utilizationRate : Float
    , days : Dict Day HoursDay
    }


type alias HoursDay =
    { type_ : String
    , hours : NDTh
    , entries : List Entry
    , closed : Bool
    }


type alias Entry =
    { id : Identifier
    , projectId : Identifier
    , taskId : Identifier
    , day : Day
    , description : String
    , closed : Bool
    , hours : NDTh
    , billably : EntryType
    }


type EntryType
    = Billable
    | NonBillable
    | Absence


type alias EntryUpdateResponse =
    { user : User
    , hours : HoursResponse
    }


type alias EntryUpdate =
    { taskId : Identifier
    , projectId : Identifier
    , description : String
    , date : Day
    , hours : NDTh
    , closed : Bool
    }
