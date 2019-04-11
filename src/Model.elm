module Model exposing (Flags, Model, Window, init, isMobile)

import Api
import Dict exposing (Dict)
import Element exposing (Device, DeviceClass(..))
import Time
import Time.Extra as TE
import Types as T exposing (Msg)



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


type alias EditingWeek =
    { week : Int 
    , entries : List T.Entry
    }


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
    , editingWeek : Maybe EditingWeek
    , allDays : Dict T.Day T.HoursDay
    , saveQueue : List (Cmd Msg)
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
      , window = { width = flags.width, height = flags.height, device = Element.classifyDevice flags }
      , editingHours = Dict.empty
      , editingWeek = Nothing
      , allDays = Dict.empty
      , saveQueue = []
      }
    , Cmd.batch
        [ Api.fetchUser
        , Api.fetchHours thirtyDaysAgo today
        ]
    )
