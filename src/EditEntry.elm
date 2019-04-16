module EditEntry exposing (editEntry)

import Dict
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as HA
import Model exposing (Model, isMobile)
import Types as T exposing (Msg(..))
import Ui exposing (colors)


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
            Ui.roundButton disabled True colors.white colors.black handlers.delete (text "-")
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
