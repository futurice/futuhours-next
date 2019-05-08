module TopBar exposing (topBar)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as HA exposing (style)
import Model exposing (Model, isMobile)
import Msg exposing (Msg(..))
import Types as T
import Ui exposing (colors)


statGroup : Model -> Element Msg
statGroup model =
    let
        user =
            Maybe.withDefault T.emptyUser model.user

        statElement icon value label tooltip =
            row
                [ spacing 10
                , htmlAttribute <| HA.title tooltip
                ]
                [ el [] (Ui.faIcon icon)
                , text value
                , text label
                ]

        commonOptions =
            [ Font.regular
            , centerX
            , Font.color colors.darkText
            ]

        deskOptions =
            [ spacing 40
            , Font.size 18
            ]
                ++ commonOptions

        mobileOptions =
            [ spacing 20
            , Font.size 16
            ]
                ++ commonOptions
    in
    row
        (if isMobile model.window then
            mobileOptions

         else
            deskOptions
        )
        [ statElement "far fa-clock" (String.fromFloat user.balance) "h" "Hour balance"
        , text "|"
        , statElement "far fa-chart-bar" (String.fromInt <| round user.utilizationRate) "%" "Utilization rate for last 30 days"
        , text "|"
        , statElement "far fa-sun" (String.fromFloat user.holidaysLeft) "days" "Holidays left"
        ]


avatarDrop : Model -> Element Msg
avatarDrop model =
    let
        img =
            case model.user of
                Just user ->
                    user.profilePicture

                Nothing ->
                    ""
    in
    Input.button
        [ htmlAttribute <| HA.class "nested-focus"
        , htmlAttribute <| HA.attribute "aria-expanded" (boolToAttrString model.isMenuOpen)
        -- This is the least-resistance way to add an accessible label.
        -- Visually hidden labels that are not the entire "label" are hard in Elm-ui.
        -- and we should also avoid changing the label on state change, since state is communicated via `aria-expanded`
        , htmlAttribute <| HA.attribute "aria-label" "Menu"
        ]
        { onPress = Just ToggleMenu
        , label =
            row
                [ Font.color colors.darkText
                , spacing 10
                ]
                [ image
                    [ alignRight
                    , width <| px 40
                    , height <| px 40
                    , htmlAttribute <| HA.class "nested-focus-target menu-circle"
                    ]
                    { src = img

                    -- Leave the description empty, since the image does not contribute to announcing the menu
                    -- (also, with aria-label above, this content would not contribute to the button label anyway)
                    , description = ""
                    }
                , el
                    [ if model.isMenuOpen then
                        rotate Basics.pi

                      else
                        rotate 0
                    , Font.color colors.white
                    ]
                    (Ui.faIcon "fa fa-angle-down")
                ]
        }


profileDropdown : Model -> Element Msg
profileDropdown model =
    let
        name =
            case model.user of
                Just user ->
                    user.firstName

                Nothing ->
                    "Noname"

        itemElement attrs elem =
            el ([ paddingXY 40 0 ] ++ attrs) elem
    in
    column
        [ alignRight
        , paddingXY 0 30
        , spacing 20
        , if isMobile model.window then
            moveLeft 0

          else
            moveLeft (model.window |> .width |> (\x -> (x - 920) // 2) |> toFloat)
        , Font.color colors.white
        , Font.light
        , Font.size 16
        , Background.color colors.topBarBackground
        ]
        [ itemElement [ Font.color colors.darkText ] (text name)
        , el [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }, width fill ] none
        , column [ paddingXY 40 0, spacing 20 ]
            [ newTabLink [ htmlAttribute <| HA.class "link" ] { url = "https://online.planmill.com/futurice/", label = text "Planmill" }
            , newTabLink [ htmlAttribute <| HA.class "link" ] { url = "https://confluence.futurice.com/pages/viewpage.action?pageId=43321030", label = text "Help" }
            , newTabLink [ htmlAttribute <| HA.class "link" ] { url = "https://hours-api.app.futurice.com/debug/users", label = text "Debug: users" }
            , newTabLink [ htmlAttribute <| HA.class "link" ] { url = "https://login.futurice.com/?logout=true", label = text "Logout" }
            ]
        ]


topBar : Model -> Element Msg
topBar model =
    let
        dropdown =
            if model.isMenuOpen then
                profileDropdown model

            else
                none

        commonOptions =
            [ width fill
            , Background.color colors.topBarBackground
            , Font.color colors.white
            , below dropdown
            ]

        deskOptions =
            [ height <| px 70
            , paddingXY 50 20
            , Font.size 16
            ]
                ++ commonOptions

        mobileOptions =
            [ paddingXY 20 15
            , spacing 20
            , Font.size 12
            ]
                ++ commonOptions

        futuLogo =
            image [ alignLeft ] { src = "futuhours.svg", description = "FutuHours" }
    in
    if isMobile model.window then
        column
            mobileOptions
            [ row [ width fill ]
                [ futuLogo
                , el [ alignRight ] (avatarDrop model)
                ]
            , statGroup model
            ]

    else
        row
            deskOptions
            [ row [ centerX, width (fill |> maximum 900) ]
                [ futuLogo
                , statGroup model
                , avatarDrop model
                ]
            ]


boolToAttrString : Bool -> String
boolToAttrString bool =
    case bool of
        True ->
            "true"

        False ->
            "false"
