module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Text as Text
import Bootstrap.Dropdown as Dropdown


main : Program Never Model Msg
main =
    program { init = model, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { dropdownState : Dropdown.State, titleLevel : String }


model : ( Model, Cmd Msg )
model =
    ( Model Dropdown.initialState "Easy", Cmd.none )


type Msg
    = DropdownToggleMsg Dropdown.State
    | DropdownEasyMsg
    | DropdownMediumMsg
    | DropdownHardMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownToggleMsg state ->
            ( { model | dropdownState = state }
            , Cmd.none
            )

        DropdownEasyMsg ->
            ( { model | titleLevel = "Easy" }
            , Cmd.none
            )

        DropdownMediumMsg ->
            ( { model | titleLevel = "Meduim" }
            , Cmd.none
            )

        DropdownHardMsg ->
            ( { model | titleLevel = "Hard" }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.dropdownState DropdownToggleMsg ]


view : Model -> Html Msg
view model =
    Grid.container [ style [ ( "padding-top", "100px" ) ] ]
        [ CDN.stylesheet
        , Grid.row [ Row.centerMd ]
            [ Grid.col
                [ Col.xs12, Col.md8, Col.lg6 ]
                [ Card.config []
                    |> Card.headerH3 [] [ text "Formula" ]
                    |> Card.block [ Card.blockAlign Text.alignXsCenter ]
                        [ Card.text [] [ text "Select operations to solve the formula" ]
                        , Card.custom <| panelFormula model
                        ]
                    |> Card.block [ Card.blockAlign Text.alignXsCenter ]
                        [ Card.custom <| panelOperations model
                        ]
                    |> Card.footer []
                        [ Button.button [ Button.outlineSecondary, Button.attrs [ class "ml-1" ] ] [ text "Reset" ]
                        , Button.button [ Button.outlineSecondary, Button.attrs [ class "ml-1" ] ] [ text "Hint" ]
                        , dropdownLevel model
                        , Button.button [ Button.outlinePrimary, Button.attrs [ class "mr-1", style [ ( "float", "right" ) ] ] ] [ text "Validate" ]
                        ]
                    |> Card.view
                ]
            ]
        ]


panelFormula : Model -> Html Msg
panelFormula model =
    div []
        [ Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True ] [ text "5" ]
        , Button.button [ Button.small, Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "?" ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "1" ]
        , Button.button [ Button.small, Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "?" ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "9" ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "=" ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "16" ]
        ]


panelOperations : Model -> Html Msg
panelOperations model =
    div []
        [ Button.button [ Button.outlineWarning ] [ text "+" ]
        , Button.button [ Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "-" ]
        , Button.button [ Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "x" ]
        , Button.button [ Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "/" ]
        ]


dropdownLevel : Model -> Html Msg
dropdownLevel model =
    Dropdown.dropdown
        model.dropdownState
        { options = [ Dropdown.attrs [ style [ ( "float", "right" ) ] ] ]
        , toggleMsg = DropdownToggleMsg
        , toggleButton =
            Dropdown.toggle [ Button.outlinePrimary ] [ text model.titleLevel ]
        , items =
            [ Dropdown.buttonItem [ onClick DropdownEasyMsg ] [ text "Easy" ]
            , Dropdown.buttonItem [ onClick DropdownMediumMsg ] [ text "Medium" ]
            , Dropdown.buttonItem [ onClick DropdownHardMsg ] [ text "Hard" ]
            ]
        }
