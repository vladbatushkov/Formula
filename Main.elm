module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Text as Text
import Bootstrap.Dropdown as Dropdown


main : Program Never Model Msg
main =
    program { init = model, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { dropdownState : Dropdown.State }


model : ( Model, Cmd Msg )
model =
    ( Model Dropdown.initialState, Cmd.none )


type Msg
    = MyDrop1Msg Dropdown.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MyDrop1Msg state ->
            ( { model | dropdownState = state }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.dropdownState MyDrop1Msg ]


view : Model -> Html Msg
view model =
    Grid.container [ style [ ( "padding-top", "100px" ), ( "width", "50%" ) ] ]
        [ CDN.stylesheet
        , Card.config []
            |> Card.headerH3 [] [ text "Formula" ]
            |> Card.block [ Card.blockAlign Text.alignXsCenter ]
                [ Card.text [] [ text "Select operations to solve the formula" ]
                , Card.custom <|
                    div []
                        [ Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True ] [ text "5" ]
                        , Button.button [ Button.small, Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "?" ]
                        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "1" ]
                        , Button.button [ Button.small, Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "?" ]
                        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "9" ]
                        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "=" ]
                        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "16" ]
                        ]
                ]
            |> Card.block [ Card.blockAlign Text.alignXsCenter ]
                [ Card.custom <| panelOperations <| model
                ]
            |> Card.footer []
                [ Button.button [ Button.outlineSuccess, Button.attrs [ class "ml-1" ] ] [ text "New puzzle" ]
                , Button.button [ Button.outlineSecondary, Button.attrs [ class "ml-1" ] ] [ text "Show hint" ]
                , dropdownLevel model
                ]
            |> Card.view
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
        , toggleMsg = MyDrop1Msg
        , toggleButton =
            Dropdown.toggle [ Button.outlinePrimary ] [ text "Easy" ]
        , items =
            [ Dropdown.buttonItem [] [ text "Medium" ]
            , Dropdown.buttonItem [] [ text "Hard" ]
            ]
        }
