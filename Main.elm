module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Card as Card
import Bootstrap.Text as Text


main : Program Never Model Msg
main =
    beginnerProgram { model = model, update = update, view = view }


type alias Model =
    {}


model : Model
model =
    Model


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    Grid.container [ style [ ( "padding-top", "100px" ) ] ]
        [ CDN.stylesheet
        , Grid.row [ Row.centerMd ]
            [ Grid.col
                [ Col.lg8 ]
                [ Card.config []
                    |> Card.headerH3 [] [ text "Formula" ]
                    |> Card.block [ Card.blockAlign Text.alignLgCenter ]
                        [ Card.titleH3 [] [ text "Special title treatment" ]
                        ]
                    |> Card.block []
                        [ Card.text [] [ text "With supporting text below as a natural lead-in to additional content." ]
                        , Card.custom <|
                            Button.button [ Button.primary ] [ text "+" ]
                        ]
                    |> Card.footer [] [ text "2 days ago" ]
                    |> Card.view
                ]
            ]
        ]
