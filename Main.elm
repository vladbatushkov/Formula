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
    { dropdownState : Dropdown.State
    , selectedLevel : Level
    , selectedOperationPosition : OperationPosition
    , firstPositionOperation : OperationType
    , secondPositionOperation : OperationType
    , firstValue : Int
    , secondValue : Int
    , thirdValue : Int
    , resultValue : Int
    }


model : ( Model, Cmd Msg )
model =
    ( Model Dropdown.initialState Easy None Question Question 0 0 0 0, Cmd.none )


type Level
    = Easy
    | Medium
    | Hard


type OperationPosition
    = None
    | First
    | Second


type OperationType
    = Question
    | Plus
    | Minus
    | Multiply
    | Divide


type Msg
    = DropdownToggleMsg Dropdown.State
    | ChangeLevelMsg Level
    | ToggleOperationPositionMsg OperationPosition
    | ChangeOperationMsg OperationType
    | Validate
    | NewPuzzle Level


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DropdownToggleMsg state ->
            ( { model | dropdownState = state }
            , Cmd.none
            )

        ChangeLevelMsg level ->
            ( { model | selectedLevel = level }
            , Cmd.none
            )

        ToggleOperationPositionMsg operationPosition ->
            let
                selectedOperationPosition =
                    if (model.selectedOperationPosition == operationPosition) then
                        None
                    else if (operationPosition == Second) then
                        Second
                    else
                        First
            in
                ( { model | selectedOperationPosition = selectedOperationPosition }
                , Cmd.none
                )

        ChangeOperationMsg operationType ->
            let
                firstPositionOperation =
                    if (model.selectedOperationPosition == First) then
                        operationType
                    else
                        model.firstPositionOperation

                secondPositionOperation =
                    if (model.selectedOperationPosition == Second) then
                        operationType
                    else
                        model.secondPositionOperation
            in
                ( { model | firstPositionOperation = firstPositionOperation, secondPositionOperation = secondPositionOperation }
                , Cmd.none
                )

        Validate ->
            ( { model | selectedOperationPosition = None }
            , Cmd.none
            )

        NewPuzzle level ->
            ( { model | selectedOperationPosition = None }
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
                        , Button.button [ Button.onClick Validate, Button.outlinePrimary, Button.attrs [ class "mr-1", style [ ( "float", "right" ) ] ] ] [ text "Validate" ]
                        ]
                    |> Card.view
                ]
            ]
        ]


panelFormula : Model -> Html Msg
panelFormula model =
    div []
        [ Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True ] [ text <| toString model.firstValue ]
        , Button.button
            [ Button.onClick <| ToggleOperationPositionMsg First
            , Button.small
            , selectedOperation <| model.selectedOperationPosition == First
            , Button.attrs [ class "ml-1" ]
            ]
            [ text <| mapOperationTypeToString model.firstPositionOperation ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text <| toString model.secondValue ]
        , Button.button
            [ Button.onClick <| ToggleOperationPositionMsg Second
            , Button.small
            , selectedOperation <| model.selectedOperationPosition == Second
            , Button.attrs [ class "ml-1" ]
            ]
            [ text <| mapOperationTypeToString model.secondPositionOperation ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text <| toString model.thirdValue ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "=" ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text <| toString model.resultValue ]
        ]


mapOperationTypeToString : OperationType -> String
mapOperationTypeToString operationType =
    case operationType of
        Question ->
            "?"

        Plus ->
            "+"

        Minus ->
            "-"

        Multiply ->
            "x"

        Divide ->
            "/"


selectedOperation : Bool -> Button.Option Msg
selectedOperation isSelected =
    if (isSelected) then
        Button.warning
    else
        Button.outlineWarning


panelOperations : Model -> Html Msg
panelOperations model =
    div []
        [ Button.button [ Button.onClick <| ChangeOperationMsg Plus, Button.outlineWarning ] [ text "+" ]
        , Button.button [ Button.onClick <| ChangeOperationMsg Minus, Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "-" ]
        , Button.button [ Button.onClick <| ChangeOperationMsg Multiply, Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "x" ]
        , Button.button [ Button.onClick <| ChangeOperationMsg Divide, Button.outlineWarning, Button.attrs [ class "ml-1" ] ] [ text "/" ]
        ]


dropdownLevel : Model -> Html Msg
dropdownLevel model =
    Dropdown.dropdown
        model.dropdownState
        { options = [ Dropdown.attrs [ style [ ( "float", "right" ) ] ] ]
        , toggleMsg = DropdownToggleMsg
        , toggleButton =
            Dropdown.toggle [ Button.outlinePrimary ] [ text <| mapLevelToString model.selectedLevel ]
        , items =
            [ Dropdown.buttonItem [ onClick <| ChangeLevelMsg Easy ] [ text <| mapLevelToString Easy ]
            , Dropdown.buttonItem [ onClick <| ChangeLevelMsg Medium ] [ text <| mapLevelToString Medium ]
            , Dropdown.buttonItem [ onClick <| ChangeLevelMsg Hard ] [ text <| mapLevelToString Hard ]
            ]
        }


mapLevelToString : Level -> String
mapLevelToString level =
    case level of
        Easy ->
            "Easy"

        Medium ->
            "Medium"

        Hard ->
            "Hard"
