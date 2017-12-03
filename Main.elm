module Main exposing (..)

import Random exposing (..)
import Arithmetic exposing (..)
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
    , firstOperationType : OperationType
    , secondOperationType : OperationType
    , firstValue : Int
    , secondValue : Int
    , thirdValue : Int
    , resultValue : Int
    , solvedStatus : SolvedStatus
    }


model : ( Model, Cmd Msg )
model =
    update (NewPuzzle Easy) (Model Dropdown.initialState Easy None Question Question 0 0 0 0 Solving)


type SolvedStatus
    = Solved
    | Failed
    | Solving


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
    | FirstValue Int
    | SecondValue Int
    | ThirdValue Int
    | FirstOperation OperationType
    | SecondOperation OperationType


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
                firstOperationType =
                    if (model.selectedOperationPosition == First) then
                        operationType
                    else
                        model.firstOperationType

                secondOperationType =
                    if (model.selectedOperationPosition == Second) then
                        operationType
                    else
                        model.secondOperationType
            in
                ( { model | firstOperationType = firstOperationType, secondOperationType = secondOperationType }
                , Cmd.none
                )

        Validate ->
            if (model.firstOperationType == Question || model.secondOperationType == Question) then
                ( model
                , Cmd.none
                )
            else if
                (model.resultValue
                    == calculateResultValue
                        model.firstValue
                        model.secondValue
                        model.thirdValue
                        model.firstOperationType
                        model.secondOperationType
                )
            then
                ( { model | solvedStatus = Solved }
                , Cmd.none
                )
            else
                ( { model | solvedStatus = Failed }
                , Cmd.none
                )

        NewPuzzle level ->
            ( Model Dropdown.initialState model.selectedLevel None Question Question 0 0 0 0 Solving
            , Cmd.batch
                [ Random.generate ThirdValue <| generateValue level
                , Random.generate SecondOperation generateOperation
                , Random.generate SecondValue <| generateValue level
                , Random.generate FirstOperation generateOperation
                , Random.generate FirstValue <| generateValue level
                ]
            )

        FirstOperation operationType ->
            ( { model | firstOperationType = operationType }
            , Cmd.none
            )

        SecondOperation operationType ->
            ( { model | secondOperationType = operationType }
            , Cmd.none
            )

        FirstValue value ->
            ( { model | firstValue = value }
            , Cmd.none
            )

        SecondValue value ->
            ( { model | secondValue = value }
            , Cmd.none
            )

        ThirdValue thirdValue ->
            if
                ((model.firstOperationType == Divide && (not <| divides model.secondValue model.firstValue))
                    || (model.secondOperationType == Divide && (not <| divides thirdValue model.secondValue))
                )
            then
                update (NewPuzzle model.selectedLevel) model
            else
                ( { model
                    | thirdValue = thirdValue
                    , resultValue =
                        calculateResultValue
                            model.firstValue
                            model.secondValue
                            thirdValue
                            model.firstOperationType
                            model.secondOperationType
                    , firstOperationType = Question
                    , secondOperationType = Question
                  }
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
                        [ Button.button [ Button.onClick <| NewPuzzle model.selectedLevel, Button.outlineSecondary, Button.attrs [ class "ml-1" ] ] [ text "New" ]

                        --, Button.button [ Button.outlineSecondary, Button.attrs [ class "ml-1" ] ] [ text "Hint" ]
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
            [ text <| mapOperationTypeToString model.firstOperationType ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text <| toString model.secondValue ]
        , Button.button
            [ Button.onClick <| ToggleOperationPositionMsg Second
            , Button.small
            , selectedOperation <| model.selectedOperationPosition == Second
            , Button.attrs [ class "ml-1" ]
            ]
            [ text <| mapOperationTypeToString model.secondOperationType ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text <| toString model.thirdValue ]
        , Button.linkButton [ Button.small, Button.outlineSecondary, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text "=" ]
        , Button.linkButton [ Button.small, resultStyle model.solvedStatus, Button.disabled True, Button.attrs [ class "ml-1" ] ] [ text <| toString model.resultValue ]
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


resultStyle : SolvedStatus -> Button.Option Msg
resultStyle status =
    case status of
        Solving ->
            Button.outlineSecondary

        Solved ->
            Button.outlineSuccess

        Failed ->
            Button.outlineDanger


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


generateOperation : Generator OperationType
generateOperation =
    Random.map (\a -> mapIntToOperationType a) (Random.int 1 4)


mapIntToOperationType : Int -> OperationType
mapIntToOperationType x =
    case x of
        1 ->
            Plus

        2 ->
            Minus

        3 ->
            Multiply

        _ ->
            Divide


generateValue : Level -> Generator Int
generateValue lvl =
    case lvl of
        Easy ->
            Random.int 1 9

        Medium ->
            Random.int 10 99

        Hard ->
            Random.int 100 999


calculateResultValue : Int -> Int -> Int -> OperationType -> OperationType -> Int
calculateResultValue firstValue secondValue thirdValue firstOperationType secondOperationType =
    let
        isByDefaultOrder =
            firstOperationType == Multiply || firstOperationType == Divide
    in
        if (isByDefaultOrder) then
            calculatePrimitive
                secondOperationType
                (calculatePrimitive firstOperationType firstValue secondValue)
                thirdValue
        else
            calculatePrimitive
                firstOperationType
                firstValue
                (calculatePrimitive secondOperationType secondValue thirdValue)


calculatePrimitive : OperationType -> Int -> Int -> Int
calculatePrimitive operationType value1 value2 =
    case operationType of
        Plus ->
            value1 + value2

        Minus ->
            value1 - value2

        Multiply ->
            value1 * value2

        Divide ->
            value1 // value2

        Question ->
            0
