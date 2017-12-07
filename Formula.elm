module Formula exposing (..)


type Operation
    = Plus
    | Minus
    | Multiply
    | Divide


type Node
    = Nil
    | Element Int Operation


type alias Elements =
    { current : Node
    , rest : Tree
    }


type Tree
    = Empty
    | Next Elements


set : Int -> Operation -> Tree -> Tree
set value operation tree =
    case tree of
        Empty ->
            Elements Nil (Next (Elements (Element value operation) Empty)) |> .rest

        Next els ->
            set value operation els.rest


get : Tree -> Node
get tree =
    case tree of
        Empty ->
            Nil

        Next els ->
            els.current
