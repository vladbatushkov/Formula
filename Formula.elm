module Formula exposing (..)


type Operation
    = Plus
    | Minus
    | Multiply
    | Divide


type Node
    = Nil Int
    | Element Int Operation


type alias Elements =
    { current : Node
    , rest : Tree
    }


type Tree
    = Last Int
    | Next Elements


add : Node -> Tree -> Tree
add node tree =
    case tree of
        Last n ->
            Next <| Elements node <| Last n

        Next els ->
            Next <| Elements node <| tree


getValue : Node -> Int
getValue node =
    case node of
        Nil val ->
            val

        Element val operation ->
            val


getOperation : Node -> Maybe Operation
getOperation node =
    case node of
        Nil val ->
            Nothing

        Element val operation ->
            Just operation


head : Tree -> Node
head tree =
    case tree of
        Last n ->
            Nil n

        Next els ->
            els.current


calculate : Tree -> Int
calculate tree =
    case tree of
        Last n ->
            n

        Next els ->
            case els.current of
                Nil val ->
                    val

                Element val op ->
                    calculatePrimitive op val (calculate els.rest)


calculatePrimitive : Operation -> Int -> Int -> Int
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
