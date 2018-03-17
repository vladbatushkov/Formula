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


calcTree : Tree -> Int
calcTree tree =
    case tree of
        Last n ->
            n

        Next els ->
            case els.current of
                Nil val ->
                    val

                Element val op ->
                    calcNode op val (calcTree els.rest)


calcNode : Operation -> Int -> Int -> Int
calcNode operationType val1 val2 =
    case operationType of
        Plus ->
            val1 + val2

        Minus ->
            val1 - val2

        Multiply ->
            val1 * val2

        Divide ->
            val1 // val2
