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
    = Empty Node
    | Next Elements


add : Node -> Tree -> Tree
add node tree =
    case tree of
        Empty n ->
            Next <| Elements node tree

        Next els ->
            add node els.rest


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
        Empty n ->
            n

        Next els ->
            els.current


calculate : Tree -> Int
calculate tree =
    case tree of
        Empty n ->
            getValue n

        Next els ->
            case (head tree) of
                Nil v ->
                    v + calculate els.rest

                Element v o ->
                    calculatePrimitive o v (calculate els.rest)


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
