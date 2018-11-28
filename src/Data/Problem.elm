module Data.Problem exposing (Problem, decodeProblem, encodeProblem, encodeProblemAnswer)

import Json.Decode as D
import Json.Encode as E


type alias Problem =
    { url : String
    , answer : String
    }


encodeProblem : Problem -> E.Value
encodeProblem problem =
    E.object
        [ ( "url", E.string problem.url )
        , ( "answer", E.string problem.answer )
        ]

encodeProblemAnswer : Problem -> E.Value
encodeProblemAnswer problem =
    E.object
        [ ( "url", E.string problem.url )
        , ( "answer", E.string problem.answer )
        ]


decodeProblem : D.Decoder Problem
decodeProblem =
    D.map2 Problem
        (D.field "url" D.string)
        (D.field "answer" D.string)
