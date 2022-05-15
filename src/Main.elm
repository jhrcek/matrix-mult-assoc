module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E


type alias Model =
    { dimP : Int
    , dimQ : Int
    , dimR : Int
    , dimS : Int
    , focus : Focus
    }


type Msg
    = SetDimension Dimension Int
    | HoverElement Matrix Int Int
    | HoverCell Matrix Int Int
    | ClearHover
    | NoOp


type Dimension
    = P
    | Q
    | R
    | S


type Matrix
    = A -- p x q
    | B -- q x r
    | C -- r x s


elemLetter : Matrix -> String
elemLetter m =
    case m of
        A ->
            "a"

        B ->
            "b"

        C ->
            "c"


type Focus
    = Element Matrix Int Int
    | Cell Matrix Int Int
    | NoFocus


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dimP = 2
      , dimQ = 2
      , dimR = 2
      , dimS = 2
      , focus = NoFocus
      }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    { title = "Matrix multiplication is associative"
    , body =
        [ viewControls model
        , viewMatrices model
        , Html.text <| Debug.toString model
        ]
    }


viewControls : Model -> Html Msg
viewControls model =
    let
        dimInput lbl getDim dim =
            Html.label []
                [ Html.text lbl
                , Html.input
                    [ A.type_ "number"
                    , A.min "1"
                    , A.max "10"
                    , A.value (String.fromInt (getDim model))
                    , onNumber (SetDimension dim)
                    ]
                    []
                ]
    in
    Html.div []
        [ dimInput "p" .dimP P
        , dimInput "q" .dimQ Q
        , dimInput "r" .dimR R
        , dimInput "s" .dimS S
        ]


viewMatrices : Model -> Html Msg
viewMatrices model =
    let
        tbl =
            -- TODO replace this with ::before based pseudo-elements as in http://matrixmultiplication.xyz/
            Html.table [ A.style "border" "solid 1px black" ]

        cellPad =
            A.style "padding" "15px"

        matrix mat rowDim colDim =
            tbl <|
                List.map
                    (\i ->
                        Html.tr [] <|
                            List.map
                                (\j ->
                                    Html.td
                                        [ A.style "color" (elementColor mat)
                                        , cellPad
                                        , E.onMouseEnter (HoverCell mat i j)
                                        , E.onMouseLeave ClearHover
                                        ]
                                        [ Html.span
                                            [ E.onMouseOver (HoverElement mat i j)
                                            , E.onMouseLeave (HoverCell mat i j)
                                            , case model.focus of
                                                Element fm fi fj ->
                                                    if fm == mat && fi == i && fj == j then
                                                        A.style "background-color" "gray"

                                                    else
                                                        A.style "background-color" "white"

                                                Cell _ _ _ ->
                                                    A.style "background-color" "white"

                                                NoFocus ->
                                                    A.style "background-color" "white"
                                            ]
                                            [ Html.text (elemLetter mat)
                                            , Html.sub [] [ Html.text <| String.fromInt i ++ "," ++ String.fromInt j ]
                                            ]
                                        ]
                                )
                            <|
                                List.range 1 (colDim model)
                    )
                <|
                    List.range 1 (rowDim model)

        matrix2 mat1 mat2 rowDim1 middleDim colDim2 =
            tbl <|
                List.map
                    (\i ->
                        Html.tr [] <|
                            List.map
                                (\j ->
                                    Html.td [ cellPad ] <|
                                        List.concat <|
                                            List.intersperse [ Html.text " + " ] <|
                                                List.map
                                                    (\k ->
                                                        [ Html.span [ A.style "color" (elementColor mat1) ]
                                                            [ Html.text (elemLetter mat1)
                                                            , Html.sub [] [ Html.text <| String.fromInt i ++ "," ++ String.fromInt k ]
                                                            ]
                                                        , Html.span [ A.style "color" (elementColor mat2) ]
                                                            [ Html.text (elemLetter mat2)
                                                            , Html.sub [] [ Html.text <| String.fromInt k ++ "," ++ String.fromInt j ]
                                                            ]
                                                        ]
                                                    )
                                                <|
                                                    List.range 1 (middleDim model)
                                )
                            <|
                                List.range 1 (colDim2 model)
                    )
                <|
                    List.range 1 (rowDim1 model)

        matrix3 =
            tbl <|
                List.map
                    (\i ->
                        Html.tr [] <|
                            List.map
                                (\j ->
                                    Html.td [ cellPad ] <|
                                        List.concat <|
                                            List.intersperse [ Html.text " + " ] <|
                                                List.concatMap
                                                    (\k ->
                                                        List.map
                                                            (\l ->
                                                                [ Html.span [ A.style "color" (elementColor A) ]
                                                                    [ Html.text (elemLetter A)
                                                                    , Html.sub [] [ Html.text <| String.fromInt i ++ "," ++ String.fromInt k ]
                                                                    ]
                                                                , Html.span [ A.style "color" (elementColor B) ]
                                                                    [ Html.text (elemLetter B)
                                                                    , Html.sub [] [ Html.text <| String.fromInt k ++ "," ++ String.fromInt l ]
                                                                    ]
                                                                , Html.span [ A.style "color" (elementColor C) ]
                                                                    [ Html.text (elemLetter C)
                                                                    , Html.sub [] [ Html.text <| String.fromInt l ++ "," ++ String.fromInt j ]
                                                                    ]
                                                                ]
                                                            )
                                                        <|
                                                            List.range 1 model.dimR
                                                    )
                                                <|
                                                    List.range 1 model.dimQ
                                )
                            <|
                                List.range 1 model.dimS
                    )
                <|
                    List.range 1 model.dimP

        flexRow =
            Html.div [ A.style "display" "flex", A.style "flex-direction" "row", A.style "gap" "10px" ]
    in
    Html.div []
        [ flexRow
            [ matrix A .dimP .dimQ
            , matrix B .dimQ .dimR
            , matrix C .dimR .dimS
            ]
        , Html.div [] [ Html.text "=" ]
        , flexRow
            [ matrix2 A B .dimP .dimQ .dimR
            , matrix C .dimR .dimS
            ]
        , Html.div [] [ Html.text "=" ]
        , matrix3
        , Html.div [] [ Html.text "=" ]
        , flexRow
            [ matrix A .dimP .dimQ
            , matrix2 B C .dimQ .dimR .dimS
            ]
        , Html.div [] [ Html.text "=" ]
        , flexRow
            [ matrix A .dimP .dimQ
            , matrix B .dimQ .dimR
            , matrix C .dimR .dimS
            ]
        ]


elementColor : Matrix -> String
elementColor m =
    case m of
        A ->
            "red"

        B ->
            "green"

        C ->
            "blue"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDimension dim val ->
            ( case dim of
                P ->
                    { model | dimP = val }

                Q ->
                    { model | dimQ = val }

                R ->
                    { model | dimR = val }

                S ->
                    { model | dimS = val }
            , Cmd.none
            )

        HoverElement m i j ->
            ( { model | focus = Element m i j }, Cmd.none )

        HoverCell m i j ->
            ( { model | focus = Cell m i j }, Cmd.none )

        ClearHover ->
            ( { model | focus = NoFocus }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onNumber : (Int -> Msg) -> Html.Attribute Msg
onNumber toMsg =
    E.onInput
        (\str ->
            case String.toInt str of
                Just i ->
                    if i > 0 then
                        toMsg i

                    else
                        NoOp

                Nothing ->
                    NoOp
        )



-- TODO brackets around matrices as in http://matrixmultiplication.xyz/
-- TODO in ABC matrix organize the terms so it's visible that each cell has entire copy of B
-- TODO highlighting on hover
