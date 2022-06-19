module Matrix exposing (Expr(..), Matrix, mult, test)

import Array exposing (Array)


{-| Matrix that has 'a' in each cell
-}
type alias Matrix a =
    { dims : ( Int, Int )

    -- Outer array = rows, inner array = cells within row
    , cells : Array (Array a)
    }


type Expr a
    = Val a
    | Plus (Expr a) (Expr a)
    | Times (Expr a) (Expr a)
      -- Is there a better way to deal with partiality of array lookup?
    | Crash


mult : Matrix (Expr a) -> Matrix (Expr a) -> Maybe (Matrix (Expr a))
mult a b =
    if Tuple.second a.dims == Tuple.first b.dims then
        Just (multiplyConformable a b)

    else
        Nothing


transpose : Matrix (Expr a) -> Matrix (Expr a)
transpose =
    Debug.todo ""


multiplyConformable : Matrix (Expr a) -> Matrix (Expr a) -> Matrix (Expr a)
multiplyConformable a b =
    let
        ( resultRows, innerDim ) =
            a.dims

        ( _, resultColumns ) =
            b.dims

        prodElems : Int -> Int -> Int -> Expr a -> Expr a
        prodElems i j k acc =
            let
                maik =
                    Array.get i a.cells |> Maybe.andThen (\arow -> Array.get k arow)

                mbkj =
                    Array.get j b.cells |> Maybe.andThen (\bcol -> Array.get k bcol)
            in
            Maybe.map2
                (case acc of
                    Crash ->
                        \aik bkj -> Times aik bkj

                    other ->
                        \aik bkj -> Plus (Times aik bkj) other
                )
                maik
                mbkj
                |> Maybe.withDefault Crash
    in
    { dims = ( resultRows, resultColumns )
    , cells =
        Array.initialize resultRows
            (\i ->
                Array.initialize resultColumns
                    (\j ->
                        List.foldr (prodElems i j) Crash <| List.range 0 (innerDim - 1)
                    )
            )
    }


evalInt : Expr Int -> Int
evalInt expr =
    case expr of
        Val i ->
            i

        Plus ea eb ->
            evalInt ea + evalInt eb

        Times ea eb ->
            evalInt ea * evalInt eb

        Crash ->
            0


test : Maybe (Matrix (Expr Int))
test =
    let
        a =
            { dims = ( 2, 3 )
            , cells =
                Array.fromList
                    [ Array.fromList [ Val 1, Val 2, Val 3 ]
                    , Array.fromList [ Val 4, Val 5, Val 6 ]
                    ]
            }

        at =
            transpose a
    in
    mult a at
