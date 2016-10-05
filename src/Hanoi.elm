module Hanoi exposing (init, update, view)

import Html exposing (Html, text, div)


-- MODEL --


type Position
    = FirstPeg
    | SecondPeg
    | ThirdPeg


{-| The i-th element of the list represents the position of i-th peg, where smaller disks have lower numbers.
For example the first element of the list represents the smallest disk
-}
type alias Model =
    List Position


init : Model
init =
    [ FirstPeg, SecondPeg, ThirdPeg, SecondPeg, ThirdPeg, FirstPeg, FirstPeg ]



-- UPDATE --


type Msg
    = NewGame
    | Move Position Position


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            init

        Move from to ->
            move model from to


move : Model -> Position -> Position -> Model
move model from to =
    model



-- VIEW --


disksPerPeg : Model -> Position -> List Int
disksPerPeg =
    disksPerPegNumber 1


disksPerPegNumber : Int -> Model -> Position -> List Int
disksPerPegNumber number model peg =
    case model of
        position :: rest ->
            if position == peg then
                number :: (disksPerPegNumber (number + 1) rest peg)
            else
                (disksPerPegNumber (number + 1) rest peg)

        _ ->
            []


view : Model -> Html Msg
view model =
    div []
        [ div []
            ((text "First Peg: ")
                :: (List.map (toString >> text)
                        (disksPerPeg model FirstPeg)
                   )
            )
        , div []
            ((text "Second Peg: ")
                :: (List.map (toString >> text)
                        (disksPerPeg model SecondPeg)
                   )
            )
        , div []
            ((text "Third Peg: ")
                :: (List.map (toString >> text)
                        (disksPerPeg model ThirdPeg)
                   )
            )
        ]
