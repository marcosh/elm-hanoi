module Hanoi exposing (Model, init, move, disposition, pegDisksWithOffset, Position(..), OutMsg(..))


type Position
    = FirstPeg
    | SecondPeg
    | ThirdPeg


{-| The i-th element of the list represents the position of i-th disk, where smaller disks have lower numbers.
For example the first element of the list represents the smallest disk
-}
type alias Model =
    List Position


init : Model
init =
    -- for the moment we decide the number of pegs
    List.repeat 3 FirstPeg


type OutMsg
    = SamePeg
    | InvalidMove
    | NoDiskOnPeg


move : Model -> Position -> Position -> ( Model, Maybe OutMsg )
move model from to =
    if from == to then
        ( model, Just SamePeg )
    else
        case model of
            firstPosition :: rest ->
                if firstPosition == to then
                    ( model, Just InvalidMove )
                else if firstPosition == from then
                    ( to :: rest, Nothing )
                else
                    let
                        ( subModel, maybeOutMsg ) =
                            move rest from to
                    in
                        ( firstPosition :: subModel, maybeOutMsg )

            _ ->
                ( [], Just NoDiskOnPeg )


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


disposition : Model -> ( List Int, List Int, List Int )
disposition model =
    ( disksPerPeg model FirstPeg, disksPerPeg model SecondPeg, disksPerPeg model ThirdPeg )


pegDisksWithOffset : List Int -> List ( Int, Int )
pegDisksWithOffset disks =
    case disks of
        [] ->
            []

        top :: rest ->
            let
                restWithOffset =
                    pegDisksWithOffset rest

                maxOffset =
                    case restWithOffset of
                        [] ->
                            -1

                        head :: _ ->
                            fst head
            in
                ( maxOffset + 1, top ) :: restWithOffset
