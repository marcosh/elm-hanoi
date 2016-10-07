module HanoiView exposing (init, update, view)

import Html exposing (Html, Attribute, div, select, option, button, text)
import Html.Events exposing (on, onClick)
import Html.Attributes exposing (selected)
import Hanoi exposing (..)
import Json.Decode as Json exposing (map, at, int)


type Peg
    = First
    | Second
    | Third


type alias Model =
    { status : Hanoi.Model
    , from : Maybe Peg
    , to : Maybe Peg
    , message : Maybe String
    }


init : Model
init =
    let
        status =
            Hanoi.init
    in
        Model status Nothing Nothing Nothing


pegToPosition : Peg -> Hanoi.Position
pegToPosition peg =
    case peg of
        First ->
            Hanoi.FirstPeg

        Second ->
            Hanoi.SecondPeg

        Third ->
            Hanoi.ThirdPeg


type Msg
    = From Peg
    | To Peg
    | Move


update : Msg -> Model -> Model
update msg model =
    case msg of
        From peg ->
            { model | from = Just peg, message = Nothing }

        To peg ->
            { model | to = Just peg, message = Nothing }

        Move ->
            case ( model.from, model.to ) of
                ( Nothing, _ ) ->
                    { model | message = Just "You didn't specify the starting peg" }

                ( _, Nothing ) ->
                    { model | message = Just "You didn't specify the ending peg" }

                ( Just from, Just to ) ->
                    performMove model.status from to


performMove : Hanoi.Model -> Peg -> Peg -> Model
performMove status from to =
    let
        ( newStatus, maybeOutMsg ) =
            Hanoi.move status (pegToPosition from) (pegToPosition to)
    in
        case maybeOutMsg of
            Just SamePeg ->
                Model newStatus (Just from) (Just to) (Just "You can not move a disk to a peg where it already is")

            Just InvalidMove ->
                Model newStatus (Just from) (Just to) (Just "Invalid move")

            Just NoDiskOnPeg ->
                Model newStatus (Just from) (Just to) (Just "There is no disk in your starting peg")

            Nothing ->
                Model newStatus Nothing Nothing Nothing


view : Model -> Html Msg
view model =
    let
        ( firstPeg, secondPeg, thirdPeg ) =
            Hanoi.disposition model.status
    in
        div []
            [ div []
                [ div []
                    ((text "First Peg: ")
                        :: (List.map (toString >> text)
                                firstPeg
                           )
                    )
                , div []
                    ((text "Second Peg: ")
                        :: (List.map (toString >> text)
                                secondPeg
                           )
                    )
                , div []
                    ((text "Third Peg: ")
                        :: (List.map (toString >> text)
                                thirdPeg
                           )
                    )
                ]
            , div []
                [ text "From: "
                , select [ onSelect From ]
                    [ option [] [ text "" ]
                    , option [ selected (model.from == Just First) ] [ text "First" ]
                    , option [ selected (model.from == Just Second) ] [ text "Second" ]
                    , option [ selected (model.from == Just Third) ] [ text "Third" ]
                    ]
                ]
            , div []
                [ text "To: "
                , select [ onSelect To ]
                    [ option [] [ text "" ]
                    , option [ selected (model.to == Just First) ] [ text "First" ]
                    , option [ selected (model.to == Just Second) ] [ text "Second" ]
                    , option [ selected (model.to == Just Third) ] [ text "Third" ]
                    ]
                ]
            , button [ onClick Move ] [ text "Move" ]
            , case model.message of
                Just string ->
                    text string

                Nothing ->
                    text ""
            ]


onSelect : (Peg -> Msg) -> Attribute Msg
onSelect msg =
    on "change" (Json.map msg targetSelectedIndex)


targetSelectedIndex : Json.Decoder Peg
targetSelectedIndex =
    Json.map
        (\i ->
            case i of
                1 ->
                    First

                2 ->
                    Second

                3 ->
                    Third

                _ ->
                    Debug.crash "impossible value"
        )
        (Json.at [ "target", "selectedIndex" ] Json.int)
