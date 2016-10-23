module HanoiView exposing (init, update, view)

import Html exposing (Html, Attribute, div, select, option, button, text, ol, li)
import Html.Events exposing (on, onClick)
import Html.Attributes exposing (class, selected, id, attribute)
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
    = From (Maybe Peg)
    | To (Maybe Peg)
    | Error String
    | Move Peg Peg


update : Msg -> Model -> Model
update msg model =
    case msg of
        From maybePeg ->
            { model | from = maybePeg, message = Nothing }

        To maybePeg ->
            { model | to = maybePeg, message = Nothing }

        Error message ->
            { model | message = Just message }

        Move from to ->
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


viewDisk : ( Int, Int ) -> Html Msg
viewDisk ( offset, i ) =
    li [ class "disk" ]
        [ div [ id ("disk" ++ toString i), attribute "data-offset" (toString offset) ] []
        ]


view : Model -> Html Msg
view model =
    let
        ( firstPeg, secondPeg, thirdPeg ) =
            Hanoi.disposition model.status
    in
        div [ class "container" ]
            [ div [ class "peg" ]
                [ ol [ class "pegList" ]
                    (List.map viewDisk
                        (Hanoi.pegDisksWithOffset firstPeg)
                    )
                ]
            , div [ class "peg" ]
                [ ol [ class "pegList" ]
                    (List.map viewDisk
                        (Hanoi.pegDisksWithOffset secondPeg)
                    )
                ]
            , div [ class "peg" ]
                [ ol [ class "pegList" ]
                    (List.map viewDisk
                        (Hanoi.pegDisksWithOffset thirdPeg)
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
            , button [ onClick (tryMove model) ] [ text "Move" ]
            , case model.message of
                Just string ->
                    text string

                Nothing ->
                    text ""
            ]


onSelect : (Maybe Peg -> Msg) -> Attribute Msg
onSelect msg =
    on "change" (Json.map msg targetSelectedIndex)


targetSelectedIndex : Json.Decoder (Maybe Peg)
targetSelectedIndex =
    Json.map
        (\i ->
            case i of
                1 ->
                    Just First

                2 ->
                    Just Second

                3 ->
                    Just Third

                _ ->
                    Nothing
        )
        (Json.at [ "target", "selectedIndex" ] Json.int)


tryMove : Model -> Msg
tryMove model =
    case ( model.from, model.to ) of
        ( Nothing, _ ) ->
            Error "You didn't specify the starting peg"

        ( _, Nothing ) ->
            Error "You didn't specify the ending peg"

        ( Just from, Just to ) ->
            Move from to
