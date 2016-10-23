module WebsocketHanoiView exposing (init, update, view, subscriptions)

import Html exposing (Html, Attribute, div, select, option, button, text)
import Html.Events exposing (on, onClick)
import Html.Attributes exposing (selected)
import Hanoi exposing (..)
import Json.Decode as Json exposing (map, at, int, object2, decodeString, (:=), Decoder)
import Json.Encode as Encode exposing (encode, object, int)
import WebSocket exposing (listen, send)


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


init : ( Model, Cmd Msg )
init =
    let
        status =
            Hanoi.init
    in
        ( Model status Nothing Nothing Nothing, Cmd.none )


pegToPosition : Peg -> Hanoi.Position
pegToPosition peg =
    case peg of
        First ->
            Hanoi.FirstPeg

        Second ->
            Hanoi.SecondPeg

        Third ->
            Hanoi.ThirdPeg


intToMaybePeg : Int -> Maybe Peg
intToMaybePeg i =
    case i of
        1 ->
            Just First

        2 ->
            Just Second

        3 ->
            Just Third

        _ ->
            Nothing


pegToInt : Peg -> Int
pegToInt peg =
    case peg of
        First ->
            1

        Second ->
            2

        Third ->
            3


type Msg
    = From (Maybe Peg)
    | To (Maybe Peg)
    | Error String
    | RequestMove Peg Peg
    | Move Peg Peg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        From maybePeg ->
            ( { model | from = maybePeg, message = Nothing }, Cmd.none )

        To maybePeg ->
            ( { model | to = maybePeg, message = Nothing }, Cmd.none )

        Error message ->
            ( { model | message = Just message }, Cmd.none )

        RequestMove from to ->
            let
                encodedJson =
                    Encode.encode 4
                        (Encode.object
                            [ ( "from", Encode.int (pegToInt from) )
                            , ( "to", Encode.int (pegToInt to) )
                            ]
                        )
            in
                ( model, WebSocket.send "ws://localhost:8080" encodedJson )

        Move from to ->
            ( performMove model.status from to, Cmd.none )


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
    Json.map intToMaybePeg
        (Json.at [ "target", "selectedIndex" ] Json.int)


tryMove : Model -> Msg
tryMove model =
    case ( model.from, model.to ) of
        ( Nothing, _ ) ->
            Error "You didn't specify the starting peg"

        ( _, Nothing ) ->
            Error "You didn't specify the ending peg"

        ( Just from, Just to ) ->
            RequestMove from to


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://localhost:8080" subscriptionStringToMessage


messageDecoder : Decoder ( Int, Int )
messageDecoder =
    object2 (,)
        ("from" := Json.int)
        ("to" := Json.int)


subscriptionStringToMessage : String -> Msg
subscriptionStringToMessage message =
    let
        result =
            decodeString messageDecoder message
    in
        case result of
            Err error ->
                Error error

            Ok fromTo ->
                let
                    maybeFirst =
                        intToMaybePeg (fst fromTo)

                    maybeSecond =
                        intToMaybePeg (snd fromTo)
                in
                    case ( maybeFirst, maybeSecond ) of
                        ( Just from, Just to ) ->
                            Move from to

                        _ ->
                            Error "The received message does not represent a valid move"
