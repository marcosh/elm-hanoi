module WebsocketMain exposing (main)

import Html.App as App exposing (program)
import WebsocketHanoiView


main : Program Never
main =
    App.program
        { init = WebsocketHanoiView.init
        , update = WebsocketHanoiView.update
        , view = WebsocketHanoiView.view
        , subscriptions = WebsocketHanoiView.subscriptions
        }
