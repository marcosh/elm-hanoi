module Main exposing (..)

import Html.App as App
import Hanoi


main : Program Never
main =
    App.beginnerProgram
        { model = Hanoi.init
        , update = Hanoi.update
        , view = Hanoi.view
        }
