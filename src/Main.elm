module Main exposing (main)

import Html.App as App exposing (beginnerProgram)
import HanoiView


main : Program Never
main =
    App.beginnerProgram
        { model = HanoiView.init
        , update = HanoiView.update
        , view = HanoiView.view
        }
