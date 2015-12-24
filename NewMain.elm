module Main (..) where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Game
import Time
import Task
import Effects exposing (Effects, Never)
import Signal


app =
    StartApp.start
        { init = ( model, Effects.none )
        , view = view
        , update = update
        , inputs = [ Signal.map Game.Tick <| Time.fps 10 ]
        }


main =
    app.html


port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks


type alias Model =
    { game : Game.Model }


model =
    Game.init seed


view address model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "right", "50%" )
            ]
        ]
        [ controls
        , Game.view address model
        ]


type alias Action =
    Game.Action


update action model =
    ( Game.update action model, Effects.none )


controls : Html
controls =
    div
        [ style
            [ ( "position", "fixed" )
            , ( "left", "1%" )
            , ( "top", "1%" )
            ]
        ]
        [ div
            [ style
                [ ( "position", "absolute" )
                , ( "left", "0%" )
                , ( "bottom", "50%" )
                ]
            ]
            [ Markdown.toHtml "##controls:" ]
        , div
            [ style
                []
            ]
            [ Markdown.toHtml """
- spacebar: sip beer
- g: gulp some beer
- c: chug beer (slam back a brewski)
- u: urinate
- a: order another beer
- o: order from the menu
- ctrl + w: win game
- clickar buttons: self explanatory
- p: pause game
"""
            ]
        ]


seed : Int
seed =
    1000
