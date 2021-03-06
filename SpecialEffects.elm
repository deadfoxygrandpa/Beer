module SpecialEffects (distort, plain, fuzzy, theBest) where

import Transform2D
import Graphics.Collage exposing (..)
import Graphics.Element exposing (container, middle)


-- Project imports

import Model exposing (..)


shear : Float -> Transform2D.Transform2D
shear x =
    Transform2D.matrix 1 (0.2 * x) 0 1 1 1


scale : Float -> Transform2D.Transform2D
scale x =
    Transform2D.matrix x 0 0 x 1 1


trans : Float -> Float -> Transform2D.Transform2D
trans x y =
    Transform2D.multiply (shear x) (scale y)


distort : SpecialEffect
distort environment element =
    let
        ( w, h ) = environment.windowSize

        factor = drunkFactor environment.factor

        sx = factor

        sy = factor * 0.5

        x = environment.time

        y = environment.time
    in
        collage w h
            <| [ groupTransform (trans (sx * (sin <| x / 60)) ((0.95 + (sy * (sin <| y / 50))))) [ toForm element ] ]


plain : SpecialEffect
plain environment element =
    let
        ( w, h ) = environment.windowSize
    in
        container w h middle element


fuzzy : SpecialEffect
fuzzy environment element =
    let
        form = toForm element

        ( w, h ) = environment.windowSize

        t = environment.time

        factor = clamp 0 100 <| environment.factor - 1.0e-2

        x = 50 * factor * (sin <| t / 15)

        y = 20 * factor * (cos <| t / 10)
    in
        collage w h [ alpha 0.7 << move ( -x, y ) <| form, alpha 0.7 << move ( x, y ) <| form ]


theBest : SpecialEffect
theBest environment =
    distort environment << fuzzy environment


drunkFactor : Float -> Float
drunkFactor bac =
    bac ^ 2
