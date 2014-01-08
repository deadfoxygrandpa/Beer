module SpecialEffects (distort, plain) where

import Transform2D

shear : Float -> Transform2D.Transform2D
shear x = Transform2D.matrix 1 (0.2 * x) 0 1 1 1

scale : Float -> Transform2D.Transform2D
scale x = Transform2D.matrix x 0 0 x 1 1

trans : Float -> Float -> Transform2D.Transform2D
trans x y = Transform2D.multiply (shear x) (scale y)

distort : Int -> Int -> Float -> Float -> Float -> Float -> Element -> Element
distort w h x y sx sy element = collage w h <| 
    [ groupTransform (trans (sx* (sin <| x/60)) ((0.95 + (0.5 * (sin <| sy * y/10))))) [toForm element] ]

plain : Int -> Int -> Float -> Float -> Float -> Float -> Element -> Element
plain w h x y sx sy element = collage w h [toForm element]