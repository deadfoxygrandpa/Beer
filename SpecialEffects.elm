module SpecialEffects (distort, plain) where

-- Standard Library imports
import Transform2D

-- Project imports
import Model

shear : Float -> Transform2D.Transform2D
shear x = Transform2D.matrix 1 (0.2 * x) 0 1 1 1

scale : Float -> Transform2D.Transform2D
scale x = Transform2D.matrix x 0 0 x 1 1

trans : Float -> Float -> Transform2D.Transform2D
trans x y = Transform2D.multiply (shear x) (scale y)

distort : Int -> Int -> Float -> Float -> Float -> Float -> Element -> Element
distort w h x y sx sy element = collage w h <| 
    [ groupTransform (trans (sx * (sin <| x/60)) ((0.95 + (0.5 * (sin <| sy * y/10))))) [toForm element] ]

plain : Int -> Int -> Float -> Float -> Float -> Float -> Element -> Element
plain w h x y sx sy element = collage w h [toForm element]

distort2 : Model.Environment -> Element -> Element
distort2 environment element = 
    let (w, h) = environment.windowSize
        sx = environment.state.person.bac
        sy = environment.state.person.bac
        x = environment.time
        y = environment.time
    in collage w h <|
    [ groupTransform (trans (sx * (sin <| x/60)) ((0.95 + (0.5 * (sin <| sy * y/10))))) [toForm element] ]

fuzzy : Model.Environment -> Element -> Element
fuzzy environment element = 
    let form = toForm element
        t = environment.time
        bac = environment.state.person.bac
        x = 50 * bac * (sin <| t / 15)
        y = 20 * bac * (cos <| t / 10)
    in collage 500 500 [ alpha 0.7 . move (-x, y) <| form, alpha 0.7 . move (x, y) <| form ] 