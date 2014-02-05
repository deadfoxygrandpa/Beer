module Randomize where

-- Project imports
import Model
import BeerList

-- Catalog imports
import Generator
import Generator.Standard

-- Basic generator additions
bool : Generator.Generator g -> (Bool, Generator.Generator g)
bool gen =
    let (x, gen') = Generator.int32Range (0, 1) gen
        int2Bool n = case n of
                        0         -> False
                        otherwise -> True
    in (int2Bool x, gen')

normal : Generator.Generator g -> (Float, Generator.Generator g)
normal gen =
    let makeXY gen = let (x, gen')  = Generator.floatRange (-1, 1) gen
                         (y, gen'') = Generator.floatRange (-1, 1) gen'
                         s = x^2 + y^2
                      in if (s < 1) then (x, y, s, gen'') else makeXY gen''
        (x, y, s, gen') = makeXY gen
        n = x * (sqrt <| (-2 * (logBase e s)) / s)
    in (n, gen')    

normal' : (Float, Float) -> Generator.Generator g -> (Float, Generator.Generator g)
normal' (mean, sigma) gen =
    let (x, gen') = normal gen
    in (x * sigma + mean, gen')

-- Randomized Person fields
sex : Generator.Generator g -> (Model.Sex, Generator.Generator g)
sex gen =
    let (x, gen') = bool gen
        bool2Sex b = if b then Model.Female else Model.Male
    in (bool2Sex x, gen')

bac : Generator.Generator g -> (Float, Generator.Generator g)
bac gen = 
    let (x, gen') = normal' (0, 0.05) gen
    in (clamp 0 100 x, gen')

weight : Model.Sex -> Generator.Generator g -> (Float, Generator.Generator g)
weight sex =
    let (mean, sigma) = case sex of
                            Model.Male   -> (75.7, 12.2)
                            Model.Female -> (64.9, 12.7)
    in normal' (mean, sigma)

urine : Generator.Generator g -> (Float, Generator.Generator g)
urine gen = 
    let (x, gen') = normal' (50, 35) gen
    in (clamp 0 1000 x, gen')

beer : Generator.Generator g -> (Model.Beer, Generator.Generator g)
beer gen =
    let n = length BeerList.allBeers
        (x, gen') = Generator.int32Range (0, n - 1) gen
    in (head . drop x <| BeerList.allBeers, gen')

person : Generator.Generator Generator.Standard.Standard -> (Model.Person, Generator.Generator Generator.Standard.Standard)
person gen =
    let (sex', gen1) = sex gen
        (bac', gen2) = bac gen1
        (weight', gen3) = weight sex' gen2
        alc = 0
        (urine', gen4) = urine gen3
        urinating = False
        wetSelf = False
        (beer', gen5) = beer gen4
    in (Model.Person sex' bac' weight' alc urine' urinating wetSelf (355, beer'), gen5)
