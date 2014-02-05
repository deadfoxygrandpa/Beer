module Randomize where

-- Project imports
import Model
import BeerList

-- Catalog imports
import Generator
import Generator.Standard


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

sex : Generator.Generator g -> (Model.Sex, Generator.Generator g)
sex gen =
    let (x, gen') = bool gen
        bool2Sex b = if b then Model.Female else Model.Male
    in (bool2Sex x, gen')



gen : Generator.Generator Generator.Standard.Standard
gen = Generator.Standard.generator 1


badBeer : Model.Beer
badBeer = BeerList.tsingtao

makePerson : Generator.Generator Generator.Standard.Standard -> (Model.Person, Generator.Generator Generator.Standard.Standard)
makePerson gen =
    let (sex, gen') = Generator.int32Range (0, 10) gen
    in (Model.Person (if (sex <= 0) then Model.Male else Model.Female) 0 (toFloat sex) 0 0 False False (355, badBeer), gen')