module Randomize where

-- Catalog imports
import Generator
import Generator.Standard

gen : Generator.Generator Generator.Standard.Standard
gen = Generator.Standard.generator 1

makePerson : Generator.Generator Generator.Standard.Standard -> (Model.Person, Generator.Generator Generator.Standard.Standard)
makePerson gen =
    let (sex, gen') = Generator.int32Range (0, 10) gen
    in (Model.Person (if (sex <= 0) then Model.Male else Model.Female) 0 (toFloat sex) 0 0 False False (355, badBeer), gen')