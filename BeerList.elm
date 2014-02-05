module BeerList where

import open Model

tsingtao = Beer "Tsingtao" "Tsingtao Brewery" PaleLager 4.3 (Score 6 49)
budweiser = Beer "Budweiser" "Anheuser-Busch InBev" PaleLager 5 (Score 0 3)
carlsberg = Beer "Carlsberg Lager" "Carlsberg Brewery" PaleLager 4.6 (Score 8 69)
londonPride = Beer "London Pride" "Fuller's" PremiumBitter 4.7 (Score 87 97)

allBeers : [Beer]
allBeers = [ tsingtao
           , budweiser
           , carlsberg
           , londonPride
           ]