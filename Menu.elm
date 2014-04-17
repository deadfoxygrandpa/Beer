module Menu where

import BeerList

data Zipper x = Zipper [x] x [x]

left : Zipper x -> Maybe (Zipper x)
left (Zipper a x b) = if (length a == 0) then Nothing
    else Just <| Zipper (tail a) (head a) (x :: b)

right : Zipper x -> Maybe (Zipper x)
right (Zipper a x b) = if (length b == 0) then Nothing
    else Just <| Zipper (x :: a) (head b) (tail b)

select : Zipper x -> x
select (Zipper _ x _) = x

getLeft : Zipper x -> [x]
getLeft (Zipper a _ _) = a

getRight : Zipper x -> [x]
getRight (Zipper _ _ b) = b

fromList : [x] -> Zipper x
fromList (x::xs) = Zipper [] x xs

toList : Zipper x -> [x]
toList (Zipper a x b) = reverse a ++ [x] ++ b

type Menu = Zipper String

menu : Menu
menu = fromList <| map .name  BeerList.allBeers

update : (Menu -> Menu) -> Menu -> Menu
update step menu = step menu

moveUp : a -> Menu -> Menu
moveUp _ menu = maybe menu id <| left menu

moveDown : a -> Menu -> Menu
moveDown _ menu = maybe menu id <| right menu

render : Menu -> (Int, Int) -> Element
render menu (w, h) = container w h midLeft <|
    flow down <| map plainText (reverse <| getLeft menu) ++ [centered . bold . toText <| select menu] ++ map plainText (getRight menu)
