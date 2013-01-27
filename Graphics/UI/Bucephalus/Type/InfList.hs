module Graphics.UI.Bucephalus.Type.InfList(
  List,
  fromList, 
  toList, 
  map,
  take,
  head,
  tail,
  zip,
  zipWith) where

import qualified Prelude as PRE
import Prelude (($))

data List a = List [a]
instance PRE.Functor List where
  fmap = map

fromList :: [a] -> List a
fromList x = List $ PRE.cycle x

toList :: List a -> [a]
toList (List x) = x 

map :: (a -> b) -> List a -> List b
map f (List x) = List $ PRE.map f x

take :: PRE.Int -> List a -> [a]
take i (List x) = PRE.take i x

head :: List a -> a
head (List x) = PRE.head x

tail :: List a -> List a
tail (List x) = List $ PRE.tail x

zip :: List a -> List b -> List (a, b)
zip (List x) (List y) = List $ PRE.zip x y

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f (List x) (List y) = List $ PRE.zipWith f x y
