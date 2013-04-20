module Graphics.UI.Bucephalus.Type.InfList(
  --型構成子をエクスポートしない事で安全性を保持
  List,
  --操作するための関数
  fromList, 
  toList, 
  map,
  take,
  head,
  tail,
  zip,
  zipWith) where

--Preludeの隠蔽
import qualified Prelude as PRE
import Prelude (($), (>>=), (.), Maybe(..))

---------------------------------------------------------------------------------------------------
-- Definition of data type
---------------------------------------------------------------------------------------------------

-- | This data type guarantees it to be a list of infinity.
data List a = List [a]

instance PRE.Functor List where
  fmap = map
instance PRE.Monad List where
  return x = List $ PRE.repeat x
  List x >>= f = List $ x >>= toList . f  

---------------------------------------------------------------------------------------------------
-- Simply functions for InfList
---------------------------------------------------------------------------------------------------

-- | The @fromList@ function provides convert from @[a]@ to @InfList.List a@.
fromList :: [a] -> a -> List a
fromList [] d = List $ PRE.repeat d
fromList x  d = List $ PRE.cycle x

-- | The @toList@ function provides convert from @InfList.List@ to @[]@.
toList :: List a -> [a]
toList (List x) = x 

-- | The @map@ function for @InfList.List@.
map :: (a -> b) -> List a -> List b
map f (List x) = List $ PRE.map f x

-- | The @take@ function for @InfList.List@
take :: PRE.Int -> List a -> [a]
take i (List x) = PRE.take i x

-- | The @head function for @InfList.List@.
head :: List a -> Maybe a
head (List []) = Nothing 
head (List x) = Just $ PRE.head x

-- | The @tail@ function for @InfList.List@.
tail :: List a -> List a
tail (List x) = List $ PRE.tail x

-- | The @zip@ function for @InfList.List@.
zip :: List a -> List b -> List (a, b)
zip (List x) (List y) = List $ PRE.zip x y

-- | The @zipWith@ function for @InfList.List@.
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f (List x) (List y) = List $ PRE.zipWith f x y
