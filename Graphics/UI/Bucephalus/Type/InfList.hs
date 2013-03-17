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
import Prelude (($), Maybe(..))

---------------------------------------------------------------------------------------------------
-- 型定義
---------------------------------------------------------------------------------------------------

data List a = List [a]

instance PRE.Functor List where
  fmap = map

---------------------------------------------------------------------------------------------------
-- リストに対する基本的な操作
---------------------------------------------------------------------------------------------------

fromList :: [a] -> List a
fromList x = List $ PRE.cycle x

toList :: List a -> [a]
toList (List x) = x 

map :: (a -> b) -> List a -> List b
map f (List x) = List $ PRE.map f x

take :: PRE.Int -> List a -> [a]
take i (List x) = PRE.take i x

head :: List a -> Maybe a
head (List []) = Nothing 
head (List x) = Just $ PRE.head x

tail :: List a -> List a
tail (List x) = List $ PRE.tail x

zip :: List a -> List b -> List (a, b)
zip (List x) (List y) = List $ PRE.zip x y

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f (List x) (List y) = List $ PRE.zipWith f x y
