
module Graphics.UI.Bucephalus.Base.Object(
  ObjectState(..)
  ) where
import Graphics.UI.Bucephalus.Type.Collision
import Graphics.UI.Bucephalus.Type.Surface

---------------------------------------------------------------------------------------------------
-- 型宣言
---------------------------------------------------------------------------------------------------


-- ObjectState型クラス ゲーム内で状態として受け渡されるキャラクターや障害物のようなオブジェクト
class ObjectState o where
  drawObject :: GameField -> o ->  IO ()

---------------------------------------------------------------------------------------------------
-- TODO アクションRPG等を作る事を考えた時、当たり判定枠が一定にはならないので
-- CollisionSetのコレクションを扱う型クラスを考える
---------------------------------------------------------------------------------------------------


-- *******************************************************
-- *** オブジェクトそのものに当たり判定は無い　***
-- *******************************************************
-----------------------------------------------------------------------------------------------------
---- 最低限の機能を持つObjectを標準で提供
-----------------------------------------------------------------------------------------------------
--
--data StandardObjectState t = StandardObjectState {
--  objectShape :: Shape,
--  objectType :: t
--  } deriving (Show, Read)
--
----当たり判定関連の型クラスインスタンス
--instance Collision (StandardObjectState t) where
--  l `collision` r = objectShape l `collision` objectShape r
--instance CollisionType t => CollisionType (StandardObjectState t) where
--  l `canOverlap` r = objectType l `canOverlap` objectType r
--
----オブジェクト型クラスのインスタンス
--instance (Show t, CollisionType t) => ObjectState (StandardObjectState t) where
--  drawObject _ o = putStrLn $ "Draw Object : " ++ show o
