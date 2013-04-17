module Graphics.UI.Bucephalus.Type.Events where

data BucephalusEvent
  = BucephalusKeyDown BucephalusKey
  | BucephalusKeyUp  BucephalusKey
  | BucephalusQuit 
  | BucephalusNoEvent
  deriving (Show, Read, Eq)

data BucephalusKey
  = BucephalusKey_Esc 
  | BucephalusKey_Up
  | BucephalusKey_Down
  | BucephalusKey_Left
  | BucephalusKey_Right
  | BucephalusKey_a
  | BucephalusKey_b
  | BucephalusKey_c
  | BucephalusKey_d
  | BucephalusKey_e
  | BucephalusKey_f
  | BucephalusKey_g
  | BucephalusKey_h
  | BucephalusKey_i
  | BucephalusKey_j
  | BucephalusKey_k
  | BucephalusKey_l
  | BucephalusKey_m
  | BucephalusKey_n
  | BucephalusKey_o
  | BucephalusKey_p
  | BucephalusKey_q
  | BucephalusKey_r
  | BucephalusKey_s
  | BucephalusKey_t
  | BucephalusKey_u
  | BucephalusKey_v
  | BucephalusKey_w
  | BucephalusKey_x
  | BucephalusKey_y
  | BucephalusKey_z
  | BucephalusKey_Return
  deriving (Show, Read, Eq)
