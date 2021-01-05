{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Debug where

import Plugin.MagicTyFam
import GHC.TypeLits
import Data.Kind (Constraint)

class Debug a where
  debug :: a -> String

class Succeed a where
  succeed :: a -> String

instance Show a => Succeed a where
  succeed = show

