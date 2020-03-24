{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module CleanShow (CleanShow(..)) where

class Show a => CleanShow a where
  cleanShow :: a -> String
  cleanShow = show

instance CleanShow String where
  cleanShow = id

instance CleanShow Int


instance (CleanShow a, CleanShow b) => CleanShow (a, b) where
  cleanShow (a, b) = "(" ++ cleanShow a ++ ", " ++ cleanShow b ++ ")"

instance (CleanShow a, CleanShow b, CleanShow c) => CleanShow (a, b, c) where
  cleanShow (a, b, c) = "(" ++ cleanShow a ++ ", " ++ cleanShow b ++ ", " ++ cleanShow c ++ ")"
