-- Page 885

{-# LANGUAGE InstanceSigs #-}

module Chapter23.State where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap aToB (Moi sToAS) =
    Moi $ \s ->
      let (a, s') = sToAS s
      in  (aToB a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) =
    Moi $ \s ->
      let
        (aToB, s') = f s
        (a, s'') = g s'
      in
        (aToB a, s'')

instance Monad (Moi s) where
  return :: a -> Moi s a
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) (Moi f) g =
    Moi $ \s ->
      let
        (a, s') = f s
        (Moi g') = g a
        (b, s'') = g' s'
      in
        (b, s'')