module Data.Time.Doomsday.State.Simple (
    State (..),
) where

newtype State s a = State { runState :: s -> (s, a) }
  deriving (Functor)

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (s, a)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  State f <*> State g = State $ \s -> 
    let (s1, h) = f s
    in let (s2, a) = g s1
    in (s2, h a)

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State f >>= g = State $ \s -> let (s1, a) = f s in ($ s1) . runState $ g a
