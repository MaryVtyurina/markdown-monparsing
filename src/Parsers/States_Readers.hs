{-#LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, DeriveFunctor, MonadComprehensions, InstanceSigs, ScopedTypeVariables,
            FunctionalDependencies, UndecidableInstances #-}
module States_Readers where

import Control.Monad (Monad, (>>=), (>=>), return, MonadPlus, mzero, mplus, ap)
import Control.Applicative (Applicative, Alternative, empty, (<|>))
import Prelude

---------------------------------------------------------------------------
--                    Type and class for readers (transformer form)
---------------------------------------------------------------------------
--ReaderM $ const $ StateM $ const []
newtype ReaderM m s a = ReaderM { unR :: s -> m a } deriving Functor

instance Monad m => Applicative (ReaderM m s) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (ReaderM m s) where
    return a  = ReaderM $ const $ return a
    r >>= f   = ReaderM bR where
        bR s = unR r s >>= bM s
        bM s a = unR (f a) s

class Monad m => ReaderMonad m s | m -> s
    where
        env    :: m s
        setenv :: s -> m a -> m a

instance Monad m => ReaderMonad (ReaderM m s) s
    where
        -- env :: ReaderM m s s
        env = ReaderM $ \s -> return s --результат вычислений
        -- setenv :: s -> ReaderM m s a -> ReaderM m s a
        setenv s srm = ReaderM $ \_ -> unR srm s -- замена текущего состояния новым результатом

instance MonadPlus m => Alternative (ReaderM m s) where
    empty     = ReaderM $ const mzero
    r1 <|> r2 = ReaderM $ \s -> unR r1 s <|> unR r2 s

instance StateMonad m a => StateMonad (ReaderM m s) a
    where
        update f   = ReaderM $ \_ -> update f

---------------------------------------------------------------------------
--                    Type and class for states (transformer form)
---------------------------------------------------------------------------
newtype StateM m s a = StateM { unS :: s -> m (a,s) } deriving Functor

instance Monad m => Applicative (StateM m s) where
    pure :: a -> StateM m s a
    pure = return
    (<*>) = ap

instance Monad m => Monad (StateM m s) where
-- result v :: a -> StateM m s a
    return v = StateM $ \s -> return (v, s)
-- bind :: StateM m s a -> (a -> StateM m s b) -> StateM m s b
    stm >>= f = StateM $
                    unS stm >=>
                        (\(a, s') -> (unS $ f a) s')

instance MonadPlus m => Alternative (StateM m s) where
    empty     = StateM $ const mzero
    s1 <|> s2 = StateM $ \s -> unS s1 s <|> unS s2 s

instance MonadPlus m => MonadPlus (StateM m s) where
    mzero = empty -- StateM $ const
    mplus = (<|>) -- StateM $ \s -> unS s1 s `mplus` unS s2 s

class Monad m => StateMonad m s | m -> s
  where
      update :: (s -> s) -> m s
      set    :: s -> m s
      fetch  :: m s
      set s   = update $ const s
      fetch   = update id

instance Monad m => StateMonad (StateM m s) s where
      -- update :: Monad m => (s -> s) -> StateM m s s
      update f =  StateM $ \s -> return (s, f s)

-- newtype I a = I a
-- type State s a = StateM I s a -- non-transformer -- State { unS :: s -> (a,s) }
