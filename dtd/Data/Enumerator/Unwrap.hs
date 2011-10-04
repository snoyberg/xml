{-# LANGUAGE RankNTypes #-}
module Data.Enumerator.Unwrap
    ( unwrapEnumeratee
    , unwrapStep
    , unwrapIter
    , liftStep
    ) where

import Control.Monad.Trans.Class (MonadTrans)
import Control.Exception (Exception, toException)
import Data.Enumerator (Enumeratee, Step (..), Iteratee (..), liftTrans)
import Control.Monad (liftM)

-- FIXME this doesn't thread state

unwrapEnumeratee :: (Exception e, MonadTrans t, Monad m, Monad (t m))
                 => (forall x. t m x -> m (Either e x))
                 -> Enumeratee ao ai (t m) b
                 -> Enumeratee ao ai m b
unwrapEnumeratee unwrap orig step =
    unwrapIter unwrap $ liftM (unwrapStep unwrap) $ orig $ liftStep step

liftStep :: (MonadTrans t, Monad m, Monad (t m))
         => Step a m b
         -> Step a (t m) b
liftStep (Yield b s) = Yield b s
liftStep (Error e) = Error e
liftStep (Continue k) = Continue $ liftTrans . k

unwrapStep :: (Monad m, Exception e)
           => (forall x. t m x -> m (Either e x))
           -> Step a (t m) b
           -> Step a m b
unwrapStep _ (Yield b s) = Yield b s
unwrapStep _ (Error e) = Error e
unwrapStep unwrap (Continue k) = Continue $ unwrapIter unwrap . k

unwrapIter :: (Monad m, Exception e)
           => (forall x. t m x -> m (Either e x))
           -> Iteratee a (t m) b
           -> Iteratee a m b
unwrapIter unwrap (Iteratee mstep) = Iteratee $ do
    estep <- unwrap mstep
    case estep of
        Left e -> return $ Error $ toException e
        Right x -> return $ unwrapStep unwrap x
