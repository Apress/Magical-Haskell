{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.MRWS
(MRWST,
runMRWST,
evalMRWST,
execMRWST,
ask,
asks,
tell,
get,
gets,
modify,
R.lift,
R.liftIO)
where

import Control.Concurrent.STM
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import qualified Control.Monad.Reader as R
import Data.Kind (Type)
import Data.Functor ((<&>))
import Data.Bifunctor (Bifunctor(first))
import Conduit (MonadTrans)
import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)

-- definition of the MRWST monad
newtype MRWST r w s (m :: Type -> Type) a =
    MRWST { fakeRunMRWST :: R.ReaderT (r, TVar (s,w)) m a }
    deriving (Applicative,
    Functor,
    Monad,
    MonadThrow,
    MonadTrans,
    MonadCatch,
    MonadMask,
    R.MonadIO,
    R.MonadReader (r, TVar (s,w)),
    MonadUnliftIO)

-- Running to the underlying monad:

runMRWST :: (R.MonadIO m, Monoid w) =>
    MRWST r w s m a -> r -> s -> m (a, s, w)
runMRWST act settings state = do
    sw <- R.liftIO $ newTVarIO (state, mempty)
    fin <- R.runReaderT (fakeRunMRWST act) (settings, sw)
    sw' <- R.liftIO $ readTVarIO sw
    pure (fin, fst sw', snd sw')

evalMRWST :: (R.MonadIO m, Monoid w) =>
    MRWST r w s m a -> r -> s -> m (a, w)
evalMRWST act settings state = do
    sw <- R.liftIO $ newTVarIO (state, mempty)
    fin <- R.runReaderT (fakeRunMRWST act) (settings, sw)
    sw' <- R.liftIO $ readTVarIO sw
    pure (fin, snd sw')

execMRWST :: (R.MonadIO m, Monoid w) =>
    MRWST r w s m a -> r -> s -> m (s, w)
execMRWST act settings state = do
    sw <- R.liftIO $ newTVarIO (state, mempty)
    _ <- R.runReaderT (fakeRunMRWST act) (settings, sw)
    R.liftIO $ readTVarIO sw

_modify :: R.MonadIO m => ((s,w) -> (s,w)) -> MRWST r w s m ()
_modify f = R.ask >>= R.liftIO . atomically . flip modifyTVar' f . snd

-- READER INTERFACE is semi-AUTOMATIC thanks to deriving MonadReader --
-- But we need to extract and manipulate with data a bit to make it
-- "really" like Reader:

ask :: Monad m => MRWST r w s m r
ask = R.asks fst

asks :: Monad m => (r -> a) -> MRWST r w s m a
asks sel = ask <&> sel

-- WRITER INTERFACE --
tell :: (R.MonadIO m, Semigroup w) => w -> MRWST r w s m ()
tell wrt = _modify (\(s,w) -> (s, w <> wrt))

-- STATE INTERFACE -- 
get :: R.MonadIO m => MRWST r w s m s
get = (R.ask >>= R.liftIO . readTVarIO . snd) <&> fst

gets :: R.MonadIO m => (s -> a) -> MRWST r w s m a
gets sel = sel <$> get

modify :: R.MonadIO m => (s -> s) -> MRWST r w s m ()
modify func = _modify (first func)




