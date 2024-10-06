{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Nodes.Arrows
where

import Control.Category ( Category(..) )
import Control.Arrow
import Control.Monad ( (>=>), liftM2 )

data Node m e a b = Node {
    runNode  :: a -> m (Either e b),
    defaults :: m (Maybe b)
}

instance (Monad m, Monoid e) => Category (Node m e) where
    id :: Node m e a a
    id = Node (pure Prelude.. Right) (pure Nothing)

    (.) :: Node m e b c -> Node m e a b -> Node m e a c
    Node fbc dc . Node fab db = Node {
        defaults = dc,
        runNode = \xa -> fab xa >>= \rb -> -- executing first function first
            case rb of -- analyzing its result
                Left err -> db >>= \db' -> -- if there's error then:
                    case db' of
                        -- no defaults? Just pass the error
                        Nothing -> pure (Left err)
                        -- defaults are present? apply next function to them
                        (Just xb) -> fbc xb
                -- of there's no error just apply the next one to the results
                Right xb -> fbc xb
    }

instance (Monad m, Monoid e) => Arrow (Node m e) where
    arr :: (b -> c) -> Node m e b c
    arr f = Node {
        runNode = \x -> pure (Right $ f x),
        defaults = pure Nothing
    }

    first :: Node m e b c -> Node m e (b, d) (c, d)
    first (Node fbc dc) = Node {
        runNode = \(xb, xd) -> fbc xb >>= \xc -> -- apply function first
            case xc of
                -- if error:
                Left err -> dc >>= \dc'->
                    case dc' of
                        -- if no defaults - translate error
                        Nothing -> pure (Left err)
                        -- otherwise - translate defaults
                        (Just dcc) -> pure (Right (dcc, xd))
                -- if no error: return the result
                Right xcc -> pure (Right (xcc, xd)),
        -- this might be questionable, but we have no way of knowing
        -- which defaults to give to the "d" parameter, so
        -- even if we have defaults for "c", we assume there's no defaults 
        -- going forward.
        -- have to test on different pipeline configurations
        defaults = pure Nothing
    }

    -- Split the input between the two argument arrows and combine their output. 
    -- redefining because we need to treat the defaults correctly!
    (***) :: Node m e b c -> Node m e b' c' -> Node m e (b, b') (c, c')
    (Node fbc dc) *** (Node fb'c' dc') = Node {
        -- if we have both defaults, combine; if not - Nothing
        defaults = dc >>= \dcm -> liftM2 (,) dcm <$> dc',
        runNode = \(xb, xb') -> do
            resc  <- fbc xb
            resc' <- fb'c' xb'
            case resc of
                Right rc -> 
                    case resc' of
                        Right rc' -> pure $ Right (rc, rc')
                        Left err  -> pure $ Left err
                Left err -> pure $ Left err
    }
    
    -- Fanout: send the input to both argument arrows and combine their output.
    -- redefining because we need to treat the defaults correctly!
    (&&&) :: Node m e b c -> Node m e b c' -> Node m e b (c, c')
    (Node fbc dc) &&& (Node fbc' dc') = Node {
        -- same as *** !
        defaults = dc >>= \dcm -> liftM2 (,) dcm <$> dc',
        runNode = \xb -> do
            resc  <- fbc xb
            resc' <- fbc' xb
            case resc of
                Right rc -> 
                    case resc' of
                        Right rc' -> pure $ Right (rc, rc')
                        Left err  -> pure $ Left err
                Left err -> pure $ Left err
    }


