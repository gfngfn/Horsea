module Util.Elaborator
  ( Elaborator,
    liftEither,
    getState,
    putState,
    askConfig,
    raiseError,
    mapError,
    run,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Either.Extra (mapLeft)
import Data.Tuple.Extra (first)
import Prelude

type Impl state config err trav a = StateT state (Reader config) (Either (err, trav) a)

newtype Elaborator state config err trav a = Elab (Impl state config err trav a)

mapRightOfM :: (a -> Impl state config err trav b) -> Either (err, trav) a -> Impl state config err trav b
mapRightOfM f = \case
  Left e -> lift $ pure $ Left e
  Right vx -> f vx

flipFmapImpl :: Impl state config err trav a -> (a -> b) -> Impl state config err trav b
flipFmapImpl sx vf = sx >>= mapRightOfM (pure . Right . vf)

instance Functor (Elaborator state config err trav) where
  fmap vf (Elab sx) = Elab $ flipFmapImpl sx vf

instance Applicative (Elaborator state config err trav) where
  pure v = Elab $ pure $ Right v
  (Elab sf) <*> (Elab sx) = Elab $ sf >>= mapRightOfM (flipFmapImpl sx)

instance Monad (Elaborator state config err trav) where
  (Elab sx) >>= f = Elab $ sx >>= mapRightOfM (\v -> let Elab s' = f v in s')

liftEither :: Either (err, trav) a -> Elaborator state config err trav a
liftEither x = Elab $ pure x

getState :: Elaborator state config err trav state
getState = Elab $ Right <$> get

putState :: state -> Elaborator state config err trav ()
putState st = Elab $ Right <$> put st

askConfig :: Elaborator state config err trav config
askConfig = Elab $ lift $ Right <$> ask

raiseError :: trav -> err -> Elaborator state config err trav a
raiseError trav e = Elab $ lift $ pure $ Left (e, trav)

mapError :: (err1 -> err2) -> Elaborator state config err1 trav a -> Elaborator state config err2 trav a
mapError f (Elab s) = Elab $ mapStateT (mapReader (first (mapLeft (first f)))) s

run :: Elaborator state config err trav a -> config -> state -> (Either (err, trav) a, state)
run (Elab checker) config st = runReader (runStateT checker st) config

-- runStateT :: StateT s m b -> s -> m (b, s)
-- s = state
-- m = Reader config
-- b = Either (err, trav) a
