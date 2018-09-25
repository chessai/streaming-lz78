{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streaming.LZ78
  ( LZ78(..)
  , Token(..)
  , encode

  ) where

import Data.Coerce
import Control.Comonad
import Data.Functor.Identity (Identity(runIdentity))
import Data.Hashable (Hashable(hashWithSalt))
import Streaming (Stream, lift)
import Streaming.Prelude (Of((:>)), next, yield)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Streaming.Prelude as S

data Token a = Token {-# UNPACK #-} !Int a
  deriving (Eq, Ord, Functor)

_getToken :: Token a -> a
_getToken (Token _ a) = a

instance Hashable a => Hashable (Token a) where
  hashWithSalt s (Token i a) = s `hashWithSalt` i `hashWithSalt` a

newtype LZ78 m a = LZ78 (Stream (Of (Token a)) m ())

instance (Comonad m, Monad m) => Foldable (LZ78 m) where
  foldMap f (LZ78 st) = go $ (extract . S.mconcat . S.map (f . _getToken)) st
    where
      _getToken (Token _ a) = a
      go (s S.:> _) = s

encode :: (Hashable a, Eq a, Monad m)
  => Stream (Of a) m ()
  -> LZ78 m a
encode st = LZ78 (encodeInternal st HashMap.empty 1 0)

encodeInternal :: (Hashable a, Eq a, Monad m)
  => Stream (Of a) m () -- stream to compress
  -> HashMap (Token a) Int -- empty hashmap, accumulator 
  -> Int -- 1
  -> Int -- 0
  -> Stream (Of (Token a)) m ()
encodeInternal st d f p = do
  e <- lift $ next st
  case e of
    Left _ -> pure ()
    Right (c, rest) -> do
      let t = Token p c
      case HashMap.lookup t d of
        Just p' -> encodeInternal st d f p'
        Nothing -> do
          yield t
          encodeInternal rest (HashMap.insert t f d) (f + 1) 0
