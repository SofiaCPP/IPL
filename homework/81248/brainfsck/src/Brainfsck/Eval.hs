{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Brainfsck.Eval
    ( eval
    ) where

import           Control.Monad (void, foldM)

import           Data.Char (chr, ord)

import           Brainfsck.Types

(?) :: (a -> b -> c) -> b -> a -> c
(?) = flip

-- We know we never reach [], because we only call this with infinite tapes.
operTape :: Expr -> Tape -> IO Tape
operTape GoRight   (Tape ls x (r:rs)) = pure $ Tape (x:ls) r rs
operTape GoLeft    (Tape (l:ls) x rs) = pure $ Tape ls l (x:rs)
operTape Incr      (Tape ls x rs)     = pure $ Tape ls (succ x) rs
operTape Decr      (Tape ls x rs)     = pure $ Tape ls (pred x) rs
operTape Out       t@(Tape _  x _)    = t <$ do putChar $ chr x
operTape In        (Tape ls _ rs)     = (Tape ls ? rs) . ord <$> getChar
operTape l@(Loop xs) t@(Tape _ x _)   =
    if x == 0 then pure t
              else do
        tn@(Tape _ x' _) <- evalTape xs t
        if x' /= 0 then operTape l tn
                   else pure tn

evalTape :: [Expr] -> Tape -> IO Tape
evalTape xs t = foldM (flip operTape) t xs

eval :: [Expr] -> IO ()
eval xs = void $ evalTape xs initialTape
