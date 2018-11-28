module Brainfsck.Types
    ( Expr(..)
    , Tape(..)
    , initialTape
    ) where

data Expr
    = GoRight -- increment the data pointer (to point to the next cell to the right).
    | GoLeft  -- decrement the data pointer (to point to the next cell to the left).
    | Incr    -- increment (increase by one) the byte at the data pointer.
    | Decr    -- decrement (decrease by one) the byte at the data pointer.
    | Out     -- output the byte at the data pointer.
    | In      -- accept one byte of input, storing its value in the byte at the data pointer.
    | Loop [Expr]
    deriving (Show, Eq)

-- if the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
-- if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command.

data Tape = Tape
    { left  :: [Int]
    , head  :: Int
    , right :: [Int]
    }
    deriving (Show, Eq)

initialTape :: Tape
initialTape = Tape (repeat 0) 0 $ repeat 0
