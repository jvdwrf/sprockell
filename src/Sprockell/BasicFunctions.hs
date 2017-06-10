{-# LANGUAGE FlexibleInstances #-}
module Sprockell.BasicFunctions where
import qualified Data.Array    as Array
import qualified Data.Sequence as Sequence
import qualified Data.Foldable as Foldable

-- ==========================================================================================================
-- Some elementary constants and Functions
-- ==========================================================================================================
reg0          = 0    :: Int                                 -- names for registers. reg0 is ALWAYS 0
regSprID      = 1    :: Int                                 -- regSprID: contains the sprockellID
regA          = 2    :: Int                                 -- registers A-F for other usage
regB          = 3    :: Int
regC          = 4    :: Int
regD          = 5    :: Int
regE          = 6    :: Int
regF          = 7    :: Int
regSP         = regbankSize
regPC         = regbankSize + 1

-- defines the number of registers excluding the stack pointer & program counter
regbankSize   = 8    :: Int

intBool True  = 1                                               -- Bool-to-Int
intBool False = 0

(+>>) :: a -> [a] -> [a]
x +>> xs = [x] ++ init xs                                       -- shift value into buffer
(<<+) :: [a] -> a -> [a]
xs <<+ x = tail xs ++ [x]

($>) :: (a->b) -> [a] -> [b]
f  $>  xs = map f xs

(|$|) :: [a->b] -> [a] -> [b]
fs |$| xs = zipWith (\f x -> f x) fs xs                         -- parallel application of a list of functions
                                                                -- to an equally long list of arguments

class Memory m where
    fromList :: [a] -> m a
    toList   :: m a -> [a]
    (!)   :: m a -> Int -> a                                    -- indexing
    (<~)  :: m a -> (Int,a) -> m a                              -- mem <~ (i,x): put value x at address i in mem

(<~!) :: Memory m => m a -> (Int,a) -> m a                      -- ibid, but leave address 0 unchanged
xs <~! (i,x)    | i == 0        = xs
                | otherwise     = xs <~ (i,x)


instance Memory [] where
    fromList = id
    toList   = id
    xs ! i = xs !! i
    []     <~ _     = []                                        -- silently ignore update after end of list
    (x:xs) <~ (0,y) = y:xs
    (x:xs) <~ (n,y) = x : (xs <~ (n-1,y))

instance Memory (Array.Array Int) where
    fromList xs = Array.listArray (0,length xs) xs
    toList = Array.elems
    (!) = (Array.!)
    xs <~ (i,x) = xs Array.// [(i,x)]

instance Memory Sequence.Seq where
    fromList = Sequence.fromList
    toList = Foldable.toList
    (!)  = Sequence.index
    xs <~ (i,x) = Sequence.update i x xs

x ∈ xs =  x `elem` xs

concatWith :: a -> [[a]] -> [a]
concatWith x [] = []                                            -- concats a list of lists with a "gluing element" x
concatWith x [y] = y
concatWith x (y:ys) = y ++ [x] ++ concatWith x ys

ljustify n x = x ++ replicate (n - length x) ' '                -- adds spaces upto n positions for outlining;
rjustify n x = replicate (n - length x) ' ' ++ x                -- may be used for your own show-function
