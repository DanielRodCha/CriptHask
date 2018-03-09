{-
	Copyright (C) 2010 Dr. Alistair Ward

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Miscellaneous polymorphic list-operations.
-}

module ToolShed.Data.List(
-- * Types
-- ** Type-synonyms
	ChunkLength,
	Matches,
-- * Functions
	chunk,
	excise,
	equalityBy,
	findConvergence,
	findConvergenceBy,
	linearise,
	merge,
	mergeBy,
	nub',
--	nubWithInt,
	permutations,
	permutationsBy,
	takeUntil,
	showListWith,
--	showListWithChar,
--	showListWithString
) where

import qualified	Control.Arrow
import			Control.Arrow((&&&))
import qualified	Data.IntSet
import qualified	Data.List
import qualified	Data.Set

-- | The length of the chunks into which a list is split.
type ChunkLength	= Int

{- |
	* Splits a list into chunks of the specified length.

	* The last chunk will be shorter, if the chunk-length isn't an aliquot part of the input list-length.

	* If the chunk-length is zero, the resulting list will be an infinite sequence of null lists.

	* CAVEAT: a similar function is available in the module /Data.List.Split/, though this one checks for @(chunkLength < 0)@.
-}
chunk
	:: ChunkLength
	-> [a]	-- ^ The polymorphic input list to be chunked.
	-> [[a]]
chunk size list
	| size < 0	= error $ "ToolShed.Data.List.chunk:\tnegative chunk-size=" ++ show size
	| otherwise	= chunk' list
	where
		chunk' :: [a] -> [[a]]
		chunk' []	= []
		chunk' a	= uncurry (:) . Control.Arrow.second chunk' $ splitAt size a

-- | Remove the single indexed element from the list.
excise
	:: Int	-- ^ The index.
	-> [a]	-- ^ The polymorphic input list.
	-> [a]	-- ^ The same list, with the indexed element removed.
excise 0	= tail	-- Just for efficiency.
excise i	= uncurry (++) . Control.Arrow.second tail . splitAt i

-- | The type of function required by 'findConvergenceBy', 'permutationsBy'.
type Matches a	= a -> a -> Bool

-- | A convenient way to compose the 'Matches'-function required by 'findConvergenceBy' & 'permutationsBy'.
equalityBy :: Eq b => (a -> b) -> Matches a
equalityBy f x y	= f x == f y

-- | Take the first element from the (potentially infinite) list, which matches the subsequent element, according to the specified function.
findConvergenceBy :: Matches a -> [a] -> a
findConvergenceBy _ []	=  error "ToolShed.Data.List.findConvergenceBy:\ta null list is too short for convergence to exist"
findConvergenceBy _ [_]	=  error "ToolShed.Data.List.findConvergenceBy:\ta singleton list is too short for convergence to exist"
findConvergenceBy matches l
	| null l'	= error "ToolShed.Data.List.findConvergenceBy:\tno convergence found"
	| otherwise	= fst $ head l'
	where
		l'	= dropWhile (not . uncurry matches) . uncurry zip $ (init &&& tail) l

-- | A specific instance of 'findConvergenceBy'.
findConvergence :: Eq a => [a] -> a
findConvergence	= findConvergenceBy (==)

{- |
	* The list of all permutations, generated by selecting any one datum from each sub-list in turn, from the specified list of lists.

	* As each item is selected, the remaining lists are filtered according to the specified 'Matches'-function.

	* Thus '/=' could be used to select a different item from each list.
-}
permutationsBy :: Matches a -> [[a]] -> [[a]]
permutationsBy matches lists
	| any null lists	= []	-- Required for efficiency, to catch the case [bigList1, bigList2 ... null]
	| otherwise		= slave lists
	where
		slave (xs : xss)	= [x : xs' | x <- xs, xs' <- slave $ map (filter $ matches x) xss]
		slave []		= [[]]

{- |
	* The list of all permutations, generated by selecting any one datum from each sub-list in turn, from the specified list of lists.

	* A specific instance of 'permutationsBy', in which no filtering of subsequent lists is performed after each item is selected.

	* NB: differs from 'Data.List.permutations', which selects items from a single input list.
-}
permutations :: [[a]] -> [[a]]
permutations	= permutationsBy (\_ _ -> True)

{-# NOINLINE nub' #-}
{-# RULES "nub'/Int" nub' = nubWithInt #-}

{- |
	* A strict version of 'Data.List.nub' with better time-complexity.

	* CAVEAT: the specified list must be finite, since the entire set is constructed before streaming to a list.

	* CAVEAT: it sorts the output as a side-effect, & consequently it requires a type which implements 'Ord'.
-}
nub' :: Ord a => [a] -> [a]
nub'	= Data.Set.toList . Data.Set.fromList

-- | A specialisation for type 'Int'.
nubWithInt :: [Int] -> [Int]
nubWithInt	= Data.IntSet.toList . Data.IntSet.fromList

-- | Converts a list of /Pairs/, into a narrower list.
linearise :: [(a, a)] -> [a]
linearise []			= []
linearise ((l, r) : remainder)	= l : r : linearise remainder	-- Recurse.

{- |
	* Merge two sorted lists, according to the specified order, to product a single sorted list.

	* The merge-process is /stable/, in that where items from each list are equal, they remain in the original order.

	* CAVEAT: duplicates are preserved.
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp	= slave	where
	slave l@(x : xs) r@(y : ys)
		| x `cmp` y == GT	= y : slave l ys
		| otherwise		= x : slave xs r
	slave [] r	= r
	slave l []	= l

-- | A specific instance of 'mergeBy'.
merge :: Ord a => [a] -> [a] -> [a]
merge	= mergeBy compare

{- |
	* Take until the specified predicate is satisfied; /including/ the item which satisfied it.

	* NB: @takeWhile (not . test)@ would return one fewer item.
-}
takeUntil
	:: (a -> Bool)	-- ^ Predicate, used to determine the last item taken.
	-> [a]		-- ^ The polymorphic input list.
	-> [a]
takeUntil predicate	= takeUntil'	where
	takeUntil' (x : xs)	= x {-take regardless-} : if predicate x then [] else takeUntil' xs
	takeUntil' _		= []

-- | Show a list, delimited by the specified tokens.
showListWith :: (Show token, Show element)
	=> (token, token, token)	-- ^ (Start-delimiter, separator, end-delimiter)
	-> [element]			-- ^ The polymorphic list to show.
	-> ShowS
showListWith (startDelimiter, separator, endDelimiter)	= foldr (.) (shows endDelimiter) . (shows startDelimiter :) . Data.List.intersperse (shows separator) . map shows

{-# NOINLINE showListWith #-}
{-# RULES "showListWith/Char" showListWith = showListWithChar #-}
{-# RULES "showListWith/String" showListWith = showListWithString #-}

-- | A specialisation of 'showListWith'.
showListWithChar :: Show element => (Char, Char, Char) -> [element] -> ShowS
showListWithChar (startDelimiter, separator, endDelimiter)	= foldr (.) (showChar endDelimiter) . (showChar startDelimiter :) . Data.List.intersperse (showChar separator) . map shows

-- | A specialisation of 'showListWith'.
showListWithString :: Show element => (String, String, String) -> [element] -> ShowS
showListWithString (startDelimiter, separator, endDelimiter)	= foldr (.) (showString endDelimiter) . (showString startDelimiter :) . Data.List.intersperse (showString separator) . map shows
