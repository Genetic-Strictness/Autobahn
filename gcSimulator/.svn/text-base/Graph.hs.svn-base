{-# LANGUAGE BangPatterns #-}
module Graph where
import Data.IntMap as M
import Data.IntSet as S
import Data.Set as Set
import Data.List as L
import Data.Sequence as Seq
import Data.Maybe
import qualified Data.Foldable as Fold
import Prelude hiding (foldr)

newtype Graph = Graph (IntMap IntList) deriving (Eq, Show)



{-- Mostly, objects have few neighbors. So we have special constructors to unpack those with five or fewer,
   Otherwise we resport to a list --}
data IntList =  Cons {-# UNPACK #-}!Int !IntList
	       -- |Quin {-# UNPACK #-} !Int !Int !Int !Int !Int
	       -- |Quad {-# UNPACK #-} !Int !Int !Int !Int 
	       -- |Trip {-# UNPACK #-} !Int !Int !Int 
	       |Doub {-# UNPACK #-} !Int !Int
	       |Sing {-# UNPACK #-} !Int
	       |Empty
	       deriving (Eq,Show)


ilFoldl'::(a->Int->a)->a->IntList->a
ilFoldl' f v  Empty      = v 
ilFoldl' f v (Cons i is) = let 
                             !x = f v i 
			   in
			     ilFoldl' f  x is
{-ilFoldl' f v (Quin b c d e g) = L.foldl' f v [b,c,d,e,g]
ilFoldl' f v (Quad b c d e)   = L.foldl' f v [b,c,d,e]
ilFoldl' f v (Trip b c d)     = L.foldl' f v [b,c,d]-}
ilFoldl' f v (Doub b c)       =   (f v b) `seq` f (f v b) c   
ilFoldl' f v (Sing a)         = f v a


del::Int->IntList->IntList
del !i (Cons j js) = if i == j then
			js
		    else
			Cons j (del i js)
del !i (Doub a b)
	| i == a    = Sing b
	| i == b    = Sing a
	| otherwise = Doub a b

del !i (Sing a) 
	| i == a    = Empty
	| otherwise = Sing a
del !i Empty 	  = Empty

ins::Int->IntList->IntList
ins i is@(Cons _ _) = Cons i is
ins i is@(Doub _ _) = Cons i is
ins i (Sing j)      = Doub i j
ins i Empty         = Sing i
    
 
{-del !i q@(Quin a b c d e) 
	| i == a  = Quad   b c d e
	| i == b  = Quad a   c d e
        | i == c  = Quad a b   d e
        | i == d  = Quad a b c   e
	| i == e  = Quad a b c d
	| otherwise = q-}


toList::IntList->[Int]
--toList (Cons i is) = i : Graph.toList is
--toList Empty = []
toList is  = ilFoldl' (flip (:)) [] is --This flips the order, but there is no cannoical edge ordering wnay way

insNode::Int->Graph->Graph
insNode n  (Graph !m) =  Graph $! M.insert n Empty m


insEdge::Int->Int->Graph->Graph
insEdge s d (Graph !m)  =  Graph $! M.insert s d_elist m
			where
			  !d_elist  =  d `ins` elist
			  (Just !elist) = M.lookup s m


delEdge::Int->Int->Graph->Graph
delEdge s d (Graph !m) =  Graph	$! M.insert s del_list  m
			 where
				!del_list = del d elist
				!elist = unpack $! M.lookup s m
			        unpack Nothing  = Empty
				unpack (Just l) = l
				
						
neighbors::Int->Graph->Maybe [Int]
neighbors n (Graph !m) 	= f (M.lookup n m)
	where
		f (Just l) = Just $ Graph.toList l
		f Nothing = Nothing


delNode::Int->Graph->Graph
delNode n (Graph !m) = Graph $! M.delete n m


hasNode::Int->Graph->Bool
hasNode n (Graph !m)  = M.member n m


nodes::Graph->S.IntSet
nodes (Graph !m) = keysSet m

{--weaklyConnComp::Graph->[S.IntSet]
weaklyConnComp g = stronglyConnComp (undirect g)

stronglyconnComp::Graph->[S.IntSet]--}

transpose::Graph->Graph
transpose g@(Graph m) = M.foldWithKey transposeOneNode nodeGraph m 
	    		where   
			  transposeOneNode::Int->IntList->Graph->Graph
			  transposeOneNode orig dests graph = L.foldl' (\g d -> insEdge d orig g)   graph (Graph.toList dests)
                          --A graph with the same nodes as g, but no edges
			  nodeGraph = S.fold  insNode Graph.empty  (M.keysSet m)
			 


empty::Graph
empty = Graph M.empty 



dfs::Graph->(IntSet,Seq.Seq Int)
dfs g@(Graph m) = S.fold (dfsVisit g) (S.empty, Seq.empty) (M.keysSet m)

dfsVisit::Graph->Int->(IntSet, Seq.Seq Int)->(IntSet, Seq.Seq Int)
dfsVisit g n (grey,black) = if n `S.member` grey then
			      (grey,black)
			  else
			      (grey',  black' |> n  )
			  where
			      (grey',black') = L.foldl' (\x  y -> dfsVisit g y x)  ( n `S.insert` grey,black )   (fromJust $ neighbors n g) 
	  
	  

dfsWithOrder::Graph->Seq.Seq Int->(IntSet, Seq.Seq Int, Set (Seq.Seq Int))
dfsWithOrder g@(Graph m) order =  Fold.foldr f  (S.empty, Seq.empty, Set.empty) order
				  where
				    f::Int->(IntSet, Seq.Seq Int, Set (Seq.Seq Int))->(IntSet, Seq.Seq Int, Set (Seq.Seq Int))
				    f n  (grey, black, comps) = (grey',Seq.empty,comps')
				   	where 
					    (grey',black') = dfsVisit g n (grey,black)
					    comps' =  black' `Set.insert`comps
				   

strongCC::Graph -> Set (Seq.Seq Int)
strongCC g = ccs
	   where
		gt         = Graph.transpose g
	        (_,order)  = dfs g
		(_,_,ccs)  = dfsWithOrder gt order 
