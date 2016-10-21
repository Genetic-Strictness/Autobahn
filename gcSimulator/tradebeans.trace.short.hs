{-# LANGUAGE BangPatterns #-}
module Main (main) where
import EtParser
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import qualified Data.Char as C
import qualified Graph as G
import qualified Data.Bits as Bits
import qualified Data.Foldable as F
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.List as List
import Data.Maybe
import qualified Data.Sequence as Seq
 
type Frame = (Int, S.IntSet)
 
type Stack = Seq.Seq Frame
 
type StackMap = M.IntMap Stack
 
push :: Int -> Int -> StackMap -> StackMap
push method (!thread) stacks
  = M.insert thread
      ((method, S.empty) Seq.<| (f (M.lookup thread stacks)))
      stacks
  where f (!((!(Just (!frames))))) = frames
        f Nothing = Seq.empty
 
pop :: Int -> Int -> StackMap -> StackMap
pop method thread (!stacks) = M.update f thread stacks
  where f (!frames)
          = if Seq.null frames then Nothing else
              Just $ Seq.drop 1 $
                Seq.dropWhileL (\ (!((!m), _)) -> m /= method) frames
 
addRoot :: Int -> Int -> StackMap -> StackMap
addRoot r thread (!stacks) = M.update f thread stacks
  where f (!frames)
          = if not $ Seq.null frames then
              let (!(mid, roots)) = frames `Seq.index` 0 in
                Just $ (mid, S.insert r roots) Seq.<| Seq.drop 1 frames
              else Just Seq.empty
 
data Machine = Machine{nursery :: !S.IntSet, nursery_size :: !Int,
                       nursery_limit :: !Int, heap_size :: !Int, heap_limit :: !Int,
                       heap :: !G.Graph, roots :: !S.IntSet, stacks :: !StackMap,
                       alloc :: !Int}
             deriving (Eq, Show)
mb = 1024 * 1024
 
initMachine :: Machine
initMachine
  = Machine S.empty 0 (4 * mb) 0 (100 * mb) G.empty S.empty M.empty 0
 
simStep :: (Machine, Int) -> Record -> (Machine, Int)
simStep (!(m, (!marks))) (!(Alloc oid (!size) ty (!tid)))
  = if doCollection then
      (collectedMachine{alloc = alloc'}, marks + newMarks) else
      (m{nursery = nursery', heap_size = heap_size', heap = heap',
         alloc = alloc'},
       marks)
  where (!nursery') = S.insert oid (nursery m)
        heap_size' = (heap_size m + size)
        heap' = G.insNode oid (heap m)
        (!doCollection) = heap_size' > heap_limit m
        ((!collectedMachine), (!newMarks)) = wholeHeapCollect m
        alloc' = (alloc m) + size
simStep (m, (!marks)) (!((!(Death oid))))
  = (m{nursery = nursery', heap = heap'}, marks)
  where (!nursery') = S.delete oid (nursery m)
        (!heap') = G.delNode oid (heap m)
simStep (m, (!marks)) (!(Update old origin new tid (!_)))
  = (m{heap = heap'}, marks)
  where heap' = G.insEdge origin new $! G.delEdge origin old (heap m)
simStep (!(m, (!marks))) (!(Entry (!mid) rid (!tid)))
  = (m{stacks = stacks'}, marks)
  where (!stacks') = push mid tid (stacks m)
simStep (m, marks) ((!(Exit (!mid) rid (!tid))))
  = (m{stacks = stacks'}, marks)
  where (!stacks') = pop mid tid (stacks m)
simStep (m, (!marks)) ((!(Root (!r) t)))
  = (m{stacks = addRoot r t (stacks m)}, marks)
 
simulate :: [Record] -> (Machine, Int)
simulate (!rs) = List.foldl' simStep (initMachine, 0) rs
 
wholeHeapCollect :: Machine -> (Machine, Int)
wholeHeapCollect m = (m{heap = heap'}, S.size markedSet)
  where heap' = sweep markedSet (heap m)
         
        mark :: S.IntSet -> S.IntSet -> G.Graph -> S.IntSet
        mark (!black) (!grey) heap
          = if grey == S.empty then black else mark black' grey' heap
          where  
                black' :: S.IntSet
                (!black') = S.fold (\ (!o) m -> S.insert o m) black grey
                 
                grey' :: S.IntSet
                grey'
                  = S.fold (S.union . neighbors) S.empty $
                      S.filter (not . isBlack) grey
                 
                neighbors :: Int -> S.IntSet
                neighbors (!n) = S.fromList $ (fromMaybe []) $ G.neighbors n heap
                 
                isBlack :: Int -> Bool
                isBlack x = S.member x black
        markedSet = mark S.empty (getRoots m) (heap m)
         
        sweep :: S.IntSet -> G.Graph -> G.Graph
        sweep (!marked) (!heap)
          = S.fold G.delNode heap (G.nodes heap S.\\ marked)
 
getRoots :: Machine -> S.IntSet
getRoots (!m)
  = M.fold (S.union) S.empty (M.map stackRoots (stacks m))
  where  
        stackRoots :: Stack -> S.IntSet
        stackRoots (!s)
          = F.foldl (S.union) (S.empty)
              (fmap (\ (!(method, roots)) -> roots) s)
main
  = do let args = ["/data/remy/temp.trace"]
       (!contents) <- L.readFile (args !! 0)
       print $! simulate $! map (f . readRecord) $! L.lines contents
  where  
        f :: Maybe (Record, L.ByteString) -> Record
        f (Just (!((!r), _))) = r
        f (!_) = error "We are all going to die."
 
data Flag = NurserySize String
          | FileName String
          deriving Eq
 
data Options = Options{optNurseryLimit :: Int,
                       optFileName :: String}
 
defaults :: Options
defaults
  = Options{optNurseryLimit = 1024 * 1024 * 4,
            optFileName = "elephantTracks.trace"}
 
flagsToOptions :: [Flag] -> Options -> Options
flagsToOptions [] (!o) = o
flagsToOptions ((!((!(NurserySize (!ns))) : fs))) (!o)
  = flagsToOptions fs o{optNurseryLimit = read ns}
flagsToOptions ((!((FileName name) : fs))) o
  = flagsToOptions fs o{optFileName = name}
 
options :: [OptDescr Flag]
options
  = [Option ['n'] ["nurserysize"] (ReqArg NurserySize "")
       "Specify the nursery size (in bytes)",
     Option ['f'] ["file"] (ReqArg FileName "")
       "Specify the trace file name"]