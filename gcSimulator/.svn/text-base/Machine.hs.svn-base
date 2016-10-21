module Machine where
import qualified Data.IntSet as S
import qualified Data.IntMultiSet as MS
import qualified Data.IntMap as M
import qualified Graph as G
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Sequence (index,(<|))
import Debug.Trace (trace)
import ObjectModel


type ObjectID = Int
type Size     = Int
type ThreadID = Int
type MethodID = Int
type FieldID  = Int

class Machine m where
  allocate::ObjectID->Size->L.ByteString->ThreadID->m->m
  update::ObjectID->ObjectID->ObjectID->Maybe FieldID->ThreadID->m->m
  root::ObjectID->ThreadID->m->m
  methodEntry::MethodID->ObjectID->ThreadID->m->m
  methodExit::MethodID->ObjectID->ThreadID->m->m
  death::ObjectID->m->m
  
gb::Int
gb = 1024*1024*1024

mb::Int
mb = 1024*1024


