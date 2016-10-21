{-# LANGUAGE BangPatterns #-}
module Simulate where
import EtParser
import Machine
import qualified Data.List as List


--We expose this so users can single step if they want
simStep::Machine m => m->Record->m
simStep !m (Alloc oid size ty thread)           = allocate oid size ty thread m
simStep !m (Death oid)		                = death oid m
simStep !m (Update old origin new thread field) = update old origin new thread field m
simStep !m (Entry mid refid thread)	        = methodEntry mid refid thread m
simStep !m (Exit mid refid thread)              = methodExit mid refid thread m
simStep !m (Root rid thread)		        = root rid thread m

--Simulate, starting with the initial machine
simulate::Machine m=>m->[Record]->m
simulate initM rs = List.foldl' simStep initM rs
