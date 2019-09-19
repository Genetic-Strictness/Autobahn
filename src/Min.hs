{-# LANGUAGE BangPatterns #-}

module Min where
import Data.List
import Utils

-- turn off all bangs except hot spots in bangs list
modBangs :: [(FilePath, [(Int, Int)])]
         -> [(FilePath, [(Bool, Int)])]
         -> [(FilePath, [(Bool, Int)])]
modBangs hs bs =
    let files  = fst $ unzip hs
        erased = map (\(f, bangs) ->
                      if elem f files then (f, bangs) else (f, eraseBangs bangs)) bs
    in map (\(f1, bangs) ->
         (foldl (\acc (f2, locs) ->
           if f1 == f2 then (f1, (modify locs bangs)) else acc) (f1, bangs) hs)) erased

-- given a list of hot spot locs and bangs,
-- turn off each bang that doesn't fall within a hot spot location
modify :: [(Int, Int)] -> [(Bool, Int)] -> [(Bool, Int)]
modify locs bs = map (\(b, l) ->
                   if (foldl (\acc (s, e) -> if l <= e && l >= s then True else acc) False locs)
                   then (b, l)
                   else (False, l)) bs

-- given a list of hot spots (file name and source location),
-- agrregate source locations of multiple hotspots in the same file together
compileFiles :: [([Char], (Int, Int))] -> [(FilePath, [(Int, Int)])]
compileFiles hs =
    let repeatedList = map (\(f1, locs) ->
                 foldl (\(f, acc) (f2, locs2) ->
                    if f1 == f2 then (f1, locs2:acc) else (f1, acc)) (f1, [locs]) hs) hs
    in foldl (\acc (f, locs) ->
         if elem f (fst $ unzip acc) then acc else (f, locs):acc) [] repeatedList

-- erase all bangs
eraseBangs :: [(Bool, Int)] -> [(Bool, Int)]
eraseBangs bs = map (\(b, l) -> (False, l)) bs

