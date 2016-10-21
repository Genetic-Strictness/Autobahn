# cp nobangs.hs Main.hs && cabal run > nobangres.log &&
cp bangs.hs Main.hs && cabal run > bangres.log &&
cp gen.hs Main.hs && cabal run > genres.log
# cp gen2.hs Main.hs && cabal run > /dev/null && mv timing.temp gen2timing.log 
