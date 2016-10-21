#! /bin/bash

# cp /data/remy/luindex.traceac /data/remy/temp.trace &&
cp nobangs.hs Main.hs && cabal run > /dev/null && mv timing.temp nobangstiming.log &&
cp bangs.hs Main.hs && cabal run > /dev/null && mv timing.temp bangstiming.log &&
cp gen.hs Main.hs && cabal run > /dev/null && mv timing.temp gentiming.log &&
cp gen2.hs Main.hs && cabal run > /dev/null && mv timing.temp gen2timing.log &&
cp Main.hs.opt Main.hs && cabal run > /dev/null && mv timing.temp opttiming.log &&
mkdir battry && mv *timing* battry

# cp /data/remy/luindex.tracead /data/remy/temp.trace &&
# cp nobangs.hs Main.hs && cabal run > /dev/null && mv timing.temp nobangstiming.log &&
# cp bangs.hs Main.hs && cabal run > /dev/null && mv timing.temp bangstiming.log &&
# cp gen.hs Main.hs && cabal run > /dev/null && mv timing.temp gentiming.log &&
# # cp gen2.hs Main.hs && cabal run > /dev/null && mv timing.temp gen2timing.log 
# mkdir lud && mv *timing* lud &&
# 
# cp /data/remy/luindex.traceae /data/remy/temp.trace &&
# cp nobangs.hs Main.hs && cabal run > /dev/null && mv timing.temp nobangstiming.log &&
# cp bangs.hs Main.hs && cabal run > /dev/null && mv timing.temp bangstiming.log &&
# cp gen.hs Main.hs && cabal run > /dev/null && mv timing.temp gentiming.log &&
# # cp gen2.hs Main.hs && cabal run > /dev/null && mv timing.temp gen2timing.log 
# mkdir lue && mv *timing* lue &&
# 
# cp /data/remy/luindex.traceaf /data/remy/temp.trace &&
# cp nobangs.hs Main.hs && cabal run > /dev/null && mv timing.temp nobangstiming.log &&
# cp bangs.hs Main.hs && cabal run > /dev/null && mv timing.temp bangstiming.log &&
# cp gen.hs Main.hs && cabal run > /dev/null && mv timing.temp gentiming.log &&
# # cp gen2.hs Main.hs && cabal run > /dev/null && mv timing.temp gen2timing.log 
# mkdir luf && mv *timing* luf &&
# 
# cp /data/remy/luindex.traceag /data/remy/temp.trace &&
# cp nobangs.hs Main.hs && cabal run > /dev/null && mv timing.temp nobangstiming.log &&
# cp bangs.hs Main.hs && cabal run > /dev/null && mv timing.temp bangstiming.log &&
# cp gen.hs Main.hs && cabal run > /dev/null && mv timing.temp gentiming.log &&
# # cp gen2.hs Main.hs && cabal run > /dev/null && mv timing.temp gen2timing.log 
# mkdir lug && mv *timing* lug &&
# 
# cp /data/remy/luindex.traceah /data/remy/temp.trace &&
# cp nobangs.hs Main.hs && cabal run > /dev/null && mv timing.temp nobangstiming.log &&
# cp bangs.hs Main.hs && cabal run > /dev/null && mv timing.temp bangstiming.log &&
# cp gen.hs Main.hs && cabal run > /dev/null && mv timing.temp gentiming.log &&
# # cp gen2.hs Main.hs && cabal run > /dev/null && mv timing.temp gen2timing.log 
# mkdir luh && mv *timing* luh &&
# 
# cp /data/remy/luindex.traceai /data/remy/temp.trace &&
# cp nobangs.hs Main.hs && cabal run > /dev/null && mv timing.temp nobangstiming.log &&
# cp bangs.hs Main.hs && cabal run > /dev/null && mv timing.temp bangstiming.log &&
# cp gen.hs Main.hs && cabal run > /dev/null && mv timing.temp gentiming.log &&
# # cp gen2.hs Main.hs && cabal run > /dev/null && mv timing.temp gen2timing.log 
# mkdir lui && mv *timing* lui &&
# 
# cp /data/remy/luindex.traceaj /data/remy/temp.trace &&
# cp nobangs.hs Main.hs && cabal run > /dev/null && mv timing.temp nobangstiming.log &&
# cp bangs.hs Main.hs && cabal run > /dev/null && mv timing.temp bangstiming.log &&
# cp gen.hs Main.hs && cabal run > /dev/null && mv timing.temp gentiming.log &&
# # cp gen2.hs Main.hs && cabal run > /dev/null && mv timing.temp gen2timing.log 
# mkdir luj && mv *timing* luj 
# 
# # cp /data/remy/tradebeans.trace /data/remy/temp.trace &&
# # cp nobangs.hs Main.hs && cabal run && mv timing.temp nobangstiming.log &&
# # cp bangs.hs Main.hs && cabal run && mv timing.temp bangstiming.log &&
# # cp gen.hs Main.hs && cabal run && mv timing.temp gentiming.log &&
# # mkdir tradebeans && mv *timing* tradebeans &&
# # 
# # cp /data/remy/tradesoap.trace /data/remy/temp.trace &&
# # cp nobangs.hs Main.hs && cabal run && mv timing.temp nobangstiming.log &&
# # cp bangs.hs Main.hs && cabal run && mv timing.temp bangstiming.log &&
# # cp gen.hs Main.hs && cabal run && mv timing.temp gentiming.log &&
# # mkdir tradesoap && mv *timing* tradesoap 
