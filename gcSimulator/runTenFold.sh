for Trace in `ls /data/remy/*short`
  do
    cp "$Trace" "/data/remy/temp.trace"
    for Hs in `ls batik.trace.short.hs`
      do
        cp "$Hs" Main.hs && cabal run > /dev/null && mv timing.temp "$Trace"."$Hs"timing.log 
#        echo "$Trace"."$Hs"
      done
  done
