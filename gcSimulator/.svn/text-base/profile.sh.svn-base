prog=$1
type=$2
arg=$3

#ghc -XBangPatterns -prof -caf-all -auto-all -fforce-recomp --make -O2   -funbox-strict-fields  $1
ghc -XBangPatterns -prof -caf-all -auto-all -fforce-recomp --make -O3 -rtsopts -funbox-strict-fields $1
#-M2048M -K83886080
time ./$prog  $arg +RTS -K80M  -M30G $type  -p  -xc  
hp2ps -e8in -c $1.hp
ghostscript $1.ps
