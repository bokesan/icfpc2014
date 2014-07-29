#!/bin/bash

src=evil.gasm

ghasm=../compiler/dist/build/ghasm/ghasm


flightDist[0]=11
flightDist[1]=13
flightDist[2]=15
flightDist[3]=14

for n in 0 1 2 3 ; do
    cpp --traditional-cpp -P -DGHOST=$n -DP_FRIGHT=${flightDist[$n]} $src ghost$n.gasm
    $ghasm ghost$n.gasm > ghost$n.ghc
    rm -f ghost$n.gasm
done
