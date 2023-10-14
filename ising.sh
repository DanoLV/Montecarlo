#!/bin/bash

clear 
# 
rm n_iter.dat
rm ziggurat.o
rm ising.o
rm isingmodule.o
rm ising

stepsMC=1000000
touch "stepsMC.dat"

echo $stepsMC > stepsMC.dat

FILE="energy.dat"

make

./ising | tee $FILE 
# 
# awk '{print $1,$2}' $FILE | xmgrace -
# time ./ising > $FILE # | xmgrace -nxy $FILE

python plotEnergy.py

# cat $FILE
