#!/bin/bash

deltabeta=0.1
i=1
n=20
pasobeta=50
beta=0
pasomc=10000000

for ((i=1;i<=$pasobeta;i++))
do
beta=$(echo "scale=4; $i*$deltabeta" | bc)
./Ising -n $n -s $pasomc -b $beta > "datos$i.dat"
done