#!/bin/bash

deltaT=0.1
i=1
n=20
pasos=50
T=0
pasomc=10000000

T=$(echo "scale=4; $i*$deltaT" | bc)
./Ising -n $n -s $pasomc -T $T -o m.dat > "datos$i.dat"

for ((i=2;i<=$pasos;i++))
do
T=$(echo "scale=4; $i*$deltaT" | bc)
./Ising -n $n -s $pasomc -T $T -o m.dat -i m.dat > "datos$i.dat"
done