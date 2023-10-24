#!/bin/bash

# Complilo el codigo fortran
make
rm datos*.*
rm datosEstadisticaT.dat
clear

# Variacion de temperatura por cada corrida
deltaT=0.05

# Dimension de la matriz
n=20

# Cantidad de temperaturas a evaluar (archivos creados)
pasos=100

# Pasos de montecarlo por temperatura
pasomc=10000000

# Pasos para termalizar en la primera corrida
pasosterm=1000000

# Primer corrida a temperatura inicial
# La matriz inicial es al azar
# Guarda la ultima matriz usada
i=1
T=0
T=$(echo "scale=4; $i*$deltaT" | bc)
./Ising -n $n -s $pasomc -sns $pasosterm -T $T -o m.dat > "datos$i.dat"

# Corridas restantes
# Cargan la matriz anterior y guardan la ultima
for ((i=2;i<=$pasos;i++))
do
T=$(echo "scale=4; $i*$deltaT" | bc)
./Ising -n $n -s $pasomc -T $T -o m.dat -i m.dat > "datos$i.dat"
done

# Proceso los datos
python3 estadistica.py -o datosEstadisticaT.dat -cantarch $pasos

# muestro los graficos
python3 plotDatosT.py -i datosEstadisticaT.dat 