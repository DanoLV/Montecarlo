import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

# Establecer parametros de linea de comando
argParser = argparse.ArgumentParser()
argParser.add_argument("-cantarch", help="cantidad de archivos de datos")
argParser.add_argument("-o", help="Archivo de salida")

# Leer parametros de linea de comando
try: 
    args = argParser.parse_args()
except:
    sys.exit("No se pudo leer parametros de linea de comandos")

cant = int(args.cantarch)
if(cant <=0 ):
    sys.exit("La cantidad de archivos no es valida")

fout = args.o
if( fout is None):
    sys.exit('Especifique nombre de archivo de salida')

# Abrir archivo de salida
fo = open(fout, "w")
separador = ' '

for i in range(1,cant+1,1):
    # Leer archivo de datos
    archivo = "datos"+ str(i) + ".dat"
    y = np.genfromtxt(archivo)

    # Procesar datos
    T = y[1,3]
    n = y[1,4]
    npart= n**2
    faceptado = str(np.mean(y[:,5]))
    npart= n**2
    E_media = str(np.mean(y[:,1])/npart)
    M_media = str(np.mean(y[:,2])/npart)

    VarE_media = np.var(y[:,1]/npart)
    VarM_media = np.var(y[:,2]/npart)

    CV_media = str(VarE_media/(T*n)**2)
    XM_media = str(npart*VarM_media/T)

    T = str(y[1,3])
    n= str(y[1,4])
    VarE_media = str(VarE_media)
    VarM_media = str(VarM_media)

    # Escribir datos en archivo de salida
    fo.write(T+separador+E_media+separador+M_media+separador+VarE_media+         
             separador+VarM_media+separador+CV_media+separador+XM_media+separador+faceptado+"\n")
    
# Cerrar archivo de salida
fo.close()