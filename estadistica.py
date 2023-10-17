import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

argParser = argparse.ArgumentParser()
argParser.add_argument("-cantarch", help="cantidad de archivos de datos")
argParser.add_argument("-o", help="Archivo de salida")

cant = 50
fout = "out.dat"

# try: 
#     args = argParser.parse_args()
#     cant = int(args.cantarch)    
#     fout = args.o
# except:
#     sys.exit("ingrese cantidad de archivos")

fo = open(fout, "w")
separador = ' '
for i in range(1,cant+1,1):
    archivo = "datos"+ str(i) + ".dat"
    y = np.genfromtxt(archivo)
    T = y[1,3]
    n = y[1,4]
    faceptado = str(np.mean(y[:,5]))
    E_media = str(np.mean(y[:,1]))
    M_media = str(np.mean(y[:,2]))

    VarE_media = np.var(y[:,1])
    VarM_media = np.var(y[:,2])

    CV_media = str(VarE_media/(T*n)**2)
    XM_media = str(VarM_media/T)

    T = str(y[1,3])
    n= str(y[1,4])
    VarE_media = str(VarE_media)
    VarM_media = str(VarM_media)

    fo.write(T+separador+E_media+separador+M_media+separador+VarE_media+
             separador+VarM_media+separador+CV_media+separador+XM_media+separador+faceptado+"\n")

fo.close()