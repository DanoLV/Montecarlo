import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys

# Establecer parametros de linea de comando
argParser = argparse.ArgumentParser()
argParser.add_argument("-i", help="archivo de datos")

# Leer parametros de linea de comando
try: 
    args = argParser.parse_args()
except: 
    sys.exit("No se pudo leer parametros de linea de comandos")

# Verificar que se paso un nombre de archivo y salir si no fue asi
archivo = args.i
if( archivo is None):
    sys.exit('Especifique nombre de archivo')

# Establecer subplots
figure, axis = plt.subplots(2,2) 

# Leer datos del archivo
y = np.genfromtxt(archivo)
sigmaE =  y[:,3] 
sigmaM=   y[:,4] 
cv = y[:,5]
XMag= y[:,6]
xmin= 0.1

# Construir los subplot de los datos
a=0
b=0
axis[a,b].plot(y[:,0],y[:,1])
axis[a,b].set_ylabel('$<E>_{sitio}$')
axis[a,b].set_xlabel('T')
axis[a,b].grid(True, which='both')
axis[a,b].set_xlim([xmin,5])

# axis[0,1].plot(y[:,0],sigmaE[:])
# axis[0,1].set_ylabel('$<\mathrm{\sigma}^2_E>$')
# axis[0,1].set_xlabel('T')
# axis[0,1].grid(True, which='both')
# axis[0,1].set_xlim([xmin,5])

a=0
b=1
axis[a,b].plot(y[:,0],y[:,2])
axis[a,b].set_ylabel('$<M>_{sitio}$')
axis[a,b].set_xlabel('T')
axis[a,b].grid(True, which='both')
axis[a,b].set_xlim([xmin,5])

# axis[1,1].plot(y[:,0],sigmaM[:])
# axis[1,1].set_ylabel(''$<\mathrm{\sigma}^2_M>$'')
# axis[1,1].set_xlabel('T')
# axis[1,1].grid(True, which='both')
# axis[1,1].set_xlim([xmin,5])

a=1
b=0
axis[a,b].plot(y[:,0],cv[:])
axis[a,b].set_ylabel('$C_V$')
axis[a,b].set_xlabel('T')
axis[a,b].grid(True, which='both')
# axis[a,b].autoscale() 
# axis[a,b].set_ylim([0,0.000001])
axis[a,b].set_xlim([xmin,5])

a=1
b=1
axis[a,b].plot(y[:,0],XMag[:])
axis[a,b].set_ylabel('$\mathrm{\chi}$')
axis[a,b].set_xlabel('T')
axis[a,b].grid(True, which='both')
# axis[a,b].autoscale() 
# axis[a,b].set_ylim([0,250])
axis[a,b].set_xlim([xmin,5])

# Mostrar el grafico
plt.subplots_adjust(hspace=0.4)
mng = plt.get_current_fig_manager()
mng.resize(*mng.window.maxsize())
plt.show()