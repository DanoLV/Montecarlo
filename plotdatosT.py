import numpy as np
import matplotlib.pyplot as plt
import argparse

argParser = argparse.ArgumentParser()
argParser.add_argument("-i", help="archivo de datos")

archivo = "out.dat"

# try: 
#     args = argParser.parse_args()
#     archivo = args.i
# except: 
#     archivo = "out.dat"

figure, axis = plt.subplots(3,2) 

y = np.genfromtxt(archivo)
sigmaE =  y[:,3] 
sigmaM=   y[:,4] 
cv = y[:,5]
<<<<<<< HEAD
XMag= y[:,7]
xmin= 0.
=======
XMag= y[:,6]
xmin= 0.5
>>>>>>> 1c434cdd02e62efc0723d409370c0a1fa82b19cf

axis[0,0].plot(y[:,0],y[:,1],"-o")
axis[0,0].set_xlim([xmin,5])
# axis[0].set_title("Energia media") 
axis[0,0].set_ylabel('<Energia>')
axis[0,0].grid(True, which='both')

axis[0,1].plot(y[:,0],sigmaE[:])
# axis[0].set_title("Energia media") 
axis[0,1].set_ylabel('<SigmaE>')
axis[0,1].grid(True, which='both')
axis[0,1].set_xlim([xmin,5])

axis[1,0].plot(y[:,0],y[:,2],"-o")
# axis[1].set_title("Magnetizacion media") 
axis[1,0].set_ylabel('<Magnetizacion>')
axis[1,0].grid(True, which='both')
axis[1,0].set_xlim([xmin,5])

axis[1,1].plot(y[:,0],sigmaM[:])
# axis[1].set_title("Magnetizacion media") 
axis[1,1].set_ylabel('<sigmaM>')
axis[1,1].grid(True, which='both')
axis[1,1].set_xlim([xmin,5])

axis[2,0].plot(y[:,0],cv[:])
# axis[1].set_title("Magnetizacion media") 
axis[2,0].set_ylabel('CV')
axis[2,0].grid(True, which='both')
axis[2,0].set_xlim([xmin,5])
<<<<<<< HEAD
# axis[2,0].set_ylim([0,3.5])
=======
axis[2,0].set_ylim([0,3.5])
>>>>>>> 1c434cdd02e62efc0723d409370c0a1fa82b19cf

axis[2,1].plot(y[:,0],XMag[:])
# axis[1].set_title("Magnetizacion media") 
axis[2,1].set_ylabel('Suceptibilidad magnetica')
axis[2,1].grid(True, which='both')
axis[2,1].set_xlim([xmin,5])
<<<<<<< HEAD
# axis[2,1].set_ylim([0,])
=======
axis[2,1].set_ylim([0,0.0002])
>>>>>>> 1c434cdd02e62efc0723d409370c0a1fa82b19cf

plt.show()