<<<<<<< HEAD
import numpy as np
import matplotlib.pyplot as plt
import argparse

argParser = argparse.ArgumentParser()
argParser.add_argument("-n", help="numero archivo de datos")
argParser.add_argument("-dim", help="ndimension de la matriz")

try: 
    args = argParser.parse_args()
    archivo = "datos"+ args.n + ".dat"
    dim = args.dim
except: 
    archivo = "datos.dat"
    dim=20


# if (args is not initial)
# archivo = "datos"+ args.n + ".dat"
# plt.figure()


#x = np.genfromtxt('stepsMC.dat')

# Initialise the subplot function using number of rows and columns 
figure, axis = plt.subplots(3,2) 

y = np.genfromtxt(archivo)
sigmaE =  np.sqrt(np.abs(y[:,3])) #y[:,3] - y[:,1]**2
sigmaM=  np.sqrt(np.abs( y[:,4])) #y[:,4] - y[:,2]**2
beta =  y[:,5]
cv = y[:,6] #(sigmaE[:] * beta**2)/ dim**2
XMag= y[:,7]

axis[0,0].plot(y[:,0],y[:,1])
# axis[0].set_title("Energia media") 
axis[0,0].set_ylabel('<Energia>')

axis[0,1].plot(y[1000:,0],sigmaE[1000:])
# axis[0].set_title("Energia media") 
axis[0,1].set_ylabel('<SigmaE>')

axis[1,0].plot(y[:,0],y[:,2])
# axis[1].set_title("Magnetizacion media") 
axis[1,0].set_ylabel('<Magnetizacion>')

axis[1,1].plot(y[1000:,0],sigmaM[1000:])
# axis[1].set_title("Magnetizacion media") 
axis[1,1].set_ylabel('<sigmaM>')

axis[2,0].plot(y[1000:,0],cv[1000:])
# axis[1].set_title("Magnetizacion media") 
axis[2,0].set_ylabel('CV')

axis[2,1].plot(y[1000:,0],XMag[1000:])
# axis[1].set_title("Magnetizacion media") 
axis[2,1].set_ylabel('Suceptibilidad magnetica')

plt.show()
# y = np.genfromtxt('datos.dat')


# plt.plot (y[:,0],y[:,1])
# plt.ylabel("Energy")
# plt.xlabel("Paso")


# plt.plot (y[:,0],y[:,2])
# plt.ylabel("Mag")
# plt.xlabel("Paso")
# plt.show()
=======
import numpy as np
import matplotlib.pyplot as plt
import argparse

argParser = argparse.ArgumentParser()
argParser.add_argument("-n", help="numero archivo de datos")
argParser.add_argument("-dim", help="ndimension de la matriz")

try: 
    args = argParser.parse_args()
    archivo = "datos"+ args.n + ".dat"
    dim = args.dim
except: 
    archivo = "datos.dat"
    dim=20


# if (args is not initial)
# archivo = "datos"+ args.n + ".dat"
# plt.figure()


#x = np.genfromtxt('stepsMC.dat')

# Initialise the subplot function using number of rows and columns 
figure, axis = plt.subplots(3,2) 

y = np.genfromtxt(archivo)
sigmaE =  np.sqrt(np.abs(y[:,3])) #y[:,3] - y[:,1]**2
sigmaM=  np.sqrt(np.abs( y[:,4])) #y[:,4] - y[:,2]**2
beta =  y[:,5]
cv = y[:,6] #(sigmaE[:] * beta**2)/ dim**2
XMag= y[:,7]

axis[0,0].plot(y[:,0],y[:,1])
# axis[0].set_title("Energia media") 
axis[0,0].set_ylabel('<Energia>')

axis[0,1].plot(y[1000:,0],sigmaE[1000:])
# axis[0].set_title("Energia media") 
axis[0,1].set_ylabel('<SigmaE>')

axis[1,0].plot(y[:,0],y[:,2])
# axis[1].set_title("Magnetizacion media") 
axis[1,0].set_ylabel('<Magnetizacion>')

axis[1,1].plot(y[1000:,0],sigmaM[1000:])
# axis[1].set_title("Magnetizacion media") 
axis[1,1].set_ylabel('<sigmaM>')

axis[2,0].plot(y[1000:,0],cv[1000:])
# axis[1].set_title("Magnetizacion media") 
axis[2,0].set_ylabel('CV')

axis[2,1].plot(y[1000:,0],XMag[1000:])
# axis[1].set_title("Magnetizacion media") 
axis[2,1].set_ylabel('Suceptibilidad magnetica')

plt.show()
# y = np.genfromtxt('datos.dat')


# plt.plot (y[:,0],y[:,1])
# plt.ylabel("Energy")
# plt.xlabel("Paso")


# plt.plot (y[:,0],y[:,2])
# plt.ylabel("Mag")
# plt.xlabel("Paso")
# plt.show()
>>>>>>> 1c434cdd02e62efc0723d409370c0a1fa82b19cf
