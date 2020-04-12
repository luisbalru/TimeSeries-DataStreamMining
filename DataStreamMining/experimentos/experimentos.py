########################################################################
#           SCRIPT PARA EXPERIMENTOS DEL TRABAJO AUTÓNOMO              #
#           MINERÍA DE FLUJOS DE DATOS. MOA                            #
#               Luis Balderas Ruiz                                     #
########################################################################

import os
import pandas as pd
import numpy as np
from scipy.stats import shapiro

def EjercicioOffline(adaptativo=False):
    print("Entrenamiento estacionario")
    print("Clasificador Hoeffding adaptativo: ", adaptativo)
    for i in range(4,34):
        if not adaptativo:
            query = 'java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask "EvaluateModel -m (LearnModel -l trees.HoeffdingTree -s (generators.WaveformGenerator -i '+str(i)+') -m 1000000) -s (generators.WaveformGenerator -i 2) -i 1000000" > offline/HNA/hna'+str(i)+'.csv'
            os.system(query)
        else:
            os.system('java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask \
                  "EvaluateModel -m (LearnModel -l trees.HoeffdingAdaptiveTree \
                  -s (generators.WaveformGenerator -i '+str(i)+') -m 1000000) \
                  -s (generators.WaveformGenerator -i 2) -i 1000000" > offline/HA/ha'+str(i)+'.csv')

def EvaluacionEjercicioOffline(dir1='offline/HNA', dir2 = 'offline/HA'):
    files1 = os.listdir(dir1)
    files2 = os.listdir(dir2)
    ha_acc = []
    ha_kap = []
    hna_acc = []
    hna_kap  = []
    for f in files1:
        data = pd.read_csv(dir1+'/'+f)
        hna_acc.append(data.iloc[-1,2:3][0])
        hna_kap.append(data.iloc[-1,3:4][0])
    for f in files2:
        data = pd.read_csv(dir2+'/'+f)
        ha_acc.append(data.iloc[-1,2:3][0])
        ha_kap.append(data.iloc[-1,3:4][0])
    resultado = pd.DataFrame({"i":np.arange(4,34).tolist(),'Accuracy HNA':hna_acc,'Kappa HNA':hna_kap, 'Accuracy HA':ha_acc, 'Kappa HA': ha_kap})
    resultado.to_csv('ejercicio_offline.csv',index=False)
    return(resultado)


def CreaPoblacion(dir1='NB',dir2='HT'):
    files1 = os.listdir(dir1)
    files2 = os.listdir(dir2)
    nbs = []
    hts = []
    for f in files1:
        data = pd.read_csv(dir1+'/'+f)
        nbs.append(data.iloc[-1,4])
    for f in files2:
        data = pd.read_csv(dir2+'/'+f)
        hts.append(data.iloc[-1,4])
    resultado = pd.DataFrame({'NB':nbs,'HT':hts})
    return(resultado)

#EjercicioOffline(False)
#EjercicioOffline(True)
EvaluacionEjercicioOffline()
