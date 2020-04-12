########################################################################
#           SCRIPT PARA EXPERIMENTOS DEL TRABAJO AUTÓNOMO              #
#           MINERÍA DE FLUJOS DE DATOS. MOA                            #
#               Luis Balderas Ruiz                                     #
########################################################################

import os
import pandas as pd
import numpy as np
from scipy.stats import shapiro

def comparaAlgoritmos(muestra1,muestra2,nombre1,nombre2):
    print("Shapiro para " + nombre1)
    shap1 = shapiro(muestra1)
    print(shap1)
    print("Shapiro para " + nombre2)
    shap2 = shapiro(muestra2)
    print(shap2)
    from scipy import stats
    if shap1[1] > 0.05 and shap2[1] > 0.05:
        print(stats.ttest_ind(muestra1,muestra2))
        print("Promedio de acierto de " + nombre1 + ": " + str(np.mean(muestra1)))
        print("Promedio de acierto de " + nombre2 + ": " + str(np.mean(muestra2)))
    else:
        print(stats.mannwhitneyu(muestra1,muestra2))
        if shap1[1] > 0.05:
            print("Promedio de acierto de " + nombre1 + ": " + str(np.mean(muestra1)))
        else:
            print("Promedio de acierto de " + nombre1 + ": " + str(np.median(muestra1)))
        if shap2[1] > 0.05:
            print("Promedio de acierto de " + nombre2 + ": " + str(np.mean(muestra2)))
        else:
            print("Promedio de acierto de " + nombre2 + ": " + str(np.median(muestra2)))


def EjercicioOffline(adaptativo=False):
    print("Entrenamiento estacionario")
    print("Clasificador Hoeffding adaptativo: ", adaptativo)
    for i in range(4,34):
        if not adaptativo:
            query = 'java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask \
                    "EvaluateModel -m (LearnModel -l trees.HoeffdingTree -s \
                    (generators.WaveformGenerator -i '+str(i)+') -m 1000000) \
                    -s (generators.WaveformGenerator -i 2) -i 1000000" > offline/HNA/hna'+str(i)+'.csv'
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
    comparaAlgoritmos(resultado['Accuracy HNA'], resultado['Accuracy HA'], 'Accuracy HNA', 'Accuracy HA')
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

def EjercicioOnline():
    print("HoeffdingTree y HoeffdingTree adaptativo online")
    for i in range(1,31):
        os.system('java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask \
                   "EvaluateInterleavedTestThenTrain -l trees.HoeffdingAdaptiveTree \
                   -s (generators.WaveformGenerator -i ' + str(i)+') -i 1000000 -f 10000" > online/HA/ha' + str(i)+'.csv')
        os.system('java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask \
                   "EvaluateInterleavedTestThenTrain -l trees.HoeffdingTree \
                   -s (generators.WaveformGenerator -i ' + str(i)+') -i 1000000 -f 10000" > online/HNA/hna' + str(i)+'.csv')

#EjercicioOffline(False)
#EjercicioOffline(True)
#EvaluacionEjercicioOffline()
EjercicioOnline()
