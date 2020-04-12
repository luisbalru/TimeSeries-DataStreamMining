########################################################################
#           SCRIPT PARA EXPERIMENTOS DEL TRABAJO GUIADO                #
#           MINERÍA DE FLUJOS DE DATOS. MOA                            #
#               Luis Balderas Ruiz                                     #
########################################################################

import os
import pandas as pd
import numpy as np
from scipy.stats import shapiro

def EjercicioClasificacion():
    print("Naive Bayes y Hoeffing Tree")
    for i in range(1,31):
        os.system('java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask \
                   "EvaluateInterleavedTestThenTrain -l bayes.NaiveBayes \
                   -s (generators.RandomTreeGenerator -i ' + str(i)+') -i 1000000 -f 10000" > NB/nb' + str(i)+'.csv')
        os.system('java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask \
                   "EvaluateInterleavedTestThenTrain -l trees.HoeffdingTree \
                   -s (generators.RandomTreeGenerator -i ' + str(i)+') -i 1000000 -f 10000" > HT/ht' + str(i)+'.csv')


def EjercicioConceptDrift():
    print("Modelo dinámico vs Modelo estático")
    for i in range(1,31):
        '''
        os.system('java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask \
                  "EvaluatePrequential -l bayes.NaiveBayes -s \
                  (ConceptDriftStream -s (generators.SEAGenerator -f 2 -i ' + str(i)+ ') -d \
                  (generators.SEAGenerator -f 3 -i ' + str(i)+ ') -p 20000 -w 100) -i 100000 -f 1000" > MDCD/m' + str(i) + '.csv')
        '''
        os.system('java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask \
                  "EvaluateModel -m (LearnModel -l bayes.NaiveBayes -s (generators.SEAGenerator -f 2 -i ' + str(i)+ ') -m 100000) \
                  -s (ConceptDriftStream -s (generators.SEAGenerator -f 2 -i ' + str(i)+ ') -d (generators.SEAGenerator -f 3 -i ' + str(i)+ ') \
                  -p 20000 -w 100) -i 100000" > MECD/m' + str(i) + '.csv')

def EjercicioDDM():
    print("Modelo DDM")
    for i in range(1,31):
        query = 'java -cp moa.jar -javaagent:sizeofag-1.0.4.jar moa.DoTask "EvaluateInterleavedTestThenTrain -l \
                 (moa.classifiers.drift.SingleClassifierDrift -l bayes.NaiveBayes -d DDM) -s (ConceptDriftStream -s \
                 (generators.SEAGenerator -i ' + str(i)+ ' -f 2) -d (generators.SEAGenerator -i ' + str(i)+ ' -f 3)\
                 -p 20000 -w 100) -i 100000" > DDM/m' + str(i) + '.csv'
        print(query)
        os.system(query)

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
#EjercicioClasificacion()
#EjercicioConceptDrift()
#EjercicioDDM()

poblacion = CreaPoblacion('MECD','DDM')
print(shapiro(poblacion.iloc[:,0]))
print(shapiro(poblacion.iloc[:,1]))


from scipy import stats
print(stats.ttest_ind(poblacion.iloc[:,0],poblacion.iloc[:,1]))
print("Promedio de acierto de MECD: " + str(np.mean(poblacion.iloc[:,0])))
print("Promedio de acierto de DDM: " + str(np.mean(poblacion.iloc[:,1])))
