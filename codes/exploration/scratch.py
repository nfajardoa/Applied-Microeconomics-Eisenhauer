
import os
import matplotlib.pyplot as plt
import numpy as np
import numpy as np
import seaborn as sns
import pandas as pd

os.chdir('/home/nico/Documents/MSc Bonn/Applied Micro')

jungendl = pd.read_csv('SOEP/jugendl.csv')
ids = jungendl['pid'].value_counts().sort_values()
jungendl['jl0018'].plot.hist()

pgen = pd.read_csv('SOEP/pgen.csv')
ids = pgen['pid'].value_counts().sort_values()
ids.plot.hist()

bioimmig =  pd.read_csv('SOEP/bioimmig.csv')


biol = pd.read_csv('SOEP/biol.csv')
ids = biol['pid'].value_counts().sort_values()
ids.plot.hist()

repeated = ids[ids==3].to_frame()


vars_set = pd.read_csv("Test_varset.csv")
vasr_set2 = pd.read_csv("Variables_set.csv")

dataset = ['biol']
varset = [[] for _ in range(1)]
for i, data in enumerate(dataset):
    sub = vars_set[vars_set['dataset'] == data]
    varset[i] = sub['variable'].tolist()

pl = pd.read_csv('SOEP/pl.csv', usecols = ['pid','cid', ])
ids = pl['pid'].value_counts().sort_values()
ids = ids[ids>=5].to_frame()
ids.index.reset = 'pid'
ids = ids.reset_index()
ids = ids['index'].tolist()

iter_csv = pd.read_csv('SOEP/pl.csv', iterator=True, chunksize=10000)
pl = pd.concat([chunk[chunk['pid'].isin(ids)] for chunk in iter_csv])