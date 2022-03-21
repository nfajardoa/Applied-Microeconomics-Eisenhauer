#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
plt.style.use("ggplot")
get_ipython().run_line_magic('matplotlib', 'inline')


# ### Import dataset

# In[2]:


# Variables
vars_set = pd.read_csv("data/Test_varset.csv")


# In[3]:


dataset = ['biobirth','bioparen', 'bioimmig','biojob', 'biosoc' ,'biopupil','biol']
varset = [[] for _ in range(7)]
for i, data in enumerate(dataset):
    sub = vars_set[vars_set['dataset'] == data]
    varset[i] = sub['variable'].tolist()


# In[4]:


biobirth = pd.read_csv("data/biobirth.csv", usecols=varset[0])
bioparen = pd.read_csv("data/bioparen.csv", usecols=varset[1])
bioimmig = pd.read_csv("data/bioimmig.csv", usecols=varset[2]) #The variables contained in BIOIMMIG relate to foreigners in (and migrants to) Germany.
biojob = pd.read_csv("data/biojob.csv", usecols=varset[3])


# In[5]:


# khong dung
#biosoc = pd.read_csv("data/biosoc.csv", usecols=varset[4]) # data on youth and socialization only from year 2000 
#biopupil = pd.read_csv("data/biopupil.csv", usecols=varset[5])  #Pre-Teen Questionnaire only from 2014 


# In[5]:


biol = pd.read_csv("data/biol.csv", usecols=varset[6])


# ### Keep only the variables needed

# In[6]:


print("biobirth:" + str(biobirth.shape))
print("bioparen:" + str(bioparen.shape)) 
print("bioimmig:" + str(bioimmig.shape)) 
print("biojob:" + str(biojob.shape)) 
print("biol:" + str(biol.shape)) 
#print("biopupil:" + str(biopupil.shape))
#print("biosoc:" + str(biosoc.shape)) 


# ### Merge dataset

# In[7]:


biol_immig = pd.merge(biol,bioimmig,on=['cid','pid','syear'],how='inner')
print("biol_immig:" + str(biol_immig.shape))


# In[8]:


biol_immig['bioage'] = biol_immig['syear'] - biol_immig['lb0011_h']


# In[9]:


biol_immig = biol_immig[biol_immig['bioage'].between(20,35)]


# In[10]:


biol_immig.rename(columns={"syear": "bioyear"},inplace=True)


# In[12]:


biol_immig['biresper'].value_counts()


# In[35]:


biol_immig_paren = pd.merge(biol_immig,bioparen,on=['cid','pid','bioyear'],how='inner')


# In[48]:


biol_immig_paren['immg_gr']=biol_immig_paren['biimgrp'].astype(str)


# In[49]:


biol_immig_paren['immg_gr'].replace(
    {"-6": "Version of questionnaire with modified filtering", 
     "-5": "Not included in this version of the questionnaire",
     "-4": "Inadmissible multiple response",
     "-3": "Answer improbable",
     "-2": "Does not apply",
     "-1": "No Answer",
     "2": "Person Of German Descent From Eastern Europe",
     "3": "German Who Lived Abroad",
     "4": "Citizen Of EU Country (up to 2009 EC)",
     "5": "Asylum seeker, refugee",
     "6": "Other Foreigner"
    },
    inplace=True
)


# In[51]:


#biol_immig_paren['biimgrp'].astype('category')
biol_immig_paren['immg_gr'].value_counts()


# In[55]:


biol_immig_paren['fnat'].value_counts()


# In[56]:


biol_immig_paren['forigin'].value_counts()


# In[57]:


test = biol_immig_paren[(biol_immig_paren['morigin'].isin([1,-2,-5]))&
                (biol_immig_paren['fnat']==1)
                ]


# In[59]:


test = biol_immig_paren[(biol_immig_paren['morigin']==2)&
                (biol_immig_paren['fnat']==1)
                ]
test


# In[7]:


dfMerge2 = pd.merge(biobirth,bioparen,on=['cid','pid','bioyear'],how='inner')
dfMerge2 = dfMerge2[dfMerge2['bioage'].between(20,35)]
print("dfMerge2:" + str(dfMerge2.shape))


# In[8]:


dfMerge3 = pd.merge(biojob,dfMerge2,on=['cid','pid','bioyear'],how='inner')
print("dfMerge3:" + str(dfMerge3.shape))


# In[9]:


#dfMerge4 = pd.merge(bioimmig,dfMerge3,on=['cid','pid'],how='right')
#print("dfMerge4:" + str(dfMerge4.shape))


# In[12]:


#Control for whose parent's country not German
df_c1 = dfMerge3[~(dfMerge3['forigin'].isin([1,-2,-5])) &
                 ~(dfMerge3['morigin'].isin([1,-2,-5]))
                ]


# In[13]:


df_c1.shape


# In[16]:


#Control for whose parent's country not German and now the nationality is German
df_c2 = dfMerge3[~(dfMerge3['forigin'].isin([1,-2,-5])) &
                 ~(dfMerge3['morigin'].isin([1,-2,-5])) &
                 (dfMerge3['fnat']==1) &
                 (dfMerge3['mnat']==1)
                ]
df_c2.shape


# In[17]:


df_c2[['forigin','morigin','fnat','mnat']]


# ### Descriptive statistic

# In[8]:


plt.figure(figsize=(15, 10))
ax = sns.scatterplot(x='syear',
                     y='',
                     hue='morigin',
                     legend='full',
                     data=df,
                     palette=sns.color_palette("Set1", n_colors=len(df.morigin.unique())))
max_transistors_per_year = df.groupby('syear')[''].max()
sns.lineplot(data=max_transistors_per_year,
             ax=ax.axes,
             color='black')
ax.set_xlim(2006, 2021)
plt.show()


# In[ ]:




