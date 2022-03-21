"""
Individual questionaire on occupation from 1984 to 2018
"""
import numpy as np
import pandas as pd

def load_pgen():
    #load data
    pgen = pd.read_csv('data\pgen.csv', usecols=['pid',
                                                 'cid',
                                                 'syear',
                                                 'pgstib',
                                                 'pgisco88',
                                                 'pgemplst',
                                                 'pglfs',
                                                 'pgjobch',
                                                 'pgbilzeit',
                                                 'pgbilztev',
                                                 'pgexpft',
                                                 "pgexpue",
                                                 ]
                       )
     #rename the columns
    pgen = pgen.rename(columns={"pgstib": "occupation",
                            "pgemplst": "employment_status",
                            "pgjobch": "occupational_change",
                           "pgexpue": "unemployment_experience",
                           "pgbilzeit": "years_education",
                           "pgbilztev": "change_in_education",
                           'pgexpft':"full_time_experience",
                           "pglfs" : "labor_force_stt"

                           }
                        )
    # consider only people are in working age group
    pgen.drop(pgen[pgen['occupation'] == 13].index,inplace=True) #drop retirement
    
    pgen['blue_collar'] = np.where(pgen['occupation'].between(210,250,inclusive = True),1,0)
    pgen['white_collar'] = np.where(pgen['occupation'].between(510,550,inclusive = True),1,0)
    pgen['military'] = np.where(pgen['occupation']==15,1,0)
    pgen['civil_servant'] = np.where(pgen['occupation'].between(610,660 , inclusive = True),1,0)
    pgen['self_employment'] = np.where(pgen['occupation'].between(410,440, inclusive = True),1,0)
    pgen['unemployed'] = np.where(pgen['occupation'].isin([10,12]),1,0)
    pgen['schooling'] = np.where(pgen['occupation']==11,1,0)
    pgen['apprentice'] = np.where(pgen['occupation'].between(110,150, inclusive = True),1,0)
    
    conditions = [pgen['occupation'].between(210,250,inclusive = True),
                  pgen['occupation'].between(510,550,inclusive = True),
                  pgen['occupation']==15,
                  pgen['occupation'].between(410,440,inclusive = True),
                  pgen['occupation'].between(610,660 ,inclusive = True),
                  pgen['occupation']==11,
                  pgen['occupation'].between(110,150, inclusive = True),
                  pgen['occupation'].isin([10,12])
             ]
    choices  = ['blue_collar', 'white_collar','military','self_employment','civil_servant', 'training_schooling','training_schooling','unemployed']
    pgen['occ_choices'] =  np.select(conditions,choices, default=np.nan)
    
    return pgen

def merge_ppathl_paren(pgen):
    
    ppathl =  pd.read_csv("data\ppathl.csv", usecols=['pid',
                                                      'syear',
                                                      'gebjahr',
                                                      'sex',
                                                      'immiyear',
                                                      'germborn',
                                                      'migback',
                                                      'phrf',
                                                      'phrfe'
                                                      ]
                          )
    bioparen = pd.read_csv("data/bioparen.csv", usecols=['pid',
                                                         'fsedu',
                                                         'msedu',
                                                         'locchildh',
                                                         'fprofstat',
                                                         'mprofstat',
                                                         'morigin',
                                                         'forigin',
                                                         'fprofedu',
                                                         'mprofedu'
                                                         ]
                           )
    ppathl_gen = pd.merge(ppathl,pgen,
                   on = ['pid','syear'],
                   how ='inner'
                   )
    ppathl_gen_paren = pd.merge(ppathl_gen, bioparen,
                   on = 'pid',
                   how ='inner'
                   )
    #create info of individual age at survey year
    ppathl_gen_paren['bioage'] = ppathl_gen_paren['syear'] - ppathl_gen_paren['gebjahr']

    # consider only people are in working age group
    ppathl_gen_paren.drop(ppathl_gen_paren[ppathl_gen_paren['bioage'] > 60].index,inplace=True) #drop age >60
    
    return ppathl_gen_paren

def convert_country_origin(ppathl_gen_paren):

    forigin_info = pd.read_csv('data/forigin_info.csv')
    # Create columns of father country of origin
    dict_forigin = {row['value'] : row['group'] for i, row in forigin_info.iterrows()}  # dictionary
    
    #Then replace categorical variables in dataset
    ppathl_gen_paren['forigin_group'] = ppathl_gen_paren['forigin'].replace(to_replace=dict_forigin.keys(), value=dict_forigin.values())
    ppathl_gen_paren['morigin_group'] = ppathl_gen_paren['morigin'].replace(to_replace=dict_forigin.keys(), value=dict_forigin.values())
    ppathl_gen_paren["country_origin"] = np.where(ppathl_gen_paren['forigin_group'] !="invalid",
                                              ppathl_gen_paren['forigin_group'],
                                              ppathl_gen_paren['morigin_group']
                                             )
    ppathl_gen_paren["country_origin"] = np.where(ppathl_gen_paren['migback']==1,'Germany',ppathl_gen_paren["country_origin"])
    
    return ppathl_gen_paren

def convert_father_training(df):
    conditions = [df['fprofedu']==10,
                  df['fprofedu'].between(20,23,inclusive = True),
                  df['fprofedu']==24,
                  df['fprofedu']==25,
                  df['fprofedu']==26,
                  df['fprofedu']==28,
                  df['fprofedu'].isin([27,30]),
                  df['fprofedu'].isin([31,32]),
                  df['fprofedu'] == 40,
                  df['fprofedu'].isin([50,51])
             ]
    choices  = ['no_vocational_degree','gen_vocational_degree','trade_farm','business','healthcare', 'civil_service','tech_engineer','college_uni','others','in_training']
    df['father_training'] =  np.select(conditions,choices, default=np.nan)

    return df

