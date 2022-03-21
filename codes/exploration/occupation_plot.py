# -*- coding: utf-8 -*-
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np

def plot_decisions_by_year(df):
    """Plot decisions by year.

    Parameters:
    -----------
    df: pd.DataFrame
        Dataframe consisting of decision data.
        
    Returns:
    --------
        Figure.
    """
    labels = ['blue_collar', 'white_collar','military','civil_servant','self_employment','training_schooling','unemployed']

    fig, ax = plt.subplots(figsize=(8,8))

    shares = df.groupby("syear").occ_choices.value_counts(normalize=True).unstack()[labels] * 100
    # Choices should be ordered: blue_collar, white_collar, military, school, home
    # Black white will be determined via colors here.
    shares.plot.bar(stacked=True, ax=ax, width=0.8)

    #ax.set_xticklabels(np.arange(35, 27, 1), rotation="horizontal")
    ax.yaxis.get_major_ticks()[0].set_visible(False)

    ax.set_ylabel("Share (in %)")
    ax.set_ylim(0, 100)

    ax.legend(
        labels=[label.split("_")[0].capitalize() for label in labels],
        loc="lower center",
        bbox_to_anchor=(0.5, 1.04),
        ncol=5,
    )

def plot_decisions_by_age(df):
    """Plot decisions by age < 60.

    Parameters:
    -----------
    df: pd.DataFrame
        Dataframe consisting of decision data.
        
    Returns:
    --------
        Figure.
    """
    labels = ['blue_collar', 'white_collar','military','civil_servant','self_employment','training_schooling','unemployed']

    fig, ax = plt.subplots(figsize=(8,8))

    shares = df.groupby("bioage").occ_choices.value_counts(normalize=True).unstack()[labels] * 100
    # Choices should be ordered: blue_collar, white_collar, military, school, home
    # Black white will be determined via colors here.
    shares.plot.bar(stacked=True, ax=ax, width=0.8)

    #ax.set_xticklabels(np.arange(35, 27, 1), rotation="horizontal")
    ax.yaxis.get_major_ticks()[0].set_visible(False)

    ax.set_ylabel("Share (in %)")
    ax.set_ylim(0, 100)

    ax.legend(
        labels=[label.split("_")[0].capitalize() for label in labels],
        loc="lower center",
        bbox_to_anchor=(0.5, 1.04),
        ncol=5,
    )

def plot_country_origin_by_year(df):
    """Plot decisions by age.

    Parameters:
    -----------
    df: pd.DataFrame
        Dataframe consisting of decision data.
        
    Returns:
    --------
        Figure.
    """
    labels = ['Turkey','Germany','Italy','Former Yugo','Other European','Greece','Poland','Eastern Europe','Middle East',"Africa",'Others']

    fig, ax = plt.subplots(figsize=(12,8))

    shares = df.groupby("syear").country_origin.value_counts(normalize=True).unstack()[labels]*100
    # Black white will be determined via colors here.
    shares.plot.bar(stacked=True, ax=ax, width=0.8)

    ax.set_xlabel("Year")
    #ax.yaxis.get_major_ticks()[0].set_visible(False)

    ax.set_ylabel("Share (in %)")
    ax.set_ylim(0, 100)

    ax.legend(
        labels=[label.split("_")[0].capitalize() for label in labels],
        loc="lower center",
        bbox_to_anchor=(0.5, 1.04),
        ncol=5,
    )

def plot_occupation_by_country_origin(df):
    
    heatmap = pd.crosstab(df.country_origin, df.occ_choices, normalize=True)
    
    fig = plt.figure(figsize=(8,10))
    r = sns.heatmap(heatmap,cmap='OrRd', annot=True)
    
    r.set_title("Heatmap of Occupational choice vs Country of origin")
    