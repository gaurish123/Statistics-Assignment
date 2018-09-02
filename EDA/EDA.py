# -*- coding: utf-8 -*-
"""
Created on Sun Aug 12 20:01:38 2018

@author: Lenovo
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import f_oneway as anova
df_sales = pd.read_csv("data//salesData.csv")

#Finding Null values
print(df_sales.isnull().sum()[df_sales.isnull().sum()>0])

def plot_data(df_sales):
    #Divding categorical and numerical variable
    numerical_columns = list(df_sales.columns[df_sales.dtypes=="int64"])
    numerical_columns = numerical_columns + list(df_sales.columns[df_sales.dtypes=="float64"])
    categorical_columns = list(df_sales.columns[df_sales.dtypes=="object"])
    
    """Useful Visualisations """
    
    #univariate visualisations
    
    print(df_sales.describe())
    df_sales[numerical_columns].hist(figsize=(16,20), bins=50, xlabelsize=8, ylabelsize=8)
    #sns.distplot(df_sales.nItems)
    #sns.lmplot(x="nItems", y="nCats",data = df_sales)
    
    fig, axes = plt.subplots(round(len(categorical_columns) / 2), 2, figsize=(18,12))
    for i, ax in enumerate(fig.axes):
     if i < len(categorical_columns):
         ax.set_xticklabels(ax.xaxis.get_majorticklabels(), rotation=90)
         sns.countplot(x=df_sales[categorical_columns[i]], alpha=0.7, data=df_sales, ax=ax)
    fig.tight_layout()
    plt.show()
    
    #Bivariate Visualisation
    
    fig, ax = plt.subplots(round(len(numerical_columns) / 3), 3, figsize = (18, 12))
    for i, ax in enumerate(fig.axes):
     if i <= len(numerical_columns) - 1:
         sns.regplot(x=df_sales[numerical_columns[i]],y=df_sales['customerDuration'], ax=ax)
    plt.show()
    
    
    fig, ax = plt.subplots(round(len(categorical_columns) / 2), 2, figsize = (18, 12))
    for i, ax in enumerate(fig.axes):
     if i <= len(categorical_columns) - 1:
         sns.boxplot(x=df_sales[categorical_columns[i]],y=df_sales['customerDuration'], ax=ax)
    plt.show()
    
    df_corr = df_sales.drop('customerDuration', axis=1).corr()
    plt.figure(figsize=(12, 10))
    sns.heatmap(df_corr[(df_corr >= 0.5) | (df_corr <= -0.4)],
     cmap='viridis', vmax=1.0, vmin=-1.0, linewidths=0.1,
     annot=True, annot_kws={"size": 8}, square=True);
    
    # Correlation plot
    corr_matrix = df_sales.corr()
    sns.heatmap(corr_matrix, vmax=.3, center=0,square=True, linewidths=.5, cbar_kws={"shrink": .5})
    
    # Pair plot
    sns.pairplot(df_sales)
    

        
    
plot_data(df_sales)

    
    
    # incase of hue sns.pairplot(ad_data,hue='Clicked on Ad',palette='bwr')
    #g = sns.jointplot(x='nItems',y='customerDuration',data=df_sales,color='green')
    #g.plot(sns.regplot, sns.distplot)
    #plt.show()
    
    #g = (sns.jointplot(x="nItems", y="customerDuration", kind='scatter', marginal_kws=dict(bins=15, rug=True),
    #                   data=df_sales.query('nItems < 250'))
    #   .plot_joint(sns.kdeplot))
    #plt.show()