#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep  3 08:40:03 2018

@author: Sebi
"""

import os
import pandas as pd
import numpy as np

from utils import myutils

# Get data:
def get_data(path, year = 2010):
    '''Loads all csv files of one year directory into a single pandas df. Default
    year = 2010'''
    
    year = str(year)
    os.chdir('data/' + year)
    file_list = os.listdir()
    file_list = [file for file in file_list if '.csv' in file]
    df_ls = []
    for file in file_list:
        print('loading', file + '...')
        
        # only the first csv files has header:
        if file[-5] == '1' and file[-6] == '0':
            head = 0
        else:
            head = None
        # elegant solution but not working:
        #with open(file, newline='') as csvfile:
            #head = csv.Sniffer().has_header(csvfile.readline())
            
        # To handle different separators in csv files set sep to None and use 
        # python engine to automatically detect the seperator via csv.sniffer()
        df = pd.read_csv(file, sep = None, header = head, engine = 'python')
        
        # Get common header:
        if head != None:
            common_header = list(df.columns)
            common_header = [col.strip() for col in common_header]

        df_ls.append(df)
        
    # Set common header:
    for df in df_ls:
        df.columns = common_header
        
    # Combine dataframes:
    full_df = pd.concat(df_ls, sort = False)
    
    # change time columns to datetime class:
    for col in full_df.columns:
        if 'time' in col:
            full_df.loc[:, col] = pd.to_datetime(full_df.loc[:, col])
            
    # Sort data by open time:
    full_df = full_df.sort_values(by=['open_time', 'submission_number', 'submit_time'])
    
    # add unique indices:
    full_df.loc[:, 'unique_submission_id'] = [i for i in range(0, len(full_df))]   
    full_df = full_df.set_index('unique_submission_id')
    
    os.chdir(path)
    
    print('Loaded full', str(year), 'data into one dataframe.')
    return full_df

def get_cleaned_data(path, year = 2010):
    
    data = get_data(path, year = year)
    data_clean = data.drop(['Unnamed: 8', 'Unnamed: 12'], axis=1)
    
    # Removing zero-point-submissions:
    data_plots = data_clean[data_clean['submission_point'] != 0]
    print('Removed', str(len(data_clean) - len(data_plots)),
          'observations with zero-point-submissions.\n')
    # Removing coders with low scores:
    data_plots = myutils.remove_low_scores(data_plots, x = 0.4)
    
    data_plots['submission_rank'] = myutils.get_current_rank(data_plots)
    data_plots.loc[:, 'my_diff_from_first'] = myutils.get_diff_from_first(data_plots)
    data_plots = import_scoring_type(data_plots, path)
    data_plots = import_problem_type(data_plots, path)

    return data_plots

def import_scoring_type(df, pth):
    absolute_problems = list(pd.read_excel(pth + '/data/challenges.xlsx',
                                           sheet_name = 0,
                                           usecols = 0)['absolute score'])
    df['absolute'] = 0
    problems = myutils.unique_list(df['problem_id'])
    for p in problems:
        if p in absolute_problems:
            df.loc[df['problem_id'] == p, 'absolute'] = 1 # Use something quicker than .loc!
    return df

def import_problem_type(df, pth):
    problem_type = pd.read_excel(pth + '/data/challenges.xlsx',
                                 sheet_name = 1,
                                 usecols = [0,1]).set_index('code index').iloc[:, 0]
    problems = myutils.unique_list(df['problem_id'])
    df['problem_type'] = np.nan
    for p in problems:
        try:
            df.loc[df['problem_id'] == p, 'problem_type'] = problem_type[p] 
        except KeyError:
            print('The type of problem', str(p), 'is not defined!')
    return df

'''
user_path = r'/Users/Sebi/Dropbox/Topcoder/analysis'  
data = get_data(user_path)
data_clean = data.drop(['Unnamed: 8', 'Unnamed: 12'], axis=1)'''
