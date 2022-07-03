#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 20 16:52:14 2018

@author: Sebi
"""

import pandas as pd
import numpy as np

from utils import myutils


# CONVERT TO TIMESERIES:
def to_timeseries(df, value_col = 'submission_point', verbose = False):
    '''Converting a data to time series format'''
    
    def rearrange(d_f, values, verb = False):
        '''Rearranging to coders as columns, time as index, submission points
        as values'''
        
        if verb == True:
            print('Rearranging data...')
        converted = pd.pivot_table(d_f,
                                   index = ['problem_id', 'submit_time'],
                                   columns = 'coder_id',
                                   values = value_col)
        return converted

    def fill_timeseries(t_ser, verb = False):
        '''Forward fill time series observations'''
        
        if verb == True:
            print('Forward filling time series observations...')
        t_ser_filled = t_ser.copy()
        count = 1
        problems = list(set(t_ser_filled.index.get_level_values(0)))
        for i in problems:
            t_ser_filled.loc[(i, ), ].fillna(method = 'pad', axis = 0, inplace = True)
            if verb == True:
                if count % 10 == 0:
                    print(str(count) + '/' + str(len(problems)))
                count = count + 1
        return t_ser_filled
    
    ts = rearrange(df, values = value_col, verb = verbose)
    ts = fill_timeseries(ts, verb = verbose)
    
    return ts

def get_challenge_ts(tser, problem_id):
    '''Returns only the coders that participated in a challenge. 
    problem_id can be int or list of ints.'''
    try:
        ts_p = tser.loc[(problem_id, ), tser.loc[(problem_id, ), ].notna().any(axis = 0)]
    except KeyError:
        print('Key Error: Problem id(s) "' + str(problem_id) + '" not in dataset!')
        ts_p = None
    return ts_p


# get only the scores of the current first ranks:
def get_first(tser):
    '''Get a timeseries of the highest current score at each point in time.''' 
    
    first = tser.apply(lambda x: x.max(), axis = 1)
    return first
        
def get_lead(tser, verbose = False):
    
    '''Get a timeseries of the leading score up to each point in time.'''
    
    if verbose == True:
        print('Computing highest current score at each point in time...')
    first = get_first(tser)
    if verbose == True:
        print('Computing leading score up to each point in time...')
    problems = myutils.unique_list(first.index.get_level_values(level = 0))
    fr_ls = []
    for p in problems:
        first_ranks = myutils.continuous_rank(first[p]) # continuous ranks
        fr_ls.extend(first_ranks)
    first = first.to_frame(name = 'submission_point')
    first['cr'] = fr_ls
    # set all observations which do not have cont. rank 1 o Nan:
    first.loc[first['cr'] > 1, 'submission_point'] = np.nan 
    # Fill the Nan observations with the leading score:
    first['submission_point'].fillna(method = 'pad', axis = 0, inplace = True)
    
    return first

def dropout_rate(tserp):
    '''Gives out a timeseries of the number of dropouts for one challenge.'''
    
    # Fill existing NAs with 0s
    tserp.fillna(0, inplace = True)
    # Get the percent changes
    tserp = tserp.apply(lambda x: x.pct_change())
    tserp.replace(np.inf, 1, inplace = True)
    # Replace all zeros with nans
    tserp.replace(0, np.nan, inplace = True)
    # Backfill the NAs => The last NAs after which no submission was made
    # stay NAs
    tserp.fillna(method = 'bfill', inplace = True)
    # Count number of nans at each point in time to determine dropouts
    tserp = tserp.apply(lambda x: (len(x) - x.notna().sum()) / len(x),
                        axis = 1)
    
    return tserp
    

def get_dropout_rate(tser):
    '''Get a timeseries of the number dropouts at each point in time for
    all challenges.'''
    
    problems = myutils.unique_list(tser.index.get_level_values(level = 0))
    tser_new = pd.DataFrame()
    for p in problems:
        tser_p = get_challenge_ts(tser, p)
        # Dropouts for this challenge:
        tser_p = dropout_rate(tser_p)
        tser_p.index = pd.MultiIndex.from_tuples([(p, t) for t in tser_p.index])
        tser_p = tser_p.reset_index()
        tser_new = pd.concat([tser_new, tser_p])
    tser_new.columns = ['problem_id', 'submit_time', 'dropout_rate']
    tser_new = tser_new.groupby(['problem_id', 'submit_time']).max()
    
    return tser_new

def restrict_length(t_ser, cutoff = 0.2):
    '''Cutting off a subset of the series at the end and the beginning.'''
    
    tser = t_ser.copy()
    problems = myutils.unique_list(tser.index.get_level_values(level = 0))
    if type(tser) == pd.Series:
        tser = tser.to_frame()
    tser.reset_index(level = 0, inplace = True)
    for p in problems:
        tserp = tser[tser['problem_id'] == p]
        step = (tserp.index.max() - tserp.index.min()) * cutoff
        start = tserp.index.min() + step
        end = tserp.index.max() - step
        t_ls = []
        for t in tserp.index:
            if t < start or t > end:
                t_ls.append(t)
        tser.drop(t_ls, inplace = True)
    tser = tser.reset_index().groupby(['problem_id', 'submit_time']).max()
    return tser


#data_ts = to_timeseries(data_plots, value_col = 'submit_point', verbose = True)
#get_challenge_ts(data_ts, 6717)


