#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep  4 14:45:50 2018

@author: Sebi
"""
import numpy as np
import pandas as pd
from time import time

def unique_list(seq):
    seen = set()
    seen_add = seen.add
    return [x for x in seq if not (x in seen or seen_add(x))]

def winner(df):
    '''Determines the winner and gives out his coder id.'''

    w_idx = df['final_points'].idxmax()
    w_cid = int(df.loc[w_idx, 'coder_id'])
    
    return w_cid

def get_difference_from_winner(df):
    '''Creates a new variable containing the difference between a 
    submission score and the winners latest submission score.'''
    
    start = time()
    df_new = df.loc[:, ['problem_id', 'coder_id', 'submit_time',
                    'submission_point', 'final_points']]
    problems = list(set(df_new.loc[:, 'problem_id']))
    df_new.loc[:, 'winner_score'] = 0
    for p in problems:
        p_df = df_new[df_new['problem_id'] == p]
        
        #identify the winner of the challenge:
        w_id = winner(p_df)
        w_df = p_df[p_df['coder_id'] == w_id].sort_values(by = 'submit_time')
        
        #get current winner score for each submission
        for t, s in zip(list(w_df['submit_time']) , list(w_df['submission_point'])):
            df_new.loc[(df_new['problem_id'] == p) & (df_new['submit_time'] >= t), 
                   'winner_score'] = s
    
    # get difference in scores from winner               
    df_new.loc[:, 'difference_from_winner'] = (df_new['submission_point'] - 
          df_new['winner_score'])
    
    end = time()
    print('Running time:', str(round(end - start, 2)), 'seconds')
    
    return df_new['difference_from_winner']

        
def final_mean(df):
    '''Calculates the mean of the final scores. Each unique coder is only
    counted once. Output as a single float.'''
    
    coders = list(set(df.loc[:, 'coder_id']))
    final_score_ls = [list(df.loc[df['coder_id'] == c, 'final_points'])[0] \
                     for c in coders]
    final_score_mean = np.mean(final_score_ls)
    
    return final_score_mean

def remove_low_scores(df, x = 0.25):
    '''Remove coders with final scores lower than x * mean of final scores.
    Default: x = 0.25'''
    
    start = time()
    problems = list(set(df.loc[:, 'problem_id']))
    keep_ls = []
    for p in problems:
        p_df = df[df['problem_id'] == p]
        fp_mean = final_mean(p_df)
        p_df = p_df[p_df['final_points'] >= x * fp_mean]
        keep_ls.extend(list(p_df.index))
    df_new = df.loc[keep_ls, :]
    end = time()
    print('Running time:', str(round(end - start, 2)), 'seconds')
    print('Removed', str(len(df) - len(df_new)), 'observations with low scores.\n')
        
    return df_new     

def continuous_rank(ser):
    '''Rank a pandas series continuously by value. Assuming series is ordered
    by time.'''
    
    rank_ls = []
    k = len(ser)
    
    # Iterate over the series and identify the ranking at a given moment
    while k > 0:
        r = int(list(ser.iloc[0:k].rank(ascending = False))[k - 1])
        rank_ls.append(r)
        k = k - 1
    rank_ls.reverse()
    
    return rank_ls

def get_current_rank(df, ranking_var = 'submission_point'):
    '''Calculating the current rank of a submission based on ranking the 
    ranking variable. Defail is submission_point'''
    
    start = time()
    df_new = df.copy().sort_values(by = ['submit_time'])
    df_new['current_ranking'] = None

    problems = list(set(df_new.loc[:, 'problem_id']))
    count = 1
    for p in list(problems):
        # Extract the variable to be ranked
        s = pd.Series(df_new.loc[df_new['problem_id'] == p, ranking_var])
        
        # Rank the series:
        r_ls = continuous_rank(s)

        df_new.loc[df_new['problem_id'] == p, 'current_ranking'] = r_ls
        
        if count % 20 == 0:
            print('ranked', str(count) + '/' + str(len(problems)), 'challenges')
    
        count = count + 1
    df_new = df_new.sort_values(by = ['open_time', 'submission_number', 'submit_time'])
    df_new.loc[df_new['problem_id'] == p, 'current_ranking']
    end = time()
    print('\nRanked coders in', str(len(problems)), 'challenges in', 
          str(round(end - start, 2)), 'seconds.')
    
    return df_new['current_ranking']

def get_diff_from_first(df, normalise = False):
    '''Computing the difference in scores to the current number 1.'''
    
    df_new = df.copy().sort_values(by = 'submit_time')
    problems = list(set(df_new['problem_id']))
    df_new['difference_from_first'] = None
    for p in problems:
        p_ls = list(df_new.loc[df_new['problem_id'] == p,'submission_point'])
        p_ls_rv = p_ls[::-1]
        r_ls = list(df_new.loc[df_new['problem_id'] == p,'submission_rank'])
        r_ls_rv = r_ls[::-1]
        f_id_ls = []
        k = 1
        while k <= len(r_ls_rv):
            #points_ls_rv = points_ls.reverse()
            f_id = r_ls_rv.index(1, -k, len(r_ls_rv))
            k = k + 1
            f_id_ls.append(f_id)
        f_ls = [p_ls_rv[f] for f in f_id_ls]
        if normalise == False:
            d_ls = list(pd.Series(p_ls) - pd.Series(f_ls))
        else:
            d_ls = list((pd.Series(p_ls) - pd.Series(f_ls)) / pd.Series(f_ls))
        df_new.loc[df_new['problem_id'] == p, 'difference_from_first'] = d_ls
    
    df_new = df_new.sort_values(by = ['open_time', 'submission_number', 'submit_time'])
    
    return df_new['difference_from_first']

def cv(df, col, res):
    '''Computes the coefficient of variation of var col in df within a number 
    of time windows (defined by res).
    Setting res either to int or to string in the format "parts: int"
    Format string will use a flexible number of time windows dependent on the 
    size of df'''
    
    if type(res) == str:
        if res == 'parts':
            parts = 8
        try:
            parts = int(res.split(': ')[1])
        except:
            print('res variable not in right format! Either use int or string' \
                  'in the format "parts: int".')
            parts = 8
        res = int(round(len(df) / parts, 0))
    td = (max(df['submit_time']) - min(df['submit_time'])) / res
    t = min(df['submit_time'])
    cv_ls = []
    t_ls = []
    while t < max(df['submit_time']):
        df_t = df[(df['submit_time'] >= t) & (df['submit_time'] <= (t + td))]
        cv = df_t[col].std() / df_t[col].mean()
        #if np.isnan(cv):
            #cv = 0
        cv_ls.append(cv)
        t = t + td
        t_ls.append(t)
      
    return pd.DataFrame(cv_ls, t_ls)

def get_score_improvement(df):
    
    df = df.set_index('submit_time')
    # get initial score of each coder:
    init_scores = df.groupby(['problem_id', 'coder_id'])['submission_point', 'open_time'].first()
    # get maximum score of each coder:
    max_scores = df.groupby(['problem_id', 'coder_id'])['submission_point'].max()
    # combine and compute difference:
    learn_df = pd.concat([init_scores, max_scores], axis=1, sort=False)
    learn_df.columns = ['init_score', 'open_time', 'max_score']
    learn_df['learning'] = (learn_df['max_score'] - learn_df['init_score']) / learn_df['max_score']
    learn_df['problem_id'] = learn_df.index.get_level_values('problem_id')
    learn_df.index.names = ['problem', 'coder']
    
    return learn_df

def get_contributor_number(df):
    
    problems = list(set(df.loc[:, 'problem_id']))
    new_df = df.copy()
    new_df = new_df.groupby(['problem_id', 'coder_id']).first()
    new_df.sort_values(by = 'submit_time', inplace = True)
    new_df['count'] = None
    new_df.reset_index(level = 0, drop = False, inplace = True)
    for p in problems:
        count = [i for i in range(1, len(new_df.loc[new_df.problem_id == p, :]) + 1)]
        new_df.loc[new_df.problem_id == p, 'count'] = count
    new_df.loc[new_df.problem_id == 6717, 'count']
    return new_df

def get_dropout_rate(df):
    
    last = df.groupby(['problem_id'])['submit_time'].agg(lambda x: max(x) - (max(x) - min(x)) / 5)
    do_ls = []
    for p in list(set(df['problem_id'])):
        df_p = df[df['problem_id'] == p]
        dropout = (1 - len(df_p[df_p['submit_time'] > last[p]].groupby('coder_id')) / 
                   len(df_p.groupby('coder_id')))
        do_ls.append(dropout)
    do = pd.Series(do_ls)
    do.index = list(set(df['problem_id']))
    
    return do

def get_rank_changes(df):
    
    ranks = df.groupby(['problem_id', 'coder_id', 'submission_number'])['my_current_rank'].max()
    rank_change_ls = []
    for p in list(set(ranks.index.get_level_values('problem_id'))):
        s = []
        for i in range(1, ranks[p].index.get_level_values('submission_number').max()):
            try:
                r_i = ranks.xs([i, p], level = ['submission_number', 'problem_id'])
                r_ii = ranks.xs([i + 1, p], level = ['submission_number', 'problem_id'])
                r_d = list(r_i - r_ii)
                r_d = [abs(r) for r in r_d if ~np.isnan(r) and r < 0]
                s.append(sum(r_d))
            except KeyError:
                print('index', str(i), 'does not exist')
        n = len(ranks.xs(p, level = 'problem_id')) # number of submissions
        m = len(df[df['problem_id'] == p].groupby('coder_id'))
        rank_change = sum(s) / (n * (m * 2))
        rank_change_ls.append(rank_change)
    rc = pd.Series(rank_change_ls)
    rc.index = list(set(ranks.index.get_level_values('problem_id')))
    
    return rc

def get_percentage_change(df):

    df_new = df.copy()
    for p in list(set(df_new['problem_id'])):
        p_df = df_new[df_new['problem_id'] == p]
        for c in list(set(p_df['coder_id'])):
            pch = p_df.loc[p_df['coder_id'] == c, 'submission_point'].pct_change()
            df_new.loc[(df_new['problem_id'] == p) & (df_new['coder_id'] == c), 'pch'] = pch
    return df_new

def get_accumulated_change(df):
    
    ts = df.groupby(['problem_id'])['submit_time'].agg(lambda x: (max(x) - min(x)) / 10)
    ch_df = pd.DataFrame()
    for p in list(set(df['problem_id'])):
        df_p = df[df['problem_id'] == p]
        t = df_p['submit_time'].min()
        ch_p =[]
        t_ls = []
        while t < df_p['submit_time'].max():
            t = t + ts[p]
            test = df_p[df_p['submit_time'] <= t].groupby(['coder_id'])['pch'].sum()
            ch_p.append(test.sum() / len(test))
            t_ls.append(t)
        ch_p_df = pd.DataFrame({'submit_time': t_ls,
                                'pch': ch_p})
        ch_p_df['problem_id'] = p
        ch_df = pd.concat([ch_df, ch_p_df])
    return ch_df

'''
# Removing zero-point-submissions:
data_plots = data_clean[data_clean['submission_point'] != 0]
print('Removed', str(len(data_clean) - len(data_plots)), 'observations with zero-point-submissions.\n')
# Removing coders with low scores:
data_plots = remove_low_scores(data_plots, x = 0.4)

print(str(len(data_plots)), 'observations left.')

data_test = data_plots.copy()

data_test['my_current_ranking'] = get_rank(data_test)'''
