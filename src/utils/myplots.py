#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Sep  4 11:50:50 2018

@author: Sebi
"""
import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

from utils import myutils

def plot_freq_time(df):
    '''Plotting the accumulated number of submissions by time.'''
    
    time_data = df
    time_data = pd.DataFrame(time_data.loc[:, 'submit_time'])
    time_data = time_data.sort_values(by = 'submit_time')
    time_data.loc[:, 'accumulated_submissions'] = [i for i in range(0, len(time_data))] 
    myplot = time_data.plot(x = 'submit_time', y = 'accumulated_submissions',
                            figsize=(13,9),
                            legend = False)
    myplot.set_ylabel('Accumulated submitssions')
    myplot.set_xlabel('Submission time')
    myplot.set_title('Accumulated submissions over time')
    return myplot

def plot_line(df, Y, inverse = False, rows = 15, cols = 5, restrict = False,
              ylab = 'y', xlab = 'submission time',
              title = False,
              group_by = 'problem_id'):
    '''
    Create a set of subplots to line plot each challenge with a user defined 
    variable on the y axis and submit time on the x-axis.
    inverse = bool; If true invert the Y-axis.
    rows = int; Number of rows.
    cols = int; Number of columns.
    restrict = False, int; Cut-off final-score rank for challenges with high number of
                    participants.
    '''
    
    problems = myutils.unique_list(df['problem_id'])
    problems = sorted(problems)
    fig, axes = plt.subplots(nrows = rows,
                             ncols = cols,
                             figsize = (10 + 5 * cols, 10 + 3.2 * rows))
    fig.subplots_adjust(wspace = 0.4, hspace = 1)
    if title != False:
        fig.suptitle(title, fontsize = 14, y = 0.90)
    r = 0
    c = 0
    grouped = df.groupby(group_by)
    for name, group in grouped:
    #for prob in problems:
        print('Problem id:', str(name))
        #print('Problem id:', str(prob))
        plot_data = group.copy()
        #plot_data = df[df['problem_id'] == prob]
        print('\t' + str(len(set(plot_data['coder_id']))), 'coders')
        # Restrict large challenges to speed pu running time:
        if restrict != False:
            if len(set(plot_data['coder_id'])) > restrict:
                print('\tWarning: only plotting the coders with the', str(restrict),
                      'best final scores!')
                fs = list(set(plot_data['final_points']))
                fs.sort(reverse = True)
                if len(fs) > restrict:
                    cut_off = fs[restrict]
                else:
                    cut_off = fs[-1]
                plot_data = plot_data.loc[plot_data['final_points'] > cut_off, :]
            
       # Subplot:
        plot_data.groupby('coder_id').plot(x = 'submit_time',
                                            y = Y,
                                            ax = axes[r, c],
                                            style='.-',
                                            legend = False)
        # Subplot name
        axes[r, c].set_title('Problem id: ' + str(name))
        axes[r, c].set_ylabel(ylab)
        axes[r, c].set_xlabel(xlab)
        # If needed inverse y-axis:
        if inverse == True:
            axes[r, c].invert_yaxis()
        c = c + 1
        if (c) % cols == 0 and r < rows:
            r = r + 1
            c = 0
    # Get rid of subplots not needed:
    while c < cols:
        axes[r, c].axis('off')
        c = c + 1
        
    return fig

def plot_kernel_density(df, Y, rows = 15, cols = 5, xlab = 'final score'):
    '''
    Create a set of subplots plotting the kernel density of a user defined variable.
    rows = int; Number of rows.
    cols = int; Number of columns.
    '''
    
    problems = myutils.unique_list(df['problem_id'])
    problems = sorted(problems)
    fig, axes = plt.subplots(nrows = rows, ncols = cols,
                             figsize = (20,30))
    fig.subplots_adjust(wspace = 0.6, hspace = 1.2)
    r = 0
    c = 0
    for prob in problems:
        print('plotting problem', str(prob))
        plot_data = df[df['problem_id'] == prob]
        max_y = plot_data[Y].max()
        #plot_data.loc[:, 'share'] = plot_data.loc[:, Y] / max_y
        try:
            p_plot = plot_data[Y].plot.kde(ax = axes[r, c],
                                           legend = False)
            success = True
        except:
            print('\tError in kernel density estimation! Skip this challenge.')
            axes[r, c].set_title('Problem id: ' + str(prob))
            success = False
        if success == True:
            p_plot.set_xlim(0, max_y * 1.25)
            p_plot.set_title('Problem id: ' + str(prob))
            axes[r, c].set_xlabel(xlab)
        c = c + 1
        if (c) % cols == 0 and r < rows:
            r = r + 1
            c = 0
    while c < cols:
        axes[r, c].axis('off')
        c = c + 1
    return fig

def plot_hist(df, Y, rows = 15, cols = 5, xlab = 'final score'):
    '''
    Create a set of histogram subplots of a user defined variable Y for
    dataset df.
    rows = int; Number of rows.
    cols = int; Number of columns.
    '''
    
    problems = myutils.unique_list(df['problem_id'])
    problems = sorted(problems)
    fig, axes = plt.subplots(nrows = rows, ncols = cols,
                             figsize = (20,40))
    fig.subplots_adjust(wspace = 0.4, hspace = 1.2)
    r = 0
    c = 0
    for prob in problems:
        print('plotting problem', str(prob))
        plot_data = df[df['problem_id'] == prob]
        p_plot = plot_data[Y].hist(ax = axes[r, c])
        p_plot.set_title('Problem id: ' + str(prob))
        axes[r, c].set_xlabel(xlab)
        axes[r, c].set_ylabel('frequency')
        c = c + 1
        if (c) % cols == 0 and r < rows:
            r = r + 1
            c = 0
    while c < cols:
        axes[r, c].axis('off')
        c = c + 1
    return fig

def plot_cv_time(df, Y, rows = 15, cols = 5, xlab = 'time', ylab = 'cv',
                 res = 10, restrict = False):
    '''
    Creates a set of subplots plotting the coefficient of variation over time
    for variable Y in df.
    Divides the data in time windows (number of windows set by res variable)
    and computes the cv for each time window.
    '''
    
    problems = myutils.unique_list(df['problem_id'])
    problems = sorted(problems)
    fig, axes = plt.subplots(nrows = rows,
                             ncols = cols,
                             figsize = (30, 70))
    fig.subplots_adjust(wspace = 0.4, hspace = 1)
    r = 0
    c = 0
    for p in problems:
        df_p = df[df['problem_id'] == p]
        # Compute the cv for all time windows
        cv_df = myutils.cv(df = df_p, col = Y, res = res)
        # Plot the cv over time
        sns.lineplot(data = cv_df,
                     ax = axes[r, c],
                     markers = True,
                     legend = False)
        axes[r, c].set_title('Problem id: ' + str(p))
        axes[r, c].set_ylabel(ylab)
        axes[r, c].set_xlabel(xlab)
        # Rotate x-axis ticks
        plt.sca(axes[r, c])
        plt.xticks(rotation = 30, ha = 'right')
        c = c + 1
        if c % cols == 0 and r < rows:
            r = r + 1
            c = 0
    # Get rid of subplots not needed:
    while c < cols:
        axes[r, c].axis('off')
        c = c + 1
    return fig      

def plot_violin(df, Y, rows = 15, cols = 5, ylab = 'Improvement / (final score)'):
    
    problems = myutils.unique_list(df['problem_id'])
    problems = sorted(problems)
    fig, axes = plt.subplots(nrows = rows,
                             ncols = cols,
                             figsize = (30, 70),
                             sharey = True,
                             sharex = True)
    fig.subplots_adjust(wspace = 0.0, hspace = 0.3)
    r = 0
    c = 0
    # create violin plot for each problem
    for p in problems:
        df_p = df[df['problem_id'] == p]
        ser_p = pd.Series(df_p[Y])
        sns.violinplot(data = ser_p,
                       ax = axes[r, c],
                       legend = False)
        axes[r, c].set_title('Problem id: ' + str(p))
        axes[r, c].set_ylabel(ylab)
        axes[r, c].set_xlabel('Density')
        axes[r, c].grid()
        axes[r, c].set(xlabel = 'Density', ylabel = ylab)
        axes[r, c].label_outer()
        c = c + 1
        if c % cols == 0 and r < rows:
            r = r + 1
            c = 0
    # Get rid of subplots not needed:
    while c < cols:
        axes[r, c].axis('off')
        c = c + 1
    return fig      

def plot_l(df, Y, rows = 15, cols = 5,
                     xlab = 'Time', ylab = 'Number of contributors'):
    
    problems = myutils.unique_list(df['problem_id'])
    problems = sorted(problems)
    fig, axes = plt.subplots(nrows = rows,
                             ncols = cols,
                             figsize = (25, 55))
    fig.subplots_adjust(wspace = 0.3, hspace = 0.9)
    r = 0
    c = 0
    for p in problems:
        df_p = df.loc[df['problem_id'] == p, :]
        sns.lineplot(data = df_p,
                     x = 'submit_time',
                     y = Y,
                     ax = axes[r, c],
                     markers = True,
                     legend = False)
                # Subplot name
        axes[r, c].set_title('Problem id: ' + str(p))
        axes[r, c].set_ylabel(ylab)
        axes[r, c].set_xlabel(xlab)
        # Rotate x-axis ticks
        plt.sca(axes[r, c])
        plt.xticks(rotation = 30, ha = 'right')
        c = c + 1
        if (c) % cols == 0 and r < rows:
            r = r + 1
            c = 0
    # Get rid of subplots not needed:
    while c < cols:
        axes[r, c].axis('off')
        c = c + 1
        
    return fig

def plot_ts(tser, Y, rows = 19, cols = 4,
            xlab = 'Time', ylab = 'Y',
            title = False, freq = False,
            multlabels = ['Y1', 'Y2']):
    problems = myutils.unique_list(tser.index.get_level_values(0))
    problems = sorted(problems)
    fig, axes = plt.subplots(nrows = rows,
                             ncols = cols,
                             figsize = (10 + 4 * cols, 10 + 2.4 * rows))
    fig.subplots_adjust(wspace = 0.3, hspace = 0.9)
    if title != False:
        fig.suptitle(title, fontsize = 14, y = 0.90)
    r = 0
    c = 0
    for p in problems:
        tser_p = tser.loc[(p, ), ]
        if freq != False:
            tser_p = tser_p.asfreq(freq = freq, method = 'pad')
        df_p = tser_p.reset_index()
        if type(Y) == str:
            Y = [Y]
        else:
            Y = list(Y)
        for Y_val in Y:
            sns.lineplot(data = df_p,
                         x = 'submit_time',
                         y = Y_val,
                         ax = axes[r, c],
                         markers = True,
                         legend = 'brief',
                         label = Y_val)
                # Subplot name
        axes[r, c].set_title('Problem id: ' + str(p))
        axes[r, c].set_ylabel(ylab)
        axes[r, c].set_xlabel(xlab)
        if len(Y) > 1:
            axes[r, c].legend(loc='best', frameon = False)
        # Rotate x-axis ticks
        plt.sca(axes[r, c])
        plt.xticks(rotation = 30, ha = 'right')
        c = c + 1
        if (c) % cols == 0 and r < rows:
            r = r + 1
            c = 0
    # Get rid of subplots not needed:
    while c < cols:
        axes[r, c].axis('off')
        c = c + 1
                
    return fig

def save_to_png(file, name, path = False):
    '''Save a matplotlib figure to png.'''
    
    if path == False:
        try:
            path = user_path
        except NameError:
            print('user_path not defined. Please define path manually when', 
                  'calling the funtion.')
    if path != False:
        path = os.path.dirname(path) + '/plots/'
        file.savefig(path + name + '.png')

'''problems = list(set(data_clean.loc[:, 'problem_id']))
prob = problems[0]
data_new = data_clean[data_clean['problem_id'] == prob]
final_mean = myutils.get_final_mean(data_new)
data_new = data_new[data_new['final_points'] >= 0.25 * final_mean]'''

