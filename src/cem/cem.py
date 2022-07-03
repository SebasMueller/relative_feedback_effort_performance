#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 24 17:51:39 2019

@author: Sebi
"""

import numpy as np
import pandas as pd


class cem:
    '''Coarsed exact matching object'''

    def __coarsen__(__self):
        '''Create coarsened dataset'''

        # Select defined covariates
        all_variables = __self.covariates + [__self.treatment]
        matching_data = __self.data[all_variables]

        # Make a copy to use as the coarsened dataset
        coarsened_data = matching_data.copy()

        # Coarsen variables
        for var_name, cuts in __self.cutpoints.items():

            # Define the conditions on the edges. Variable lower than lowest cutpoint or higher than highest cutpoint.
            conditions_edges = [
                (coarsened_data[var_name] < cuts[0]).tolist(),
                (coarsened_data[var_name] >= cuts[-1]).tolist()
            ]

            # If more than one cutpoints, define conditions in between. Hihger than the current and lower than the
            #next cutpoint.
            if len(cuts) > 1:
                conditions_between = [
                    ((coarsened_data[var_name] >= cuts[i])  & (coarsened_data[var_name] < cuts[i+1])).tolist()
                    for i in range(len(cuts[0:-1]))
                ]

                # Put it together
                conditions = conditions_edges + conditions_between

            else:
                conditions = conditions_edges

            # Define the list of choices which is 0 to lenght of cutpoints + 1.
            choices = list(range(len(cuts) + 1))

            # Coarsen the data based on the conditions and choices defined above.
            coarsened_data[var_name] = np.select(conditions, choices, default = 0)

        return coarsened_data

    def __match__(coarsened_data, covariates):
        '''Perform the matching on the coarsened dataset'''

        # Extract a list of the values of the rows
        rows = [
            coarsened_data.loc[row, covariates].tolist()
            for row in coarsened_data.index.tolist()
        ]

        # Create a list of all observations that need to be matched
        tomatch = coarsened_data.index.tolist()

        # Create an empty list of matched and not-matched items
        matched = []
        not_matched = []

        # Iterate over each item to match
        for row in tomatch:
            # If item not matched already, find matches and add to matched

            if (row not in [item for sublist in matched for item in sublist] and
                row not in not_matched):
                indices = [i for i, x in enumerate(rows) if x == rows[row]]
                if len(indices) > 1:
                    matched.append(indices)
                else:
                    not_matched.extend(indices)

        # keep only matches that vary on the explanatory variables
        matched_final = []
        not_matched_final = not_matched

        for matches in matched:
            if len(set(coarsened_data.loc[matches, "scoring_type"])) > 1:
                matched_final.append(matches)
            else:
                not_matched_final.extend(matches)

        matched_ids = set([item for sublist in matched_final for item in sublist])

        return matched_final, not_matched_final, matched_ids

    def __match_table__(__self):
        '''Contingency table of matches'''

        # Extract index lists of matched and non-matched observations
        matched_ids = set([item for sublist in __self.matches for item in sublist])
        not_matched_ids = __self.not_matched.copy()

        # Create a match table
        not_matched_bygroup = __self.data.loc[not_matched_ids, :].groupby(__self.treatment).count().iloc[:, 0].rename("not_matched")
        matched_bygroup = __self.data.loc[matched_ids, :].groupby(__self.treatment).count().iloc[:, 0].rename("matched")

        match_table = pd.concat([not_matched_bygroup, matched_bygroup], axis = 1)
        match_table = match_table.assign(sum = match_table.sum(axis = 1))
        match_table.loc["sum", :] = match_table.sum(axis = 0)
        match_table = match_table.astype("int")

        return match_table

    def diff_in_means(self):
        '''Get difference in means for each stratum. '''

        diff_dct = dict()

        for stratum, matches in self.strata.items():
            stratum_data = self.data.loc[matches, self.covariates + [self.treatment]]
            diff_in_means = stratum_data.groupby(self.treatment).mean().diff().iloc[1].to_dict()
            diff_dct[stratum] = diff_in_means

        diffs = pd.DataFrame.from_dict(diff_dct, orient = 'index')

        return diffs

    def __str__(self):
        return "Coarsened exact matching object"

    def __init__(self, data, covariates, treatment, cutpoints):
        self.data = data.reset_index()
        self.covariates = covariates
        self.treatment = treatment
        self.cutpoints = cutpoints

        # coarsen data
        coarsened_data = cem.__coarsen__(self)

        # perform the matching
        self.matches, self.not_matched, self.matched = cem.__match__(coarsened_data, self.covariates)

        # compute table
        self.tab = cem.__match_table__(self)
        print(self.tab)

        # Define strata
        self.strata = {
            i[0]: i[1] for i in list(zip(list(range(len(self.matches))), self.matches))
        }
