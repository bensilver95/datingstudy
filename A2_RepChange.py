#!/usr/bin/env python3

import pandas as pd
import numpy as np
import pickle
import warnings
from progressbar import ProgressBar

warnings.filterwarnings("ignore", message="Mean of empty slice.")
warnings.filterwarnings("ignore", message="invalid value encountered in divide")

home = '/home/bms2202/dating/'
add = False

tdata = pd.read_csv(home + 'fmri_ratings_fa.csv')

# ask me for this - it's 63 GBs
with open('MV_DATA_ALL.pkl', 'rb') as pickle_file:
    DATA = pickle.load(pickle_file)

pbar = ProgressBar()
sims = pd.DataFrame()
if add:
    old = pd.read_csv('acrossrun.csv',dtype = {'roi':'str'})
    # long thing to get new ROIs
    to_add = set([x for x in DATA.keys( )]) - \
    set([int(item) if item.isdigit() else item for item in np.unique(old['roi']).tolist()])
    rois = to_add
    print('adding the below rois')
    print(rois)
else:
    rois = DATA.keys()

for roi in pbar(rois):
    # speed things up
    if ~np.isnan(DATA[roi]['wholevideo']['DAT003']['Breslin']['run-1']).all():
        for template in DATA[roi]:
            for sub in DATA[roi][template]:
                if sub == 'DAT006':
                    continue
                # get averages for subtraction
                avg_run1 = np.nanmean(np.row_stack([DATA[roi][template][sub][x]['run-1'].mean(axis = 1) \
                                                    for x in DATA[roi][template][sub]]),
                                        axis = 0)
                avg_run2 = np.nanmean(np.row_stack([DATA[roi][template][sub][x]['run-2'].mean(axis = 1) \
                                                    for x in DATA[roi][template][sub]]),
                                        axis = 0)
                
                for pro_run1 in DATA[roi][template][sub]:
                    run1 = DATA[roi][template][sub][pro_run1]['run-1'].mean(axis = 1)
                    if template != 'photos':
                        run1 = run1 - avg_run1
                    
                    feedback = tdata[(tdata['sub'] == sub) & \
                    (tdata['name'] == pro_run1)]['feedback'].to_string(index = False)
                    alignment = tdata[(tdata['sub'] == sub) & \
                    (tdata['name'] == pro_run1)]['congruence'].to_string(index = False)
                    start = tdata[(tdata['sub'] == sub) & \
                    (tdata['name'] == pro_run1)]['slider_assess.response'].to_string(index = False)
                    for pro_run2 in DATA[roi][template][sub]:
                        run2 = DATA[roi][template][sub][pro_run2]['run-2'].mean(axis = 1)
                        if template != 'photos':
                            run2 = run2 - avg_run2
                        
                        #mask = ~np.isnan(DATA2['run-1']) & ~np.isnan(DATA2['run-2'])   
                        temp = pd.DataFrame({'roi':roi,'template':template,
                                                'sub':sub,'profile_run1':pro_run1,
                                                'profile_run2': pro_run2,
                                                'feedback_valence':feedback,
                                                'feedback_alignment':alignment,
                                                'initial_impression':start,
                                                'similarity':np.corrcoef(run1,run2)[1,0]}, 
                                            index = [0]) 
                        sims = pd.concat([sims,temp])

sims['comp_type'] = np.where(sims['profile_run1'] == sims['profile_run2'],'within_pro','between_pro')
if add:
    sims = pd.concat([old,sims])
sims.to_csv('acrossrun.csv',index = False)