#!/usr/bin/env python3

import pandas as pd
import numpy as np
import pickle
import warnings
from progressbar import ProgressBar
from scipy.spatial.distance import cdist

warnings.filterwarnings("ignore", message="Mean of empty slice.")
warnings.filterwarnings("ignore", message="invalid value encountered in divide")

home = '/home/bms2202/dating/'
add = False # can make True to add ROIs

tdata = pd.read_csv(home + 'fmri_ratings_allqs.csv')
tdata = tdata.sort_values(['sub','name','question'])
tdata = tdata[tdata['sub'] != 'DAT006'].reset_index(drop = True)
profiles = sorted(tdata['name'].unique())

# ask me for this - it's 63 GBs
with open('/MV_DATA_ALL.pkl', 'rb') as pickle_file:
    DATA_T = pickle.load(pickle_file)

ALL = pd.DataFrame()

if add:
    old = pd.read_csv('neurbehav_allqs_noruns_spatial.csv',
                        dtype = {'roi':'str'})
    # long thing to get new ROIs
    to_add = set([x for x in DATA_T.keys( )]) - \
    set([int(item) if item.isdigit() else item for item in np.unique(old['roi']).tolist()])
    rois = to_add
    print('adding the below rois')
    print(rois)
else:
    rois = DATA_T.keys()

for template2 in ['','10secs','last10secs']:
    print(template2)
    pbar = ProgressBar()

    if template2 == '':
        index = (slice(None), slice(None))
    elif template2 == '10secs':
        index = (slice(None),slice(0, 10))
    elif template2 == 'last10secs':
        index = (slice(None),slice(-10, None))
    
    for roi_name in pbar(rois):
        # speed things up
        if ~np.isnan(DATA_T[roi_name]['wholevideo']['DAT003']['Breslin']['run-1']).all():
            for template in ['wholevideo']:
                DATA2 = DATA_T[roi_name][template]
                for p in profiles:
                    for v in ['1','2']:
                        tdata2_assess = tdata[(tdata['name'] == p) & \
                        (tdata['assess_video'].str.contains('_' + v))]
                        subs_assess = tdata2_assess['sub'].unique()
                        bdata_assess = [DATA2[x][p]['run-1'][index].mean(axis = 1) for x in subs_assess]
                        
                        tdata2_reveal = tdata[(tdata['name'] == p) & \
                        (tdata['reveal_video'].str.contains('_' + v))]
                        subs_reveal = tdata2_reveal['sub'].unique()
                        bdata_reveal = [DATA2[x][p]['run-2'][index].mean(axis = 1) for x in subs_reveal]
                        
                        bdata = bdata_assess + bdata_reveal
                        minTRs = min([len(x) for x in bdata])
                        bdata = [x[:minTRs] for x in bdata]
                        bdata_mat = np.corrcoef(bdata)
                        bdata_flat = bdata_mat[np.tril(bdata_mat, k = -1).astype(bool)]
        
                        tdata2_assessp = tdata2_assess.pivot(index = 'sub',columns = 'question',
                                                                values = 'slider_assess.response')
                        tdata2_revealp = tdata2_reveal.pivot(index = 'sub',columns = 'question',
                                                                values = 'slider_reveal.response')
                        tdata2 = pd.concat([tdata2_assessp,tdata2_revealp]).to_numpy()
                        tdata2_mat = cdist(tdata2, tdata2, metric='euclidean')
                        tdata2_flat = tdata2_mat[np.tril(tdata2_mat, k = -1).astype(bool)]
        
                        subs = [x for x in subs_assess] + [x for x in subs_reveal]
                        subs_pairs = [[] for x in range(len(subs))]
                        for i,s in enumerate(subs):
                            for j,s2 in enumerate(subs):
                                subs_pairs[i].append(' '.join([s,s2]))
                        sub_flat = np.array(subs_pairs)[np.tril(tdata2_mat, k = -1).astype(bool)]
                        
        
                        temp = pd.DataFrame({'template2':template2,
                                                'roi':roi_name,'profile':p,'video':v,
                                                'sub1': [x.split(' ')[1] for x in sub_flat],
                                                'sub2': [x.split(' ')[0] for x in sub_flat],
                                            'brain_correlation':bdata_flat,
                                            'behavior_distance':tdata2_flat})
                        ALL = pd.concat([ALL,temp])

if add:
    ALL = pd.concat([old,ALL])
ALL.to_csv('neurbehav_allqs_noruns_spatial.csv',index = False) 