#!/usr/bin/env python3

import os
import h5py
import pandas as pd
import numpy as np
from glob import glob
import nibabel as nib
import pickle
import warnings
from progressbar import ProgressBar

warnings.filterwarnings("ignore", message="Mean of empty slice.")
warnings.filterwarnings("ignore", message="invalid value encountered in divide")

def parc_files(p):
    wholebrain = '/home/bms2202/dating/masks/resampled/group_fmriprep/wholebrain.nii.gz'
    parcel_file = '/home/bms2202/dating/masks/resampled/group_fmriprep/shen.nii.gz'
    denoised = home + 'denoised_' + p
    analyses = home + 'analyses_' + p

    return denoised, analyses, wholebrain, parcel_file

# pick ROI
def pick_roi2(roi_name, wholebrain,parc_file):

    if type(roi_name) == str:
        roi_file = os.path.dirname(parc_file) + '/' + roi_name + '.nii.gz'
        roi_mask = nib.load(roi_file).get_fdata().astype(bool)
        wholebrain_mask = nib.load(wholebrain).get_fdata().astype(bool)
        roi_mask = roi_mask[wholebrain_mask]
    else:
        roi_file = parc_file
        roi_mask = nib.load(roi_file).get_fdata()
        wholebrain_mask = nib.load(wholebrain).get_fdata().astype(bool)
        roi_mask = roi_mask[wholebrain_mask]
        roi_mask = np.isin(roi_mask,roi_name)
    return roi_mask

home = '/home/bms2202/dating/'
add = False

for preproctype in ['fmriprep']:

    denoised, analyses, wholebrain, parc_file = parc_files(preproctype)
    
    subs = sorted(glob(denoised + '/*/sub*_ratings_run-1.h5'))
    subs = [x for x in subs if 'DAT006' not in x] # fix
    subs = [x for x in subs if 'DAT021' not in x] # fix
    subs = [sub.split('sub-')[1].split('_')[0] for sub in subs]
    subs = np.unique(subs)

    # ask me for this - it's 63 GB
    with open('MV_DATA_ALL.pkl', 'rb') as pickle_file:
        DATA = pickle.load(pickle_file)
    
    all_subs = pd.DataFrame()
    pbar = ProgressBar()

    if add:
        old = pd.read_csv('reactivation_wholerest.csv',
                          dtype = {'roi':'str'})
        # long thing to get new ROIs
        to_add = set([x for x in DATA.keys( )]) - \
        set([int(item) if item.isdigit() else item for item in np.unique(old['roi']).tolist()])
        rois = to_add
        print('adding the below rois')
        print(rois)
    else:
        rois = DATA.keys()
    
    for roi_name in pbar(rois):
        # speed things up
        if ~np.isnan(DATA[roi_name]['wholevideo']['DAT003']['Breslin']['run-1']).all():

            roi_mask = pick_roi2(roi_name,wholebrain,parc_file)
            
            for sub in subs:
                for templaterun in ['1','2']:
                    for restrun in ['0','1','2']:
                        restdf = pd.DataFrame()
                        rest = h5py.File(f'{denoised}/sub-{sub}_rest_run-{restrun}.h5')

                        rest = rest['data']['Vol'][:]
                        rest = rest[roi_mask]
        
                        for templatetype in ['wholevideo','recall']:
        
                            if (sub == 'DAT019') & (templatetype == 'recall'):
                                continue # fix for DAT019 recall issue
        
                            data_temp = DATA[roi_name][templatetype][sub]
                            for profile in data_temp:
                                template = data_temp[profile]['run-' + templaterun].mean(axis = 1)
                                restdf[profile] = np.corrcoef(template,rest.T)[0][1:]
        
                            if templatetype == 'wholevideo':
                                if restrun == '0':
                                    pctiles_wv = restdf.quantile(.95)
                                replays = pd.DataFrame(np.where(restdf > pctiles_wv, 1, 0), columns=restdf.columns)
                            if templatetype == 'photos':
                                if restrun == '0':
                                    pctiles_pv = restdf.quantile(.95)
                                replays = pd.DataFrame(np.where(restdf > pctiles_pv, 1, 0), columns=restdf.columns)
                            if templatetype == 'recall':
                                if restrun == '0':
                                    pctiles_re = restdf.quantile(.95)
                                replays = pd.DataFrame(np.where(restdf > pctiles_re, 1, 0), columns=restdf.columns)
                            
                            replays['Any'] = replays.sum(axis = 1)
                            replays['Any'] = np.where(replays['Any'] == 0,0,1)
                            
                            replays['sub'] = sub
                            replays['templaterun'] = templaterun
                            replays['restrun'] = restrun
                            replays['template'] = templatetype
                            replays['roi'] = roi_name
                            replays['rest_segment'] = ['early']*130 + ['middle']*130 + ['late']*130
        
                            all_subs = pd.concat([all_subs,replays], axis = 0)
                                
    # get rid of template comparisons that don't make sense
    condition = (((all_subs['templaterun'] == '1') & (all_subs['restrun'] == '0')) | \
                 ((all_subs['templaterun'] == '1') & (all_subs['restrun'] == '1')) | \
                 ((all_subs['templaterun'] == '1') & (all_subs['restrun'] == '2')) | \
                 ((all_subs['templaterun'] == '2') & (all_subs['restrun'] == '1')) | \
                 ((all_subs['templaterun'] == '2') & (all_subs['restrun'] == '2')))
    all_subs = all_subs[condition]

    # save sums for entire run
    all_subs_summary = all_subs.groupby(by = ['roi', 'template', 'sub','templaterun','restrun']).sum(numeric_only = True,min_count = 1).reset_index().melt(id_vars = ['roi','template','sub','templaterun','restrun'],var_name = 'profile',value_name = 'rf')
    all_subs_summary = all_subs_summary[all_subs_summary['rf'].notna()]
    #all_subs_summary = all_subs_summary.drop('templaterun', axis = 1).rename(columns = {'restrun':'run'})
    all_subs_summary = all_subs_summary.rename(columns = {'restrun':'run'})
    if add:
        all_subs_summary = pd.concat([old,all_subs_summary])
    all_subs_summary.to_csv('reactivation_wholerest.csv', index = False)

    # save sums for each rest segment
    all_subs_summary = all_subs.groupby(by = ['roi', 'template', 'sub', 'templaterun', 'restrun', 'rest_segment']).sum(numeric_only = True,min_count = 1).reset_index().melt(id_vars = ['roi', 'template', 'sub', 'templaterun','restrun','rest_segment'],var_name = 'profile',value_name = 'rf')
    all_subs_summary = all_subs_summary[all_subs_summary['rf'].notna()]
    all_subs_summary = all_subs_summary.drop('templaterun', axis = 1).rename(columns = {'restrun':'run'})
    all_subs_summary.to_csv('reactivation_restsegments.csv', index = False)