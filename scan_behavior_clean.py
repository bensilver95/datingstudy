#!/usr/bin/env python3

import pandas as pd
from glob import glob
import math
import numpy as np
import re
import os
from progressbar import ProgressBar
from rpy2 import robjects

from sentence_transformers import SentenceTransformer, util
model = SentenceTransformer('all-MiniLM-L6-v2')

def cosine_sim(text1,text2):
    cosine_scores = util.cos_sim(text1, text2)
    correct_scores = []
    for i in range(len(cosine_scores)):
        correct_scores.append(cosine_scores[i][i].cpu().numpy().tolist())
    return correct_scores

def ratings_clean(datafile,version):
    if version == 'assess':
        vids = 'video_set1'
        crosshair = 'crosshair_2'
        crosshair2 = 'crosshair'
    elif version == 'reveal':
        vids = 'video_set2'
        crosshair = 'crosshair'
        crosshair2 = 'crosshair_2'

    data = pd.read_csv(datafile)

    end_ratings = data[['name',crosshair2 + '.started']]
    end_ratings = end_ratings[end_ratings[crosshair2 + '.started'].notna()]
    end_ratings = end_ratings.rename({crosshair2 + '.started':'slider_' + version + '.ended'}, axis = 1)

    start = data.loc[1,'waiting_text.started'] + data.loc[1,'trigger_resp.rt']
    data = data[data['question'].notna()]
    data = data.merge(end_ratings)
    data[version + '_video_duration'] = data[crosshair + '.started'] - data['move_on.started']
    data['move_on.started'] = data.groupby('name')['move_on.started'].transform(lambda x: x.fillna(x.mean()))
    data[version + '_video_duration'] = data.groupby('name')[version + '_video_duration'].transform(lambda x: x.fillna(x.mean()))
    if 'endPosX' in data.columns: # fix for button not clicking issue
        data['slider_assess.response'] = np.where(data['slider_assess.response'] == 'None',
                                         data['endPosX'].apply(lambda x: round((x + 0.25) * 9 / 0.5625 + 1, 1) if x == x else x),
                                         data['slider_assess.response'])
    data['slider_assess.response'] = data['slider_assess.response'].replace('None',np.nan).astype(float)
    if version == 'assess':
        data = data[['name','video_set1','move_on.started',version + '_video_duration',
                                   'question','slider_assess.response',
                     'slider_assess.started','slider_' + version + '.ended']]
        data = data.rename({'video_set1':version + '_video',
                                     'move_on.started':version + '_video_onset'}, axis = 1)
        if 'DAT025' in datafile or 'DAT029' in datafile:
            s = datafile.split('data/')[1].split('_assess')[0]
            fix = pd.read_csv(glob('../ScanTask/data/z_fixes_material/' + s + '_assess_*csv')[0])
            fixstart = fix.loc[1,'waiting_text.started'] + fix.loc[1,'trigger_resp.rt']
            fixprofs = fix['name'].dropna().unique()
            for prof in fixprofs:
                data.loc[data['name'] == prof,'assess_video_onset'] = data['assess_video_onset'] - fixstart + start
                data.loc[data['name'] == prof,'slider_assess.started'] = data['slider_assess.started'] - fixstart + start
                data.loc[data['name'] == prof,'slider_assess.ended'] = data['slider_assess.ended'] - fixstart + start

    elif version == 'reveal':
        data['compatibility.started'] = data.groupby('name')['compatibility.started'].transform(lambda x: x.fillna(x.mean()))
        data = data[['name','question','video_set2','move_on.started',version + '_video_duration',
                     'phrase','compatibility.started','slider_assess.response',
                     'slider_assess.started','slider_' + version + '.ended']]
        data = data.rename({'video_set2':version + '_video','slider_assess.response':'slider_reveal.response',
                            'phrase':'feedback','move_on.started':version + '_video_onset',
                           'slider_assess.started':'slider_reveal.started','compatibility.started':'feedback_onset',
                           crosshair2 +'.started':'slider_' + version + '.ended'}, axis = 1) 
        data['feedback'] = data['feedback'].replace({'said that he does not think he is romantically compatible with you.':'neg',
                                                               'said that she does not think she is romantically compatible with you.':'neg',
                                                              'said that he thinks he is romantically compatible with you.':'pos',
                                                              'said that she thinks she is romantically compatible with you.':'pos'})
        data['feedback_onset'] = data['feedback_onset'] - start
    
    data[version + '_video_onset'] = data[version + '_video_onset'] - start
    data['slider_' + version + '.started'] = data['slider_' + version + '.started'] - start
    data['slider_' + version + '.ended'] = data['slider_' + version + '.ended'] - start
    return data

files = glob('../ScanTask/data/DAT*')
subs = sorted(set([s.split('_')[0].split('/')[-1] for s in files]))
#subs.remove('DAT001')
#subs.remove('DAT002')

DATA = pd.DataFrame(columns = ['sub','vid_gender'])
PFDF = pd.DataFrame()

pbar = ProgressBar()
print('cleaning')
for sub in pbar(subs):
    
    ###### ratings, video timing #######
    
    assess_data = ratings_clean(glob('../ScanTask/data/' + sub + '_assess*.csv')[0],'assess')
    reveal_data = ratings_clean(glob('../ScanTask/data/' + sub + '_reveal*.csv')[0],'reveal')
 
    ###### memory #######
    
    if sub != 'DAT019':
        assess_memory = pd.read_csv(glob('../ScanTask/data/' + sub + '_memory_assess*.csv')[0])
        start = assess_memory.loc[1,'trigger_resp.started'] + assess_memory.loc[1,'trigger_resp.rt']
        assess_memory = assess_memory[assess_memory['name'].notna()]
        assess_memory['assess_memory_duration'] = assess_memory['crosshair.started'] - assess_memory['submit.started']   
        assess_memory['assess_memory_onset'] = assess_memory['submit.started'] - start
        assess_memory = assess_memory[['name','trials.thisN','assess_memory_onset','assess_memory_duration']]
        assess_memory = assess_memory.rename(columns = {'trials.thisN':'assess_trialN'})
    else:
        assess_memory = pd.DataFrame({'name':assess_data['name'].unique(),
                                      'assess_trialN':range(8),
                                     'assess_memory_duration':np.nan,
                                     'assess_memory_onset':np.nan})
    
    reveal_memory = pd.read_csv(glob('../ScanTask/data/' + sub + '_memory_reveal*.csv')[0])
    start = reveal_memory.loc[1,'trigger_resp.started'] + reveal_memory.loc[1,'trigger_resp.rt']
    reveal_memory = reveal_memory[reveal_memory['name'].notna()]
    reveal_memory['reveal_memory_duration'] = reveal_memory['crosshair.started'] - reveal_memory['submit.started']
    reveal_memory['reveal_memory_onset'] = reveal_memory['submit.started'] - start
    reveal_memory = reveal_memory[['name','trials.thisN','reveal_memory_onset','reveal_memory_duration']]
    reveal_memory = reveal_memory.rename(columns = {'trials.thisN':'reveal_trialN'})
    
    data = assess_data.merge(reveal_data, on = ['name','question'])
    data = data.merge(assess_memory, on = 'name')
    data = data.merge(reveal_memory, on = 'name')
    data['sub'] = sub
    data['vid_gender'] = glob('../ScanTask/data/' + sub + '_assess*.csv')[0].split('assess_')[1].split('_')[0][0]
    data['feedback.d'] = np.where(data['feedback'] == 'neg',0,1)

    # redo this for each way of summarizing impressions
    data['start.d'] = np.where(data['slider_assess.response'] < 5, 0, 1)
    data['congruence'] = np.where(data['start.d'] == data['feedback.d'],'Congruent','Incongruent')
    data['congruence.d'] = np.where(data['congruence'] == 'Congruent',1,0)
   
    
    DATA = pd.concat([DATA,data], sort = False).reset_index(drop = True)
    
    ###### photos #######

    photos_files = glob('../ScanTask/data/' + sub + '_photos*csv')
    photos_files = sorted(photos_files, key=lambda t: os.stat(t).st_mtime)
    iteration = 0
    
    for pf in photos_files:
        
        pfdf = pd.read_csv(pf)
        if len(pfdf) < 3:
            continue
        
        start = pfdf.loc[1,'waiting_text.started'] + pfdf.loc[1,'trigger_resp.rt']
        pfdf = pfdf.loc[2:,:].reset_index(drop = True)
        pfdf['tar_photo.started'] = pfdf['tar_photo.started'] - start
        pfdf['crosshair.started'] = pfdf['crosshair.started'] - start
        pfdf = pfdf[['participant','type','name','tar_photo.started','crosshair.started']]
        pfdf = pfdf.melt(id_vars = ['participant','type','name'], 
                         value_vars = ['tar_photo.started','crosshair.started'],
                        var_name = 'trial_type', value_name = 'onset').sort_values('onset').reset_index(drop = True)
        pfdf['duration'] = pfdf['onset'].diff()
        pfdf['duration'] = pfdf['duration'].shift(-1)

        pfdf['iteration'] = iteration
        
        ## !! Need to eventually figure out what to do about Nones - replace with liking question or calculate 
        ## response based on mouse position
        #pfdf = pfdf.merge(data[data['question'].str.contains('compatible')][['name','slider_assess.response','feedback','slider_reveal.response']],on = 'name')
        #pfdf['slider_assess.response'] = pfdf['slider_assess.response'].replace('None',np.nan)
        #pfdf['slider_reveal.response'] = pfdf['slider_reveal.response'].replace('None',np.nan)
        
        PFDF = pd.concat([PFDF,pfdf], sort = False).reset_index(drop = True)
        
        iteration = iteration + 1

###### transcript #######
print('loading transcripts')
transcripts = pd.DataFrame({
    'name':['Hannah','Tamyra','Monica','Jessica',
             'Kayla','Taylor','Marissa','Annie',
             'Zach','Breslin','Josh','Landon',
             'Cam','James','Jason','Eddie'],
    'video1_transcript':[
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p01_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p02_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p03_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p05_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p06_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p12_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p13_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p18_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p07_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p08_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p09_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p10_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p14_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p15_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p16_1.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p17_1.txt').read().replace('Speaker 0', ''))
    ],
    'video2_transcript':[
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p01_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p02_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p03_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p05_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p06_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p12_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p13_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/F_p18_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p07_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p08_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p09_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p10_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p14_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p15_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p16_2.txt').read().replace('Speaker 0', '')),
        re.sub(r'\w+:\w+:\w+', '',open('../stimuli/transcripts/M_p17_2.txt').read().replace('Speaker 0', ''))
    ]
})

transcripts = DATA[['sub','name','assess_video','reveal_video']].drop_duplicates().merge(transcripts)
transcripts['assess_transcript'] = np.where(transcripts['assess_video'].str.contains('_1'),
                                           transcripts['video1_transcript'],transcripts['video2_transcript'])
transcripts['reveal_transcript'] = np.where(transcripts['reveal_video'].str.contains('_1'),
                                           transcripts['video1_transcript'],transcripts['video2_transcript'])

memory = pd.read_csv('real/memory_transcripts.csv')
memory['Type'] = memory['Type'].replace({'Assess':'assess',
                                'Reveal':'reveal'})

# do assess and reveal separately because pivoting was giving me issues
memory_at = memory[memory['Type'] == 'assess'].reset_index(drop = True)
memory_at = memory_at.rename(columns = {'Subject':'sub',
                                        'Transcript':'assess_memory',
                                       'Trial':'assess_trialN'})
memory_at = memory_at.merge(DATA[['sub','name','assess_trialN']].drop_duplicates())
memory_at = memory_at.merge(transcripts[['sub','name','assess_transcript']])
memory_at['assess_memory'] = memory_at['assess_memory'].fillna('')

print('assess_memory')
assess_transcript_embeddings = model.encode(memory_at['assess_transcript'], convert_to_tensor = True)
assess_memory_embeddings = model.encode(memory_at['assess_memory'], convert_to_tensor = True)
memory_at['assess_memory_acc'] = cosine_sim(assess_transcript_embeddings,assess_memory_embeddings)
memory_at.loc[memory_at['assess_memory'] == '','assess_memory_acc'] = np.nan


memory_rt = memory[memory['Type'] == 'reveal'].reset_index(drop = True)
memory_rt = memory_rt.rename(columns = {'Subject':'sub',
                                        'Transcript':'reveal_memory',
                                       'Trial':'reveal_trialN'})
memory_rt = memory_rt.merge(DATA[['sub','name','reveal_trialN']].drop_duplicates())
memory_rt = memory_rt.merge(transcripts[['sub','name','reveal_transcript']])
memory_rt['reveal_memory'] = memory_rt['reveal_memory'].fillna('')

print('reveal_memory')
reveal_transcript_embeddings = model.encode(memory_rt['reveal_transcript'], convert_to_tensor = True)
reveal_memory_embeddings = model.encode(memory_rt['reveal_memory'], convert_to_tensor = True)
memory_rt['reveal_memory_acc'] = cosine_sim(reveal_transcript_embeddings,reveal_memory_embeddings)
memory_rt.loc[memory_rt['reveal_memory'] == '','reveal_memory_acc'] = np.nan

DATA = DATA.merge(memory_at[['sub','name','assess_memory','assess_memory_acc']])
DATA = DATA.merge(memory_rt[['sub','name','reveal_memory','reveal_memory_acc']])
DATA = DATA.drop(columns = ['assess_trialN','reveal_trialN'])

## manual memory scores from RAs
print('manual memory')
manmem = pd.read_csv('real/manual_memory.csv')
manmem_sum = manmem.groupby(['Subject','Profile','Video']).sum(numeric_only = True).reset_index()
manmem_sum = manmem_sum.pivot_table(values='Fact_Remembered', 
                                    index=['Subject','Profile'], columns='Video').reset_index()
manmem_sum = manmem_sum.rename(columns = {'Subject':'sub','Profile':'name',
                                          1:'video1_memory_sum',2:'video2_memory_sum'})

manmem_perc = manmem.groupby(['Subject','Profile','Video']).mean(numeric_only = True).reset_index()
manmem_perc = manmem_perc.pivot_table(values='Fact_Remembered', 
                                      index=['Subject','Profile'], columns='Video').reset_index()
manmem_perc = manmem_perc.rename(columns = {'Subject':'sub','Profile':'name',
                                          1:'video1_memory_perc',2:'video2_memory_perc'})

manmem = manmem_sum.merge(manmem_perc).reset_index(drop = True)
manmem = DATA[['sub','name','assess_video','reveal_video']].drop_duplicates().merge(manmem)

manmem['assess_memorysum'] = np.where(manmem['assess_video'].str.contains('_1.'),
                                       manmem['video1_memory_sum'],manmem['video2_memory_sum'])
manmem['reveal_memorysum'] = np.where(manmem['reveal_video'].str.contains('_1.'),
                                       manmem['video1_memory_sum'],manmem['video2_memory_sum'])
manmem['assess_memoryperc'] = np.where(manmem['assess_video'].str.contains('_1.'),
                                       manmem['video1_memory_perc'],manmem['video2_memory_perc'])
manmem['reveal_memoryperc'] = np.where(manmem['reveal_video'].str.contains('_1.'),
                                       manmem['video1_memory_perc'],manmem['video2_memory_perc'])
manmem = manmem[['sub','name','assess_memorysum',
                 'reveal_memorysum','assess_memoryperc','reveal_memoryperc']]
DATA = DATA.merge(manmem, how = 'left')

###### next day memory #######
print('next day memory')

nextday_raw = pd.read_csv('real/next_day_survey.csv', skiprows = [1,2])

nextday_raw['Q4'] = nextday_raw['Q4'].str.lower()
subs = pd.read_csv('real/subs.csv')
nextday = nextday_raw.merge(subs, left_on = 'Q4',right_on = 'Email.Q')
nextday = nextday[nextday['Finished'] == 1]

nextday = nextday.loc[:,~nextday.columns.str.endswith('First Click')]
nextday = nextday.loc[:,~nextday.columns.str.endswith('Last Click')]
nextday = nextday.loc[:,~nextday.columns.str.endswith('Page Submit')]
nextday = nextday.loc[:,~nextday.columns.str.endswith('Click Count')]

nextday_men = nextday[nextday['Q5'] == 1]
nextday_men = pd.concat([nextday_men['sub'],nextday_men.filter(like = 'Q57')], axis = 1)
nextday_men = pd.melt(nextday_men, id_vars='sub', value_vars=nextday_men.columns[1:], 
                   var_name='name', value_name='response')

nextday_women = nextday[nextday['Q5'] == 2]
nextday_women = pd.concat([nextday_women['sub'],nextday_women.filter(like = 'Q2')], axis = 1)
nextday_women = pd.melt(nextday_women, id_vars='sub', value_vars=nextday_women.columns[1:], 
                   var_name='name', value_name='response')

nextday = pd.concat([nextday_men,nextday_women])

nextday['name'] = nextday['name'].replace({'1_Q2':'Hannah','3_Q2':'Tamyra','5_Q2':'Monica','7_Q2':'Jessica',
                                           '9_Q2':'Kayla','11_Q2':'Taylor','13_Q2':'Marissa','15_Q2':'Annie',
                                           '1_Q57':'Zach','2_Q57':'Breslin','3_Q57':'Josh','4_Q57':'Landon',
                                          '5_Q57':'Cam','6_Q57':'James','7_Q57':'Jason','8_Q57':'Eddie'})
nextday = nextday.sort_values('sub')

nextday = nextday.merge(transcripts)

response_embeddings = model.encode(nextday['response'], convert_to_tensor = True)

print('next day assess memory')
assess_transcript_embeddings_nextday = model.encode(nextday['assess_transcript'], convert_to_tensor = True)
nextday['assess_memorynextday'] = cosine_sim(response_embeddings,assess_transcript_embeddings_nextday)

print('next day reveal memory')
reveal_transcript_embeddings_nextday = model.encode(nextday['reveal_transcript'], convert_to_tensor = True)
nextday['reveal_memorynextday'] = cosine_sim(response_embeddings,reveal_transcript_embeddings_nextday)

DATA = DATA.merge(nextday[['sub','name','assess_memorynextday','reveal_memorynextday']], how = 'left')

####### save to files #######
DATA.to_csv('real/fmri_ratings_allqs.csv',index = False)

PFDF = PFDF.rename(columns = {'type':'stim_gender'})
PFDF['trial_type'] = PFDF['trial_type'].replace({'tar_photo.started':'photo',
                                                        'crosshair.started':'crosshair'})
PFDF = PFDF[PFDF['duration'].notna()].reset_index(drop = True)

PFDF.to_csv('real/fmri_photos.csv',index = False)

##### average all impressions except for attractive #####
impression_average = DATA[~DATA['question'].str.contains('attractive')].groupby(['sub','name']).mean(numeric_only = True)[['slider_assess.response','slider_reveal.response']].reset_index()
DATA_av = DATA[DATA['question'].str.contains('compatible')]
DATA_av = DATA_av.drop(['slider_assess.response','slider_reveal.response'], axis = 1)
DATA_av = DATA_av.merge(impression_average)

DATA_av['start.d'] = np.where(DATA_av['slider_assess.response'] < 5, 0, 1)
DATA_av['congruence'] = np.where(DATA_av['start.d'] == DATA_av['feedback.d'],'Congruent','Incongruent')
DATA_av['congruence.d'] = np.where(DATA_av['congruence'] == 'Congruent',1,0)

DATA_av.to_csv('real/fmri_ratings_av.csv',index = False)

##### factor analysis ########

with open("scan_behavior_clean_fa.R", "r") as file:
    r_script = file.read()
robjects.r(r_script)

DATA_fa_temp = pd.read_csv('real/fmri_ratings_fa_temp.csv')
DATA_fa = DATA[DATA['question'].str.contains('compatible')]
DATA_fa = DATA_fa.drop(['slider_assess.response','slider_reveal.response'], axis = 1)
DATA_fa = DATA_fa.merge(DATA_fa_temp)

DATA_fa['start.d'] = np.where(DATA_fa['slider_assess.response'] < 5, 0, 1)
DATA_fa['congruence'] = np.where(DATA_fa['start.d'] == DATA_fa['feedback.d'],'Congruent','Incongruent')
DATA_fa['congruence.d'] = np.where(DATA_fa['congruence'] == 'Congruent',1,0)

DATA_fa.to_csv('real/fmri_ratings_fa.csv',index = False)





