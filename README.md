# The mentalizing network updates neural representations of romantic interest in response to social feedback

This repository contains all of the analysis scripts for an fMRI study of romantic interest updating. The data and the stimuli for this study can be found on the study's [OSF](https://osf.io/qzx3b/) page.

Descriptions of each file:
* `scan_behavior_clean.py` and `scan_behavior_clean_fa.R` are needed to clean up the behavioral and timing data from the scanner. If you'd like access to the raw data files from the scanner, please email me.
* `fmri_analyses_paper.ipynb` mostly demonstrates how I created my ROIs. It also directs you to the correct scripts to wrangle fMRI data for each analysis. These scripts are called `A1_RSA.py`, `A2_RepChange.py`, and `A3_Reactivation.py`. Each of these scripts relies on a .pkl file with all of the fMRI data. That file is 63GB - please email me if you'd like to see it. These scripts then output clean .csv files that can then be used for analyses.
* `analyses.R` runs all of the statistical analyses and creates all of the figures, using clean behavioral and fMRI data. Again, all of that data can be found on the OSF page.
