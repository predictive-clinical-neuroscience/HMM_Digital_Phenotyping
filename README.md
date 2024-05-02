# HMM_Digital_Phenotyping
Hidden Markov Models for Digital Phenotyping

In this repository we provide scripts for fitting an HMM to digital phenotyping data using depmixS4 and evaluating the HMM-derived dwell time.

1. create_training_testing_sets.ipynb: this file is used to split the data into training and testing sets. Healthy controls (HCs) are selected with high data availability for training, and the remaining HCs are assigned to testing. Participants belonging to other diagnostic groups and a small number of high data availability HCs are also assigned to testing.

2. apply_depmixS4.ipynb: the HMM is fit to training data in this script. The data is binned and missing values assigned before model training. After training, hidden state sequences are generated for participants and their "dwell time" (% of time spent in a hidden state) calculated.

3. Statistics files
   
   a. stats_social-measures_linear-regression.R: the dwell time is compared to two social measures (social functioning and loneliness) using linear regression models, with models also run for each participant group.

   b. stats_logistic-regression.R: the dwell time of participants from each diagnostic group (schizophrenia, Alzheimer's disease, subjective cognitive complaints) is compared to the dwell time of HCs using multinomial logistic regression. Age sensitivity analysis is also carried out with binomial logistic regression models per diagnostic group.

   c. stats_clinical-measures_linear-regression.R: the dwell time is compared to two clinical measures (cognitive functioning; positive and negative symptoms) using linear regression models. For cognitive functioning, models are also run for each available participant group. For positive/negative symptoms, models are run for the different subscale measures.
