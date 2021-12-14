###### author details: Taegyoon Kim, taegyoon@psu.edu
###### purpose: This script is for computing ICR scores for vaccine-relevance hand-labeling.
###### note: labels are from two independent coders
###### last edit: 14 Dec 2021


# packages -------------------------

packages <- c('psy', 'irr', 'irrCAC')
lapply(packages, library, character.only = TRUE)


# read data -------------------------

path <- '/Users/taegyoon/Google Drive/spap_state/spap_state_vaccine/data/'
df_vaccine_label <- read.csv(paste0(path, 'spap_vaccine_icr_vaccine.csv'))
head(df_vaccine_label, 5)


# compute scores -------------------------

kappa2(df_vaccine_label[, c('label_drew', 'label_taegyoon')], weight = 'unweighted') 
krippen.alpha.raw(df_vaccine_label[c('label_drew', 'label_taegyoon')]) 