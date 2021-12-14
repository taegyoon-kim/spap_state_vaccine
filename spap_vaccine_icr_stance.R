###### author details: Taegyoon Kim, taegyoon@psu.edu
###### purpose: This script is for computing ICR scores for anti-vax hand-labeling.
###### note: labels are from two independent coders (-1: anti, 0: neutral, 1: pro, 9: unrelated to vaccine)
###### last edit: 14 Dec 2021


# packages -------------------------

packages <- c('psy', 'irr', 'irrCAC')
lapply(packages, library, character.only = TRUE)


# read data -------------------------

path <- '/Users/taegyoon/Google Drive/spap_state/spap_state_vaccine/classification/'
df_stance_label <- read.csv(paste0(path, 'spap_vaccine_icr_stance.csv'))
df_stance_label <- df_stance_label[which(df_stance_label$stance_final != 9), ]


# compute scores -------------------------

kappa2(df_stance_label[, c('stance_binary_anti_rest_taegyoon', 'stance_binary_anti_rest_drew')], weight = 'unweighted') 
krippen.alpha.raw(df_stance_label[, c('stance_binary_anti_rest_taegyoon', 'stance_binary_anti_rest_drew')]) 
