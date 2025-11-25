# ==============================================================================
# Comparison of drop-out rates
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(Bayesrel)       # Bayesian reliability estimation

#############################  Load data #######################################

GAData <- read.csv("GAData_preprocessed.csv")

######################## Compute reliability ###################################

#### Automaticity ####

omega_baseline_automaticity <- strel(GAData[, c("baseline_automaticity1", "baseline_automaticity2", "baseline_automaticity3", "baseline_automaticity4")], 
                                     estimates = "omega", Bayes=TRUE, freq = FALSE)

omega_baseline_automaticity

omega_follow_automaticity <- strel(GAData[, c("follow_automaticity1", "follow_automaticity2", "follow_automaticity3", "follow_automaticity4")],
                                   estimates = "omega", Bayes=TRUE, freq = FALSE)

omega_follow_automaticity

#### Sociocognitive scales ####

omega_baseline_sociocog_att <- strel(GAData[, c("baseline_sociocog_att1", "baseline_sociocog_att2", "baseline_sociocog_att3", "baseline_sociocog_att4ReverseCoded_recoded", "baseline_sociocog_att5ReverseCoded_recoded")],
                                     estimates = "omega", Bayes=TRUE, freq = FALSE)

omega_baseline_sociocog_att

omega_baseline_behControl <- strel(GAData[, c("baseline_sociocog_behControl1", "baseline_sociocog_behControl2ReverseCoded_recoded", "baseline_sociocog_behControl3")],
                                   estimates = "omega", Bayes=TRUE, freq = FALSE)

omega_baseline_behControl

#### Commitment ####

omega_baseline_commitDirect <- strel(GAData[, c("baseline_commitDirect1", "baseline_commitDirect2", "baseline_commitDirect3")],
                                     estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_commitDirect

omega_post_commitDirect <- strel(GAData[, c("post_commitDirect1", "post_commitDirect2", "post_commitDirect3")],
                                       estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_post_commitDirect


omega_baseline_commitIndirect <- strel(GAData[, c("baseline_commitIndirect1", "baseline_commitIndirect2", "baseline_commitIndirect3", "baseline_commitIndirect4")],
                                       estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_commitIndirect

omega_post_commitIndirect <- strel(GAData[, c("post_commitIndirect1", "post_commitIndirect2", "post_commitIndirect3", "post_commitIndirect4")],
                                   estimates = "omega", Bayes=TRUE, freq = FALSE)

omega_post_commitIndirect
