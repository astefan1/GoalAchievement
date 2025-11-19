# ==============================================================================
# Descriptive Statistics of key variables
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(brms)
library(psych)

############################# Load data ########################################

GAData <- read.csv("GAData_preprocessed.csv")

######################### Compute Descriptives #################################

GAData$treatment <- factor(GAData$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

GADataSmall <- GAData[,c("baseline_automaticity",
                         "baseline_physAct",
                         "baseline_sociocog_att",
                         "baseline_sociocog_behControl",
                         "baseline_sociocog_subjnorm",
                         "baseline_commitDirect",
                         "baseline_commitIndirect",
                         "post_commitDirect",
                         "post_commitIndirect",
                         "follow_physAct",
                         "follow_automaticity",
                         "treatment")]
allStats <- tapply(GADataSmall, GADataSmall$treatment, describe)

allStatsTable <- rbind(allStats[[1]], allStats[[2]], allStats[[3]], allStats[[4]])

allStatsTable <- allStatsTable[order(allStatsTable$vars), ]

allStatsTable <- allStatsTable[1:44, c("n", "mean", "sd", "median", "min", "max")]
allStatsTable <- data.frame(allStatsTable)

allStatsTable$group <- rep(c("CC", "II", "MC", "MCII"), 11)
allStatsTable <- allStatsTable[, c(7, 1:6)]


