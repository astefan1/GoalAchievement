# ==============================================================================
# Group homogeneity at baseline
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(BayesFactor)

############################# Load data ########################################

# Full dataset
GAData_ITT <- read.csv("GAData_preprocessed.csv")
GAData_PPA <- GAData_ITT[GAData_ITT$PerProtocolAnalyses == "Yes",]

# Exclude participants with high GLTEQ scores
GAData_ITT_ExclGLTEQ <- GAData_ITT[GAData_ITT$follow_physAct < 190, ]
GAData_PPA_ExclGLTEQ <- GAData_PPA[GAData_PPA$follow_physAct < 190, ]

# Exclude participants with > 40 hours per week
hoursPerWeek <- rowSums(GAData_ITT[, c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild")])/2
GAData_ITT_ExclHours <- GAData_ITT[hoursPerWeek <= 40, ]
GAData_PPA_ExclHours <- GAData_ITT[(GAData_ITT$PerProtocolAnalyses == "Yes") & (hoursPerWeek <= 40),]

################### Conduct tests for physical activity ########################
