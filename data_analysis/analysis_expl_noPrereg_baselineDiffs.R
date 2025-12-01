# ==============================================================================
# Group homogeneity at baseline
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(BayesFactor)

############################# Load data ########################################

# Full dataset
GAData_ITT <- read.csv("GAData_preprocessed.csv")
GAData_ITT$treatment <- factor(GAData_ITT$treatment) # convert treatment into factor
GAData_PPA <- GAData_ITT[GAData_ITT$PerProtocolAnalyses == "Yes",]

################### Conduct tests for physical activity ########################

# ITT Data
1/anovaBF(baseline_physAct ~ treatment, data = GAData_ITT)

# PPA Data
1/anovaBF(baseline_physAct ~ treatment, data = GAData_PPA)

###################### Conduct tests for automaticity ##########################

# ITT Data
1/anovaBF(baseline_automaticity ~ treatment, data = GAData_ITT)

# PPA Data
1/anovaBF(baseline_automaticity ~ treatment, data = GAData_PPA)

################# Conduct tests for direct commitment ##########################

# ITT Data
1/anovaBF(baseline_commitDirect ~ treatment, data = GAData_ITT)

# PPA Data
1/anovaBF(baseline_commitDirect ~ treatment, data = GAData_PPA)

################# Conduct tests for indirect commitment ########################

# ITT Data
1/anovaBF(baseline_commitIndirect ~ treatment, data = GAData_ITT)

# PPA Data
1/anovaBF(baseline_commitIndirect ~ treatment, data = GAData_PPA)
