# ==============================================================================
# EXPLORATORY ANALYSES: Mediation 
# treatment -> experienced automaticity -> physical activity
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(brms)           # Bayesian regression
library(bayestestR)     # Bayesian mediation analysis

############################# Load data ########################################

GAData <- read.csv("GAData_preprocessed.csv")
GAData_ITT <- GAData[GAData$NoDataInPhase2 == FALSE, ] 
GAData_PPA <- GAData_ITT[GAData_ITT$PerProtocolAnalyses == "Yes",]

########################### Data preparation ###################################

# Center physical activity at baseline
GAData_ITT$baseline_physAct_centered <- GAData_ITT$baseline_physAct-mean(GAData_ITT$baseline_physAct)
GAData_ITT$baseline_automaticity_centered <- GAData_ITT$baseline_automaticity-mean(GAData_ITT$baseline_automaticity)
GAData_ITT$treatment <- factor(GAData_ITT$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

GAData_PPA$baseline_physAct_centered <- GAData_PPA$baseline_physAct-mean(GAData_PPA$baseline_physAct)
GAData_PPA$baseline_automaticity_centered <- GAData_PPA$baseline_automaticity-mean(GAData_PPA$baseline_automaticity)
GAData_PPA$treatment <- factor(GAData_PPA$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))


# Exploratory datasets (outlier exclusions)

GAData_ITT_ExclGLTEQ <- GAData_ITT[GAData_ITT$follow_physAct < 190, ]
GAData_ITT$hoursPerWeek <- rowSums(GAData_ITT[, c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild")])/2
GAData_ITT_ExclHours <- GAData_ITT[GAData_ITT$hoursPerWeek <= 40, ]

GAData_PPA_ExclGLTEQ <- GAData_PPA[GAData_PPA$follow_physAct < 190, ]
GAData_PPA$hoursPerWeek <- rowSums(GAData_PPA[, c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild")])/2
GAData_PPA_ExclHours <- GAData_PPA[GAData_PPA$hoursPerWeek <= 40, ]

############################### ITT Analyses ###################################

#### Original dataset ####
f1 <- bf(follow_automaticity ~ baseline_physAct_centered + baseline_automaticity_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + baseline_automaticity_centered + treatment + follow_automaticity)
med1_ITT <- brm(f1+f2+set_rescor(FALSE), 
                data = GAData_ITT,
                iter = 12000,
                chains = 5,
                warmup = 2000,
                file = "../model_fits/exp_medAuto_ITT")

mediation(med1_ITT, treatment = "treatmentcombiTreat", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_ITT, treatment = "treatmentimpInt", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_ITT, treatment = "treatmentmentCont", mediator = "follow_automaticity", centrality = "mean")

#### Exclusions based on extreme GLTEQ scores ####

med1_ITT_ExclGLTEQ <- brm(f1+f2+set_rescor(FALSE), 
                          data = GAData_ITT_ExclGLTEQ,
                          iter = 12000,
                          chains = 5,
                          warmup = 2000,
                          file = "../model_fits/exp_medAuto_ITT_ExclGLTEQ")

mediation(med1_ITT_ExclGLTEQ, treatment = "treatmentcombiTreat", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_ITT_ExclGLTEQ, treatment = "treatmentimpInt", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_ITT_ExclGLTEQ, treatment = "treatmentmentCont", mediator = "follow_automaticity", centrality = "mean")

#### Exclusions based on number of hours ####

med1_ITT_ExclHours <- brm(f1+f2+set_rescor(FALSE), 
                          data = GAData_ITT_ExclHours,
                          iter = 12000,
                          chains = 5,
                          warmup = 2000,
                          file = "../model_fits/exp_medAuto_ITT_ExclHours")

mediation(med1_ITT_ExclHours, treatment = "treatmentcombiTreat", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_ITT_ExclHours, treatment = "treatmentimpInt", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_ITT_ExclHours, treatment = "treatmentmentCont", mediator = "follow_automaticity", centrality = "mean")

############################ Per-Protocol Analyses #############################

#### Original dataset ####
f1 <- bf(follow_automaticity ~ baseline_physAct_centered + baseline_automaticity_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + baseline_automaticity_centered + treatment + follow_automaticity)
med1_PPA <- brm(f1+f2+set_rescor(FALSE), 
                data = GAData_PPA,
                iter = 12000,
                chains = 5,
                warmup = 2000,
                file = "../model_fits/exp_medAuto_PPA")

mediation(med1_PPA, treatment = "treatmentcombiTreat", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_PPA, treatment = "treatmentimpInt", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_PPA, treatment = "treatmentmentCont", mediator = "follow_automaticity", centrality = "mean")

#### Exclusions based on extreme GLTEQ scores ####

med1_PPA_ExclGLTEQ <- brm(f1+f2+set_rescor(FALSE), 
                          data = GAData_PPA_ExclGLTEQ,
                          iter = 12000,
                          chains = 5,
                          warmup = 2000,
                          file = "../model_fits/exp_medAuto_PPA_ExclGLTEQ")

mediation(med1_PPA_ExclGLTEQ, treatment = "treatmentcombiTreat", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_PPA_ExclGLTEQ, treatment = "treatmentimpInt", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_PPA_ExclGLTEQ, treatment = "treatmentmentCont", mediator = "follow_automaticity", centrality = "mean")

#### Exclusions based on number of hours ####

med1_PPA_ExclHours <- brm(f1+f2+set_rescor(FALSE), 
                          data = GAData_PPA_ExclHours,
                          iter = 12000,
                          chains = 5,
                          warmup = 2000,
                          file = "../model_fits/exp_medAuto_PPA_ExclHours")

mediation(med1_PPA_ExclHours, treatment = "treatmentcombiTreat", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_PPA_ExclHours, treatment = "treatmentimpInt", mediator = "follow_automaticity", centrality = "mean")
mediation(med1_PPA_ExclHours, treatment = "treatmentmentCont", mediator = "follow_automaticity", centrality = "mean")
