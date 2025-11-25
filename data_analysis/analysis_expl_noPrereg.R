# ==============================================================================
# NON-PREREGISTERED EXPLORATORY ANALYSES: t-tests quantifying DV increase
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(BayesFactor)           # Bayesian t-tests

############################## Load data #######################################

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

# Create a variable for all treatment names
whichTreatment <- unique(GAData_ITT$treatment)

################### Conduct tests for physical activity ########################

# Intention to treat data, no exclusions
PAData_ITT <- GAData_ITT[!is.na(GAData_ITT$follow_physAct),]
PA_ITT_Res <- matrix(NA, nrow=4, ncol=5, dimnames = list(1:4, c("Condition", "d", "CI_lower", "CI_upper", "BF")))
PA_ITT_Res <- as.data.frame(PA_ITT_Res)
for(i in 1:4){ # for each condition
  # Compute BF
  tempBF <- ttestBF(PAData_ITT$follow_physAct[PAData_ITT$treatment == whichTreatment[i]],
                    PAData_ITT$baseline_physAct[PAData_ITT$treatment == whichTreatment[i]],
                    paired = TRUE) 
  # Save posterior draws
  tempdraws <- ttestBF(PAData_ITT$follow_physAct[PAData_ITT$treatment == whichTreatment[i]],
                       PAData_ITT$baseline_physAct[PAData_ITT$treatment == whichTreatment[i]],
                       paired = TRUE,
                       posterior = TRUE,
                       iterations = 100000) # posterior draws
  # Compute effect size estimate and 95%CI                 
  tempD <- c(summary(tempdraws)$statistics["delta", "Mean"],
             summary(tempdraws)$quantiles["delta", c("2.5%", "97.5%")])
  # Save results in table
  PA_ITT_Res[i,] <- c(whichTreatment[i], tempD, tempBF)
  
}

PA_ITT_Res

# Intention to treat data, exclude participants with high GLTEQ scores
PAData_ITT_ExclGLTEQ <- GAData_ITT_ExclGLTEQ[!is.na(GAData_ITT_ExclGLTEQ$follow_physAct),]
PA_ITT_ExclGLTEQ_Res <- matrix(NA, nrow=4, ncol=5, dimnames = list(1:4, c("Condition", "d", "CI_lower", "CI_upper", "BF")))
PA_ITT_ExclGLTEQ_Res <- as.data.frame(PA_ITT_ExclGLTEQ_Res)
for(i in 1:4){ # for each condition
  # Compute BF
  tempBF <- ttestBF(PAData_ITT_ExclGLTEQ$follow_physAct[PAData_ITT_ExclGLTEQ$treatment == whichTreatment[i]],
                    PAData_ITT_ExclGLTEQ$baseline_physAct[PAData_ITT_ExclGLTEQ$treatment == whichTreatment[i]],
                    paired = TRUE) 
  # Save posterior draws
  tempdraws <- ttestBF(PAData_ITT_ExclGLTEQ$follow_physAct[PAData_ITT_ExclGLTEQ$treatment == whichTreatment[i]],
                       PAData_ITT_ExclGLTEQ$baseline_physAct[PAData_ITT_ExclGLTEQ$treatment == whichTreatment[i]],
                       paired = TRUE,
                       posterior = TRUE,
                       iterations = 100000) # posterior draws
  # Compute effect size estimate and 95%CI                 
  tempD <- c(summary(tempdraws)$statistics["delta", "Mean"],
             summary(tempdraws)$quantiles["delta", c("2.5%", "97.5%")])
  # Save results in table
  PA_ITT_ExclGLTEQ_Res[i,] <- c(whichTreatment[i], tempD, tempBF)
  
}

PA_ITT_ExclGLTEQ_Res

# Intention to treat data, exclude participants with > 40 hours 
PAData_ITT_ExclHours <- GAData_ITT_ExclHours[!is.na(GAData_ITT_ExclHours$follow_physAct),]
PA_ITT_ExclHours_Res <- matrix(NA, nrow=4, ncol=5, dimnames = list(1:4, c("Condition", "d", "CI_lower", "CI_upper", "BF")))
PA_ITT_ExclHours_Res <- as.data.frame(PA_ITT_ExclHours_Res)
for(i in 1:4){ # for each condition
  # Compute BF
  tempBF <- ttestBF(PAData_ITT_ExclHours$follow_physAct[PAData_ITT_ExclHours$treatment == whichTreatment[i]],
                    PAData_ITT_ExclHours$baseline_physAct[PAData_ITT_ExclHours$treatment == whichTreatment[i]],
                    paired = TRUE) 
  # Save posterior draws
  tempdraws <- ttestBF(PAData_ITT_ExclHours$follow_physAct[PAData_ITT_ExclHours$treatment == whichTreatment[i]],
                       PAData_ITT_ExclHours$baseline_physAct[PAData_ITT_ExclHours$treatment == whichTreatment[i]],
                       paired = TRUE,
                       posterior = TRUE,
                       iterations = 100000) # posterior draws
  # Compute effect size estimate and 95%CI                 
  tempD <- c(summary(tempdraws)$statistics["delta", "Mean"],
             summary(tempdraws)$quantiles["delta", c("2.5%", "97.5%")])
  # Save results in table
  PA_ITT_ExclHours_Res[i,] <- c(whichTreatment[i], tempD, tempBF)
  
}

PA_ITT_ExclHours_Res

# Per-protocol analysis, no exclusions
PAData_PPA <- GAData_PPA[!is.na(GAData_PPA$follow_physAct),]
PA_PPA_Res <- matrix(NA, nrow=4, ncol=5, dimnames = list(1:4, c("Condition", "d", "CI_lower", "CI_upper", "BF")))
PA_PPA_Res <- as.data.frame(PA_PPA_Res)
for(i in 1:4){ # for each condition
  # Compute BF
  tempBF <- ttestBF(PAData_PPA$follow_physAct[PAData_PPA$treatment == whichTreatment[i]],
                    PAData_PPA$baseline_physAct[PAData_PPA$treatment == whichTreatment[i]],
                    paired = TRUE) 
  # Save posterior draws
  tempdraws <- ttestBF(PAData_PPA$follow_physAct[PAData_PPA$treatment == whichTreatment[i]],
                       PAData_PPA$baseline_physAct[PAData_PPA$treatment == whichTreatment[i]],
                       paired = TRUE,
                       posterior = TRUE,
                       iterations = 100000) # posterior draws
  # Compute effect size estimate and 95%CI                 
  tempD <- c(summary(tempdraws)$statistics["delta", "Mean"],
             summary(tempdraws)$quantiles["delta", c("2.5%", "97.5%")])
  # Save results in table
  PA_PPA_Res[i,] <- c(whichTreatment[i], tempD, tempBF)
  
}

PA_PPA_Res

# Per-protocol analysis, exclude participants with high GLTEQ scores
PAData_PPA_ExclGLTEQ <- GAData_PPA_ExclGLTEQ[!is.na(GAData_PPA_ExclGLTEQ$follow_physAct),]
PA_PPA_ExclGLTEQ_Res <- matrix(NA, nrow=4, ncol=5, dimnames = list(1:4, c("Condition", "d", "CI_lower", "CI_upper", "BF")))
PA_PPA_ExclGLTEQ_Res <- as.data.frame(PA_PPA_ExclGLTEQ_Res)
for(i in 1:4){ # for each condition
  # Compute BF
  tempBF <- ttestBF(PAData_PPA_ExclGLTEQ$follow_physAct[PAData_PPA_ExclGLTEQ$treatment == whichTreatment[i]],
                    PAData_PPA_ExclGLTEQ$baseline_physAct[PAData_PPA_ExclGLTEQ$treatment == whichTreatment[i]],
                    paired = TRUE) 
  # Save posterior draws
  tempdraws <- ttestBF(PAData_PPA_ExclGLTEQ$follow_physAct[PAData_PPA_ExclGLTEQ$treatment == whichTreatment[i]],
                       PAData_PPA_ExclGLTEQ$baseline_physAct[PAData_PPA_ExclGLTEQ$treatment == whichTreatment[i]],
                       paired = TRUE,
                       posterior = TRUE,
                       iterations = 100000) # posterior draws
  # Compute effect size estimate and 95%CI                 
  tempD <- c(summary(tempdraws)$statistics["delta", "Mean"],
             summary(tempdraws)$quantiles["delta", c("2.5%", "97.5%")])
  # Save results in table
  PA_PPA_ExclGLTEQ_Res[i,] <- c(whichTreatment[i], tempD, tempBF)
  
}

PA_PPA_ExclGLTEQ_Res

# Per-protocol analysis, exclude participants with > 40 hours 
PAData_PPA_ExclHours <- GAData_PPA_ExclHours[!is.na(GAData_PPA_ExclHours$follow_physAct),]
PA_PPA_ExclHours_Res <- matrix(NA, nrow=4, ncol=5, dimnames = list(1:4, c("Condition", "d", "CI_lower", "CI_upper", "BF")))
PA_PPA_ExclHours_Res <- as.data.frame(PA_PPA_ExclHours_Res)
for(i in 1:4){ # for each condition
  # Compute BF
  tempBF <- ttestBF(PAData_PPA_ExclHours$follow_physAct[PAData_PPA_ExclHours$treatment == whichTreatment[i]],
                    PAData_PPA_ExclHours$baseline_physAct[PAData_PPA_ExclHours$treatment == whichTreatment[i]],
                    paired = TRUE) 
  # Save posterior draws
  tempdraws <- ttestBF(PAData_PPA_ExclHours$follow_physAct[PAData_PPA_ExclHours$treatment == whichTreatment[i]],
                       PAData_PPA_ExclHours$baseline_physAct[PAData_PPA_ExclHours$treatment == whichTreatment[i]],
                       paired = TRUE,
                       posterior = TRUE,
                       iterations = 100000) # posterior draws
  # Compute effect size estimate and 95%CI                 
  tempD <- c(summary(tempdraws)$statistics["delta", "Mean"],
             summary(tempdraws)$quantiles["delta", c("2.5%", "97.5%")])
  # Save results in table
  PA_PPA_ExclHours_Res[i,] <- c(whichTreatment[i], tempD, tempBF)
  
}

PA_PPA_ExclHours_Res

###################### Conduct tests for automaticity ##########################

# Intention-to-treat analysis

AutoData_ITT <- GAData_ITT[!is.na(GAData_ITT$follow_automaticity),]
Auto_ITT_Res <- matrix(NA, nrow=4, ncol=5, dimnames = list(1:4, c("Condition", "d", "CI_lower", "CI_upper", "BF")))
Auto_ITT_Res <- as.data.frame(Auto_ITT_Res)
for(i in 1:4){ # for each condition
  # Compute BF
  tempBF <- ttestBF(AutoData_ITT$follow_automaticity[AutoData_ITT$treatment == whichTreatment[i]],
                    AutoData_ITT$baseline_automaticity[AutoData_ITT$treatment == whichTreatment[i]],
                    paired = TRUE) 
  # Save posterior draws
  tempdraws <- ttestBF(AutoData_ITT$follow_automaticity[AutoData_ITT$treatment == whichTreatment[i]],
                       AutoData_ITT$baseline_automaticity[AutoData_ITT$treatment == whichTreatment[i]],
                       paired = TRUE,
                       posterior = TRUE,
                       iterations = 100000) # posterior draws
  # Compute effect size estimate and 95%CI                 
  tempD <- c(summary(tempdraws)$statistics["delta", "Mean"],
             summary(tempdraws)$quantiles["delta", c("2.5%", "97.5%")])
  # Save results in table
  Auto_ITT_Res[i,] <- c(whichTreatment[i], tempD, tempBF)
  
}

Auto_ITT_Res

# Per-protocol analysis

AutoData_PPA <- GAData_PPA[!is.na(GAData_PPA$follow_automaticity),]
Auto_PPA_Res <- matrix(NA, nrow=4, ncol=5, dimnames = list(1:4, c("Condition", "d", "CI_lower", "CI_upper", "BF")))
Auto_PPA_Res <- as.data.frame(Auto_PPA_Res)
for(i in 1:4){ # for each condition
  # Compute BF
  tempBF <- ttestBF(AutoData_PPA$follow_automaticity[AutoData_PPA$treatment == whichTreatment[i]],
                    AutoData_PPA$baseline_automaticity[AutoData_PPA$treatment == whichTreatment[i]],
                    paired = TRUE) 
  # Save posterior draws
  tempdraws <- ttestBF(AutoData_PPA$follow_automaticity[AutoData_PPA$treatment == whichTreatment[i]],
                       AutoData_PPA$baseline_automaticity[AutoData_PPA$treatment == whichTreatment[i]],
                       paired = TRUE,
                       posterior = TRUE,
                       iterations = 100000) # posterior draws
  # Compute effect size estimate and 95%CI                 
  tempD <- c(summary(tempdraws)$statistics["delta", "Mean"],
             summary(tempdraws)$quantiles["delta", c("2.5%", "97.5%")])
  # Save results in table
  Auto_PPA_Res[i,] <- c(whichTreatment[i], tempD, tempBF)
  
}

Auto_PPA_Res

################### Conduct tests for direct goal commitment ###################


