# ==============================================================================
# EXPLORATORY ANALYSES: Moderation by subjective norms
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(brms)           # Bayesian regression

############################# Load data ########################################

GAData <- read.csv("GAData_preprocessed.csv")
GAData_ITT <- GAData[GAData$NoDataInPhase2 == FALSE, ] 
GAData_PPA <- GAData_ITT[GAData_ITT$PerProtocolAnalyses == "Yes",]

########################### Data preparation ###################################

# Center moderators and physical activity at baseline
GAData_ITT$baseline_sociocog_subjnorm_centered <- GAData_ITT$baseline_sociocog_subjnorm-mean(GAData_ITT$baseline_sociocog_subjnorm)
GAData_ITT$baseline_physAct_centered <- GAData_ITT$baseline_physAct-mean(GAData_ITT$baseline_physAct)
GAData_ITT$treatment <- factor(GAData_ITT$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

GAData_PPA$baseline_sociocog_subjnorm_centered <- GAData_PPA$baseline_sociocog_subjnorm-mean(GAData_PPA$baseline_sociocog_subjnorm)
GAData_PPA$baseline_physAct_centered <- GAData_PPA$baseline_physAct-mean(GAData_PPA$baseline_physAct)
GAData_PPA$treatment <- factor(GAData_PPA$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Exploratory datasets (outlier exclusions)

GAData_ITT_ExclGLTEQ <- GAData_ITT[GAData_ITT$follow_physAct < 190, ]
GAData_ITT$hoursPerWeek <- rowSums(GAData_ITT[, c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild")])/2
GAData_ITT_ExclHours <- GAData_ITT[GAData_ITT$hoursPerWeek <= 40, ]

GAData_PPA_ExclGLTEQ <- GAData_PPA[GAData_PPA$follow_physAct < 190, ]
GAData_PPA$hoursPerWeek <- rowSums(GAData_PPA[, c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild")])/2
GAData_PPA_ExclHours <- GAData_PPA[GAData_PPA$hoursPerWeek <= 40, ]

############################ Prior distributions ###############################

modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma", lb=0))

############################### ITT Analyses ###################################

#### Original dataset ####

# Model fitting
modexp_ITT <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_subjnorm_centered,
                  data = GAData_ITT, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/exp_modNorm_ITT") 

# Parameter estimation
summary(modexp_ITT)

# Hypothesis testing
H_norm1 <- hypothesis(modexp_ITT, "treatmentimpInt:baseline_sociocog_subjnorm_centered = 0")
H_norm1
H_norm2 <- hypothesis(modexp_ITT, "treatmentmentCont:baseline_sociocog_subjnorm_centered = 0")
H_norm2
H_norm3 <- hypothesis(modexp_ITT, "treatmentcombiTreat:baseline_sociocog_subjnorm_centered  = 0")
H_norm3

# Effect size
bayes_R2(modexp_ITT)

#### Exclusions based on extreme GLTEQ scores ####

# Model fitting
modexp_ITT_ExclGLTEQ <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_subjnorm_centered,
                            data = GAData_ITT_ExclGLTEQ, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/exp_modNorm_ITT_ExclGLTEQ") 


# Parameter estimation
summary(modexp_ITT_ExclGLTEQ)

# Hypothesis testing
H_norm1 <- hypothesis(modexp_ITT_ExclGLTEQ, "treatmentimpInt:baseline_sociocog_subjnorm_centered = 0")
H_norm1
H_norm2 <- hypothesis(modexp_ITT_ExclGLTEQ, "treatmentmentCont:baseline_sociocog_subjnorm_centered = 0")
H_norm2
H_norm3 <- hypothesis(modexp_ITT_ExclGLTEQ, "treatmentcombiTreat:baseline_sociocog_subjnorm_centered  = 0")
H_norm3

# Effect size
bayes_R2(modexp_ITT_ExclGLTEQ)

#### Exclusions based on number of hours ####

# Model fitting
modexp_ITT_ExclHours <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_subjnorm_centered,
                            data = GAData_ITT_ExclHours, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/exp_modNorm_ITT_ExclHours") 

# Parameter estimation
summary(modexp_ITT_ExclHours)

# Hypothesis testing
H_norm1 <- hypothesis(modexp_ITT_ExclHours, "treatmentimpInt:baseline_sociocog_subjnorm_centered = 0")
H_norm1
H_norm2 <- hypothesis(modexp_ITT_ExclHours, "treatmentmentCont:baseline_sociocog_subjnorm_centered = 0")
H_norm2
H_norm3 <- hypothesis(modexp_ITT_ExclHours, "treatmentcombiTreat:baseline_sociocog_subjnorm_centered  = 0")
H_norm3

# Effect size
bayes_R2(modexp_ITT_ExclHours)

############################### Per-Protocol Analyses ##########################

#### Original dataset ####

# Model fitting
modexp_PPA <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_subjnorm_centered,
                  data = GAData_PPA, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/exp_modNorm_PPA") 

# Parameter estimation
summary(modexp_PPA)

# Hypothesis testing
H_norm1 <- hypothesis(modexp_PPA, "treatmentimpInt:baseline_sociocog_subjnorm_centered = 0")
H_norm1
H_norm2 <- hypothesis(modexp_PPA, "treatmentmentCont:baseline_sociocog_subjnorm_centered = 0")
H_norm2
H_norm3 <- hypothesis(modexp_PPA, "treatmentcombiTreat:baseline_sociocog_subjnorm_centered  = 0")
H_norm3

# Effect size
bayes_R2(modexp_PPA)

#### Exclusions based on extreme GLTEQ scores ####

# Model fitting
modexp_PPA_ExclGLTEQ <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_subjnorm_centered,
                            data = GAData_PPA_ExclGLTEQ, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/exp_modNorm_PPA_ExclGLTEQ") 


# Parameter estimation
summary(modexp_PPA_ExclGLTEQ)

# Hypothesis testing
H_norm1 <- hypothesis(modexp_PPA_ExclGLTEQ, "treatmentimpInt:baseline_sociocog_subjnorm_centered = 0")
H_norm1
H_norm2 <- hypothesis(modexp_PPA_ExclGLTEQ, "treatmentmentCont:baseline_sociocog_subjnorm_centered = 0")
H_norm2
H_norm3 <- hypothesis(modexp_PPA_ExclGLTEQ, "treatmentcombiTreat:baseline_sociocog_subjnorm_centered  = 0")
H_norm3

# Effect size
bayes_R2(modexp_PPA_ExclGLTEQ)

#### Exclusions based on number of hours ####

# Model fitting
modexp_PPA_ExclHours <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_subjnorm_centered,
                            data = GAData_PPA_ExclHours, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/exp_modNorm_PPA_ExclHours") 


# Parameter estimation
summary(modexp_PPA_ExclHours)

# Hypothesis testing
H_norm1 <- hypothesis(modexp_PPA_ExclHours, "treatmentimpInt:baseline_sociocog_subjnorm_centered = 0")
H_norm1
H_norm2 <- hypothesis(modexp_PPA_ExclHours, "treatmentmentCont:baseline_sociocog_subjnorm_centered = 0")
H_norm2
H_norm3 <- hypothesis(modexp_PPA_ExclHours, "treatmentcombiTreat:baseline_sociocog_subjnorm_centered  = 0")
H_norm3

# Effect size
bayes_R2(modexp_PPA_ExclHours)
