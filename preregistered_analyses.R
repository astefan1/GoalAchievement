# ==============================================================================
# Preregistered Analysis Plan
# ==============================================================================

# In the following, we will compute all analyses proposed in the Stage II
# report based on the simulated dataset. Drop-out rates are assumed to be 
# recorded outside the main dataset.

###########################  Load packages #####################################

library(multibridge)    # Bayesian multinomial test
library(Bayesrel)       # Bayesian reliability estimation
library(brms)           # Bayesian regression
library(bayestestR)     # Bayesian mediation analysis
library(readr)          # parse character vectors

############################# Load data ########################################

load("./generated_data/preregDat.RData")
set.seed(1234)
dropout <- sample(1:660, size=50, replace=FALSE)
exclusions <- sample(c(1:660)[!1:660 %in% dropout], size = 20, replace = FALSE)

###################### Drop-Out Rates Comparison ###############################

ss_per_group <- as.vector(table(preregDat$treatment[dropout]))
mult_bf_equality(x = ss_per_group, a = rep(1, 4))

############### Comparison of Participant Exclusion Rates ######################

ss_per_group <- as.vector(table(preregDat$treatment[exclusions]))
mult_bf_equality(x = ss_per_group, a = rep(1, 4))

############ Compute score values and add them to new dataset ##################

datscores <- data.frame(matrix(NA, nrow=660, ncol=0))
datscores$baseline_physAct <- preregDat[, 1]
datscores$baseline_automaticity <- rowMeans(preregDat[, 2:5], na.rm = TRUE)
datscores$baseline_selfdet_amot <- rowMeans(preregDat[, 6:8], na.rm = TRUE)
datscores$baseline_selfdet_extReg <- rowMeans(preregDat[, 9:11], na.rm = TRUE)
datscores$baseline_selfdet_intro <- rowMeans(preregDat[, 12:14], na.rm = TRUE)
datscores$baseline_selfdet_idReg <- rowMeans(preregDat[, 15:17], na.rm = TRUE)
datscores$baseline_selfdet_intReg <- rowMeans(preregDat[, 18:20], na.rm = TRUE)
datscores$baseline_selfdet_intMot <- rowMeans(preregDat[, 21:23], na.rm = TRUE)
datscores$baseline_sociocog_att <- rowMeans(preregDat[, 24:28], na.rm = TRUE)
datscores$baseline_sociocog_subjnorm <- preregDat[, 29]
datscores$baseline_sociocog_behControl <- rowMeans(preregDat[, 30:32], na.rm = TRUE)
datscores$baseline_sociocog_intStrength <- rowMeans(preregDat[, 33:35], na.rm = TRUE)
datscores$treatment <- preregDat[, 36]
datscores$post_energization <- rowMeans(preregDat[, 37:41], na.rm = TRUE)
datscores$post_commitment <- rowMeans(preregDat[, 42:44], na.rm = TRUE)
datscores$post_affcommitment <- rowMeans(preregDat[, 45:47], na.rm = TRUE)
datscores$gender <- preregDat[, 48]
datscores$age <- preregDat[,49]
datscores$follow_physAct <- preregDat[,52]
datscores$follow_automaticity <- rowMeans(preregDat[, 53:56], na.rm = TRUE)

####### Compute McDonalds Omega for all scales consisting of multiple items ####

omega_baseline_automaticity <- strel(preregDat[, 2:5], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_amot <- strel(preregDat[, 6:8], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_extReg <- strel(preregDat[, 9:11], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_intro <- strel(preregDat[, 12:14], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_idReg <- strel(preregDat[, 15:17], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_intReg <- strel(preregDat[, 18:20], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_intMot <- strel(preregDat[, 21:23], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_sociocog_att <- strel(preregDat[, 24:28], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_behControl <- strel(preregDat[, 30:32], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_sociocog_intStrength <- strel(preregDat[, 33:35], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_post_energization <- strel(preregDat[, 37:41], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_post_commitment <- strel(preregDat[, 42:44], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_post_affcommitment <- strel(preregDat[, 45:47], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_follow_automaticity <- strel(preregDat[, 53:56], estimates = "omega", Bayes=TRUE, freq = FALSE)

################################################################################

# INTENTION TO TREAT VS. PER-PROTOCOL ANALYSIS

# From here on out, we will conduct all analyses twice: Once with the entire 
# usable dataset (i.e., preregDat[-dropout, ]), once with the dataset where
# additional participants are excluded who did not follow the instructions or 
# didn't understand the task (see criteria in the manuscript; i.e., 
# preregDat[-c(dropout, exclusions),]).
#
# The first analysis constitutes the "intention to treat" analysis, the second
# analysis the "per-protocol" analysis. We do not expect the results of these
# analyses to differ much, but if there are differences, we will report these
# in the manuscript.

# For simplicity, we will not repeat the analyses in this script, and only
# conduct the per protocol analysis to showcase the statistical procedures.
# The only change in the intention-to-treat-analysis will be that the dataset
# is slightly bigger.

datscores <- datscores[-c(dropout, exclusions),]

############ H1: Hypothesis testing and parameter estimation ###################

# Data preparation
datscores$baseline_physAct_centered <- datscores$baseline_physAct-mean(datscores$baseline_physAct)
datscores$treatment <- factor(datscores$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Prior distributions
modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma"))

# Model fitting
mod1 <- brm(follow_physAct ~ baseline_physAct_centered + treatment, 
            data = datscores, 
            family = "gaussian",
            prior = modelpriors,
            sample_prior = "yes",
            save_pars = save_pars(all = TRUE))

mod1_2 <- brm(follow_physAct ~ baseline_physAct_centered * treatment, 
            data = datscores, 
            family = "gaussian",
            prior = modelpriors,
            sample_prior = "yes",
            save_pars = save_pars(all = TRUE))

# Test whether the assumption of parallel slopes, i.e., no interaction between
# pretest scores and treatment effectiveness. If the interaction model explains
# the data substantially better than the simple ANCOVA model (BF > 6), we will
# interpret and test the coefficients of the interaction model mod1_2; otherwise
# we will interpret the coefficients of the ANCOVA model.

bayes_factor(mod1, mod1_2)

# In the following, we will assume that the data are more likely to have occurred
# under the ANCOVA model, and use mod1 for further analyses.

# Parameter estimation
summary(mod1)

# Hypothesis testing
H1a <- hypothesis(mod1, "treatmentcombiTreat - treatmentmentCont = 0")
H1b <- hypothesis(mod1, "treatmentcombiTreat - treatmentimpInt = 0")
H1c <- hypothesis(mod1, "treatmentcombiTreat = 0")
H1d <- hypothesis(mod1, "treatmentimpInt - treatmentmentCont = 0")
H1e <- hypothesis(mod1, "treatmentmentCont = 0")
H1f <- hypothesis(mod1, "treatmentimpInt = 0")

# Posterior probability if BF > 6
if(H1a$hypothesis$Evid.Ratio < 1/6) H1a_postprob <- hypothesis(mod1, "treatmentcombiTreat - treatmentmentCont > 0")
if(H1b$hypothesis$Evid.Ratio < 1/6) H1b_postprob <- hypothesis(mod1, "treatmentcombiTreat - treatmentimpInt > 0")
if(H1c$hypothesis$Evid.Ratio < 1/6) H1c_postprob <- hypothesis(mod1, "treatmentcombiTreat > 0")
if(H1e$hypothesis$Evid.Ratio < 1/6) H1f_postprob <- hypothesis(mod1, "treatmentmentCont > 0")
if(H1f$hypothesis$Evid.Ratio < 1/6) H1f_postprob <- hypothesis(mod1, "treatmentimpInt > 0")

# Effect size
bayes_R2(mod1)

############ H2: Hypothesis testing and parameter estimation ###################

# Data preparation
datscores$baseline_automaticity_centered <- datscores$baseline_automaticity - mean(datscores$baseline_automaticity)

# Prior distributions
modelpriors <- c(set_prior("normal(0, 1.5)", class = "b"),
                 set_prior("normal(2.3, 1)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.7)", class = "sigma"))

# Model fitting
mod2 <- brm(follow_automaticity ~ baseline_automaticity_centered + treatment, 
            data = datscores, 
            family = "gaussian",
            prior = modelpriors,
            sample_prior = "yes",
            save_pars = save_pars(all = TRUE))

mod2_2 <- brm(follow_automaticity ~ baseline_automaticity_centered + treatment, 
            data = datscores, 
            family = "gaussian",
            prior = modelpriors,
            sample_prior = "yes",
            save_pars = save_pars(all = TRUE))

# Test whether the assumption of parallel slopes, i.e., no interaction between
# pretest scores and treatment effectiveness. If the interaction model explains
# the data substantially better than the simple ANCOVA model (BF > 6), we will
# interpret and test the coefficients of the interaction model mod2_2; otherwise
# we will interpret the coefficients of the ANCOVA model.

bayes_factor(mod2, mod2_2)

# In the following, we will assume that the data are more likely to have occurred
# under the ANCOVA model, and use mod1 for further analyses.

# Parameter estimation
summary(mod2)

# Hypothesis testing
H2a <- hypothesis(mod2, "treatmentcombiTreat - treatmentmentCont = 0")
H2b <- hypothesis(mod2, "treatmentcombiTreat - treatmentimpInt = 0")
H2c <- hypothesis(mod2, "treatmentcombiTreat = 0")
H2d <- hypothesis(mod2, "treatmentimpInt - treatmentmentCont = 0")
H2e <- hypothesis(mod2, "treatmentmentCont = 0")
H2f <- hypothesis(mod2, "treatmentimpInt = 0")

# Posterior probability if BF > 6
if(H2a$hypothesis$Evid.Ratio < 1/6) H2a_postprob <- hypothesis(mod2, "treatmentcombiTreat - treatmentmentCont > 0")
if(H2c$hypothesis$Evid.Ratio < 1/6) H2c_postprob <- hypothesis(mod2, "treatmentcombiTreat > 0")
if(H2d$hypothesis$Evid.Ratio < 1/6) H2d_postprob <- hypothesis(mod2, "treatmentimpInt - treatmentmentCont > 0")
if(H2f$hypothesis$Evid.Ratio < 1/6) H2f_postprob <- hypothesis(mod2, "treatmentimpInt > 0")

# Effect size
bayes_R2(mod2)

############ H3: Hypothesis testing and parameter estimation ###################

#### H3_1: Motivational Goal Commitment ####

# Data preparation
datscores$baseline_sociocog_intStrength_centered <- datscores$baseline_sociocog_intStrength - mean(datscores$baseline_sociocog_intStrength)

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma"))

# Model fitting
mod3_1 <- brm(post_commitment ~ baseline_sociocog_intStrength_centered + treatment,
              data = datscores, 
              family = "gaussian",
              prior = modelpriors,
              sample_prior = "yes",
              save_pars = save_pars(all = TRUE))

mod3_1_2 <- brm(post_commitment ~ baseline_sociocog_intStrength_centered * treatment,
              data = datscores, 
              family = "gaussian",
              prior = modelpriors,
              sample_prior = "yes",
              save_pars = save_pars(all = TRUE))

# Test whether the assumption of parallel slopes, i.e., no interaction between
# pretest scores and treatment effectiveness. If the interaction model explains
# the data substantially better than the simple ANCOVA model (BF > 6), we will
# interpret and test the coefficients of the interaction model mod3_1_2; otherwise
# we will interpret the coefficients of the ANCOVA model.

bayes_factor(mod3_1, mod3_1_2)

# In the following, we will assume that the data are more likely to have occurred
# under the ANCOVA model, and use mod1 for further analyses.
            
# Parameter estimation
summary(mod3_1)

# Hypothesis testing
H31a <- hypothesis(mod3_1, "treatmentcombiTreat - treatmentmentCont = 0")
H31b <- hypothesis(mod3_1, "treatmentcombiTreat - treatmentimpInt = 0")
H31c <- hypothesis(mod3_1, "treatmentcombiTreat = 0")
H31d <- hypothesis(mod3_1, "treatmentimpInt - treatmentmentCont = 0")
H31e <- hypothesis(mod3_1, "treatmentmentCont = 0")
H31f <- hypothesis(mod3_1, "treatmentimpInt = 0")

# Posterior probability if BF > 6
if(H31b$hypothesis$Evid.Ratio < 1/6) H32b_postprob <- hypothesis(mod3_1, "treatmentcombiTreat - treatmentimpInt > 0")
if(H31c$hypothesis$Evid.Ratio < 1/6) H32c_postprob <- hypothesis(mod3_1, "treatmentcombiTreat > 0")
if(H31d$hypothesis$Evid.Ratio < 1/6) H32d_postprob <- hypothesis(mod3_1, "treatmentimpInt - treatmentmentCont < 0")
if(H31e$hypothesis$Evid.Ratio < 1/6) H32e_postprob <- hypothesis(mod3_1, "treatmentmentCont > 0")

# Effect size
bayes_R2(mod3_1)

#### H3_2: Affective Goal Commitment ####

# Model fitting
mod3_2 <- brm(post_affcommitment ~ treatment,
              data = datscores, 
              family = "gaussian",
              prior = modelpriors,
              sample_prior = "yes",
              save_pars = save_pars(all = TRUE))

# Parameter estimation
summary(mod3_2)

# Hypothesis testing
H32a <- hypothesis(mod3_2, "treatmentcombiTreat - treatmentmentCont = 0")
H32b <- hypothesis(mod3_2, "treatmentcombiTreat - treatmentimpInt = 0")
H32c <- hypothesis(mod3_2, "treatmentcombiTreat = 0")
H32d <- hypothesis(mod3_2, "treatmentimpInt - treatmentmentCont = 0")
H32e <- hypothesis(mod3_2, "treatmentmentCont = 0")
H32f <- hypothesis(mod3_2, "treatmentimpInt = 0")

# Posterior probability if BF > 6
if(H32b$hypothesis$Evid.Ratio < 1/6) H32b_postprob <- hypothesis(mod3_2, "treatmentcombiTreat - treatmentimpInt > 0")
if(H32c$hypothesis$Evid.Ratio < 1/6) H32c_postprob <- hypothesis(mod3_2, "treatmentcombiTreat > 0")
if(H32d$hypothesis$Evid.Ratio < 1/6) H32d_postprob <- hypothesis(mod3_2, "treatmentimpInt - treatmentmentCont < 0")
if(H32e$hypothesis$Evid.Ratio < 1/6) H32e_postprob <- hypothesis(mod3_2, "treatmentmentCont > 0")

# Effect size
bayes_R2(mod3_2)

############ H4: Hypothesis testing and parameter estimation ###################

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma"))

# Model fitting
mod4 <- brm(post_energization ~ treatment, 
            data = datscores, 
            family = "gaussian",
            prior = modelpriors,
            sample_prior = "yes",
            save_pars = save_pars(all = TRUE))

# Parameter estimation
summary(mod4)

# Hypothesis testing
H4a <- hypothesis(mod4, "treatmentcombiTreat - treatmentmentCont = 0")
H4b <- hypothesis(mod4, "treatmentcombiTreat - treatmentimpInt = 0")
H4c <- hypothesis(mod4, "treatmentcombiTreat = 0")
H4d <- hypothesis(mod4, "treatmentimpInt - treatmentmentCont = 0")
H4e <- hypothesis(mod4, "treatmentmentCont = 0")
H4f <- hypothesis(mod4, "treatmentimpInt = 0")

# Posterior probability if BF > 6
if(H4b$hypothesis$Evid.Ratio < 1/6) H4b_postprob <- hypothesis(mod4, "treatmentcombiTreat - treatmentimpInt > 0")
if(H4c$hypothesis$Evid.Ratio < 1/6) H4c_postprob <- hypothesis(mod4, "treatmentcombiTreat > 0")
if(H4d$hypothesis$Evid.Ratio < 1/6) H4d_postprob <- hypothesis(mod4, "treatmentimpInt - treatmentmentCont > 0")
if(H4e$hypothesis$Evid.Ratio < 1/6) H4e_postprob <- hypothesis(mod4, "treatmentmentCont > 0")

# Effect size
bayes_R2(mod4)

################# Exploratory Moderation Hypotheses ############################

# Data preparation
datscores$baseline_sociocog_att_centered <- datscores$baseline_sociocog_att-mean(datscores$baseline_sociocog_att)
datscores$baseline_sociocog_behControl_centered <- datscores$baseline_sociocog_behControl-mean(datscores$baseline_sociocog_behControl)
datscores$baseline_sociocog_intStrength_centered <- datscores$baseline_sociocog_intStrength-mean(datscores$baseline_sociocog_intStrength)
datscores$baseline_sociocog_subjnorm_centered <- datscores$baseline_sociocog_subjnorm-mean(datscores$baseline_sociocog_subjnorm)
datscores$baseline_selfdet_amot_centered <- datscores$baseline_selfdet_amot-mean(datscores$baseline_selfdet_amot) 
datscores$baseline_selfdet_extReg_centered <- datscores$baseline_selfdet_extReg-mean(datscores$baseline_selfdet_extReg)
datscores$baseline_selfdet_idReg_centered <- datscores$baseline_selfdet_idReg-mean(datscores$baseline_selfdet_idReg)
datscores$baseline_selfdet_intMot_centered <- datscores$baseline_selfdet_intMot-mean(datscores$baseline_selfdet_intMot)
datscores$baseline_selfdet_intro_centered <- datscores$baseline_selfdet_intro-mean(datscores$baseline_selfdet_intro)

# Prior distributions
modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma"))

#### Attitude at baseline ####

# Model fitting
modexp_att <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_att_centered,
                  data = datscores, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes") 

# Parameter estimation
summary(modexp_att)

# Hypothesis testing
H_att1 <- hypothesis(modexp_att, "treatmentimpInt:baseline_sociocog_att_centered = 0")
H_att2 <- hypothesis(modexp_att, "treatmentmentCont:baseline_sociocog_att_centered = 0")
H_att3 <- hypothesis(modexp_att, "treatmentcombiTreat:baseline_sociocog_att_centered  = 0")

# Effect size
bayes_R2(modexp_att)

#### Strength of intention at baseline ####

# Model fitting
modexp_intStrength <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_intStrength_centered,
                          data = datscores, 
                          family = "gaussian",
                          prior = modelpriors,
                          sample_prior = "yes") 

# Parameter estimation
summary(modexp_intStrength)

# Hypothesis testing
H_intStrength1 <- hypothesis(modexp_intStrength, "treatmentimpInt:baseline_sociocog_intStrength_centered = 0")
H_intStrength2 <- hypothesis(modexp_intStrength, "treatmentmentCont:baseline_sociocog_intStrength_centered = 0")
H_intStrength3 <- hypothesis(modexp_intStrength, "treatmentcombiTreat:baseline_sociocog_intStrength_centered  = 0")

# Effect size
bayes_R2(modexp_intStrength)

#### Subjective norms at baseline ####

# Model fitting
modexp_subjnorm <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_subjnorm_centered,
                       data = datscores, 
                       family = "gaussian",
                       prior = modelpriors,
                       sample_prior = "yes") 

# Parameter estimation
summary(modexp_subjnorm)

# Hypothesis testing
H_subjnorm1 <- hypothesis(modexp_subjnorm, "treatmentimpInt:baseline_sociocog_subjnorm_centered = 0")
H_subjnorm2 <- hypothesis(modexp_subjnorm, "treatmentmentCont:baseline_sociocog_subjnorm_centered = 0")
H_subjnorm3 <- hypothesis(modexp_subjnorm, "treatmentcombiTreat:baseline_sociocog_subjnorm_centered  = 0")

# Effect size
bayes_R2(modexp_subjnorm)

#### Behavioral control at baseline ####

# Model fitting
modexp_behControl <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_behControl_centered,
                         data = datscores, 
                         family = "gaussian",
                         prior = modelpriors,
                         sample_prior = "yes") 

# Parameter estimation
summary(modexp_behControl)

# Hypothesis testing
H_behControl1 <- hypothesis(modexp_behControl, "treatmentimpInt:baseline_sociocog_behControl_centered = 0")
H_behControl2 <- hypothesis(modexp_behControl, "treatmentmentCont:baseline_sociocog_behControl_centered = 0")
H_behControl3 <- hypothesis(modexp_behControl, "treatmentcombiTreat:baseline_sociocog_behControl_centered  = 0")

# Effect size
bayes_R2(modexp_behControl)

#### Amotivation at baseline ####

# Model fitting
modexp_amot <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_selfdet_amot_centered,
                   data = datscores, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes") 

# Parameter estimation
summary(modexp_amot)

# Hypothesis testing
H_amot1 <- hypothesis(modexp_amot, "treatmentimpInt:baseline_selfdet_amot_centered = 0")
H_amot2 <- hypothesis(modexp_amot, "treatmentmentCont:baseline_selfdet_amot_centered = 0")
H_amot3 <- hypothesis(modexp_amot, "treatmentcombiTreat:baseline_selfdet_amot_centered  = 0")

# Effect size
bayes_R2(modexp_amot)

#### External Regulation at baseline ####

# Model fitting
modexp_extReg <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_selfdet_extReg_centered,
                     data = datscores, 
                     family = "gaussian",
                     prior = modelpriors,
                     sample_prior = "yes") 

# Parameter estimation
summary(modexp_extReg)

# Hypothesis testing
H_extReg1 <- hypothesis(modexp_extReg, "treatmentimpInt:baseline_selfdet_extReg_centered = 0")
H_extReg2 <- hypothesis(modexp_extReg, "treatmentmentCont:baseline_selfdet_extReg_centered = 0")
H_extReg3 <- hypothesis(modexp_extReg, "treatmentcombiTreat:baseline_selfdet_extReg_centered  = 0")

# Effect size
bayes_R2(modexp_extReg)

#### Identified Regulation at baseline ####

# Model fitting
modexp_idReg <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_selfdet_idReg_centered,
                    data = datscores, 
                    family = "gaussian",
                    prior = modelpriors,
                    sample_prior = "yes") 

# Parameter estimation
summary(modexp_idReg)

# Hypothesis testing
H_idReg1 <- hypothesis(modexp_idReg, "treatmentimpInt:baseline_selfdet_idReg_centered = 0")
H_idReg2 <- hypothesis(modexp_idReg, "treatmentmentCont:baseline_selfdet_idReg_centered = 0")
H_idReg3 <- hypothesis(modexp_idReg, "treatmentcombiTreat:baseline_selfdet_idReg_centered  = 0")

# Effect size
bayes_R2(modexp_idReg)

#### Integrated Regulation ####

# Model fitting
modexp_intReg <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_selfdet_intReg_centered,
                    data = datscores, 
                    family = "gaussian",
                    prior = modelpriors,
                    sample_prior = "yes") 

# Parameter estimation
summary(modexp_intReg)

# Hypothesis testing
H_intReg1 <- hypothesis(modexp_idReg, "treatmentimpInt:baseline_selfdet_intReg_centered = 0")
H_intReg2 <- hypothesis(modexp_idReg, "treatmentmentCont:baseline_selfdet_intReg_centered = 0")
H_intReg3 <- hypothesis(modexp_idReg, "treatmentcombiTreat:baseline_selfdet_intReg_centered  = 0")

# Effect size
bayes_R2(modexp_intReg)

#### Intrinsic Motivation at baseline ####

# Model fitting
modexp_intMot <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_selfdet_intMot_centered,
                     data = datscores, 
                     family = "gaussian",
                     prior = modelpriors,
                     sample_prior = "yes") 

# Parameter estimation
summary(modexp_intMot)

# Hypothesis testing
H_intMot1 <- hypothesis(modexp_intMot, "treatmentimpInt:baseline_selfdet_intMot_centered = 0")
H_intMot2 <- hypothesis(modexp_intMot, "treatmentmentCont:baseline_selfdet_intMot_centered = 0")
H_intMot3 <- hypothesis(modexp_intMot, "treatmentcombiTreat:baseline_selfdet_intMot_centered  = 0")

# Effect size
bayes_R2(modexp_intMot)

#### Introjection at baseline ####

# Model fitting
modexp_intro <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_selfdet_intro_centered,
                    data = datscores, 
                    family = "gaussian",
                    prior = modelpriors,
                    sample_prior = "yes") 

# Parameter estimation
summary(modexp_intro)

# Hypothesis testing
H_intro1 <- hypothesis(modexp_intro, "treatmentimpInt:baseline_selfdet_intro_centered = 0")
H_intro2 <- hypothesis(modexp_intro, "treatmentmentCont:baseline_selfdet_intro_centered = 0")
H_intro3 <- hypothesis(modexp_intro, "treatmentcombiTreat:baseline_selfdet_intro_centered  = 0")

# Effect size
bayes_R2(modexp_intro)

################# Exploratory Mediation Hypotheses #############################

# Use default prior distributions in brms
rm(modelpriors)

#### treatment -> affective commitment -> physical activity ####

f1 <- bf(post_affcommitment ~ baseline_physAct_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + treatment + post_affcommitment)
med1 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med1)

# Effect size
bayes_R2(med1)

#### treatment -> motivational commitment -> physical activity ####

f1 <- bf(post_commitment ~ baseline_physAct_centered + baseline_sociocog_intStrength_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + baseline_sociocog_intStrength_centered + treatment + post_commitment)
med2 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med2)

# Effect size
bayes_R2(med2)

#### treatment -> energization -> physical activity ####

f1 <- bf(post_energization ~ baseline_physAct_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + treatment + post_energization)
med3 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med3)

# Effect size
bayes_R2(med3)

#### treatment -> experienced automaticity -> physical activity ####

f1 <- bf(follow_automaticity ~ baseline_physAct_centered + baseline_automaticity_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + baseline_automaticity_centered + treatment + follow_automaticity)
med4 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med4)

# Effect size
bayes_R2(med4)

############## Robustness Checks: Discrete Regression Models ###################

#### Hypothesis 1: Physical Activity ####

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2.3)", class = "b"),
                 set_prior("normal(2.6, 2.7)", class = "Intercept", lb=0))

# Model fitting
mod1_r <- brm(follow_physAct ~ baseline_physAct + treatment,
              data = datscores, 
              family = "poisson",
              prior = modelpriors,
              sample_prior = "yes",
              save_pars = save_pars(all = TRUE))

# Parameter estimation
summary(mod1_r)

# Model comparison to initial analysis
bayes_factor(mod1, mod1_r)

# Effect size
bayes_R2(mod1_r)

#### Hypothesis 2: Automaticity ####

# Data preparation
autoData <- cbind(preregDat[, 2:5], preregDat[, 53:56], treatment=preregDat$treatment, ID_person=seq(1, nrow(preregDat)))
autoData_long <-  reshape2::melt(autoData, id.vars = c("ID_person", "treatment"))
autoData_long$time <- as.numeric(grepl("follow", autoData_long$variable))
colnames(autoData_long)[4] <- c("automaticity")
autoData_long$treatment <- factor(autoData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
autoData_long$item <- parse_number(as.character(autoData_long$variable))

# Model fitting
mod2_r <- brm(automaticity ~ 1 + time*treatment + (1 | ID_person) + (1 | item),
              data = autoData_long, 
              family = cumulative("probit"),
              sample_prior = "yes",
              save_pars = save_pars(all = TRUE))

# Parameter estimation
summary(mod2_r)

# Model comparison to initial analysis
bayes_factor(mod2, mod2_r)

# Effect size
bayes_R2(mod2_r)

#### Hypothesis 3-1: Motivational Goal Commitment ####

# Data preparation
motcomData <- cbind(preregDat[, 33:35], preregDat[, 42:44], treatment=preregDat$treatment, ID_person=seq(1, nrow(preregDat)))
motcomData_long <-  reshape2::melt(motcomData, id.vars = c("ID_person", "treatment"))
motcomData_long$time <- as.numeric(grepl("follow", motcomData_long$variable))
colnames(motcomData_long)[4] <- c("motCommitment")
motcomData_long$treatment <- factor(motcomData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
motcomData_long$item <- parse_number(as.character(motcomData_long$variable))

# Model fitting
mod3_1_r <- brm(motCommitment ~ 1 + time*treatment + (1 | ID_person) + (1 | item),
                data = motcomData_long, 
                family = cumulative("probit"),
                sample_prior = "yes",
                save_pars = save_pars(all = TRUE))

# Parameter estimation
summary(mod3_1_r)

# Model comparison to initial analysis
bayes_factor(mod3_1, mod2_r)

# Effect size
bayes_R2(mod3_1_r)

#### Hypothesis 3-2: Affective Goal Commitment ####

affcomData <- cbind(preregDat[, 45:47], treatment=preregDat$treatment, ID_person=seq(1, nrow(preregDat)))
affcomData_long <-  reshape2::melt(affcomData, id.vars = c("ID_person", "treatment"))
colnames(affcomData_long)[4] <- c("affCommitment")
affcomData_long$treatment <- factor(affcomData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
affcomData_long$item <- parse_number(as.character(affcomData_long$variable))

# Model fitting
mod3_2_r <- brm(affCommitment ~ 1 + treatment + (1 | ID_person) + (1 | item),
                data = affcomData_long,
                family = cumulative("probit"),
                sample_prior = "yes",
                save_pars = save_pars(all = TRUE))

# Parameter estimation
summary(mod3_2_r)

# Model comparison to initial analysis
bayes_factor(mod3_2, mod3_2_r)

# Effect size
bayes_R2(mod3_2_r)

#### Hypothesis 4: Energization ####

energyData <- cbind(preregDat[, 37:41], treatment=preregDat$treatment, ID_person=seq(1, nrow(preregDat)))
energyData_long <-  reshape2::melt(energyData, id.vars = c("ID_person", "treatment"))
colnames(energyData_long)[4] <- c("energization")
energyData_long$treatment <- factor(energyData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
energyData_long$item <- parse_number(as.character(energyData_long$variable))

# Model fitting
mod4_r <- brm(energization ~ 1 + treatment + (1 | ID_person) + (1 | item),
              data = energyData_long,
              family = cumulative("probit"),
              sample_prior = "yes",
              save_pars = save_pars(all = TRUE))

# Parameter estimation
summary(mod4_r)

# Model comparison to initial analysis
bayes_factor(mod4, mod4_r)

# Effect size
bayes_R2(mod4_r)
