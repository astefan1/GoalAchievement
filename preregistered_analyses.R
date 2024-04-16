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
datscores$baseline_sociocog_commitDirect <- rowMeans(preregDat[, 33:35], na.rm = TRUE)
datscores$treatment <- preregDat[, 36]
datscores$post_commitDirect <- rowMeans(preregDat[, 37:39], na.rm = TRUE)
datscores$post_commitIndirect <- rowMeans(preregDat[, 40:42], na.rm = TRUE)
datscores$gender <- preregDat[, 43]
datscores$age <- preregDat[,44]
datscores$follow_physAct <- preregDat[,47]
datscores$follow_automaticity <- rowMeans(preregDat[, 48:51], na.rm = TRUE)

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
omega_baseline_sociocog_commitDirect <- strel(preregDat[, 33:35], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_post_commitDirect <- strel(preregDat[, 37:39], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_post_commitIndirect <- strel(preregDat[, 40:42], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_follow_automaticity <- strel(preregDat[, 48:51], estimates = "omega", Bayes=TRUE, freq = FALSE)

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

#### H3_1: Goal Commitment - Direct Measure ####

# Data preparation
datscores$baseline_sociocog_commitDirect_centered <- datscores$baseline_sociocog_commitDirect - mean(datscores$baseline_sociocog_commitDirect)

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma"))

# Model fitting
mod3_1 <- brm(post_commitDirect ~ baseline_sociocog_commitDirect_centered + treatment,
              data = datscores, 
              family = "gaussian",
              prior = modelpriors,
              sample_prior = "yes",
              save_pars = save_pars(all = TRUE))

mod3_1_2 <- brm(post_commitDirect ~ baseline_sociocog_commitDirect_centered * treatment,
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

#### H3_2: Goal Commitment - Indirect Measure ####

# Model fitting
mod3_2 <- brm(post_commitIndirect ~ treatment,
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

################# Exploratory Moderation Hypotheses ############################

# Data preparation
datscores$baseline_sociocog_att_centered <- datscores$baseline_sociocog_att-mean(datscores$baseline_sociocog_att)
datscores$baseline_sociocog_behControl_centered <- datscores$baseline_sociocog_behControl-mean(datscores$baseline_sociocog_behControl)
datscores$baseline_sociocog_commitDirect_centered <- datscores$baseline_sociocog_commitDirect-mean(datscores$baseline_sociocog_commitDirect)
datscores$baseline_sociocog_subjnorm_centered <- datscores$baseline_sociocog_subjnorm-mean(datscores$baseline_sociocog_subjnorm)

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
modexp_commitDirect <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_commitDirect_centered,
                          data = datscores, 
                          family = "gaussian",
                          prior = modelpriors,
                          sample_prior = "yes") 

# Parameter estimation
summary(modexp_commitDirect)

# Hypothesis testing
H_commitDirect1 <- hypothesis(modexp_commitDirect, "treatmentimpInt:baseline_sociocog_commitDirect_centered = 0")
H_commitDirect2 <- hypothesis(modexp_commitDirect, "treatmentmentCont:baseline_sociocog_commitDirect_centered = 0")
H_commitDirect3 <- hypothesis(modexp_commitDirect, "treatmentcombiTreat:baseline_sociocog_commitDirect_centered  = 0")

# Effect size
bayes_R2(modexp_commitDirect)

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

################# Exploratory Mediation Hypotheses #############################

# Use default prior distributions in brms
rm(modelpriors)

#### treatment -> affective commitment -> physical activity ####

f1 <- bf(post_commitIndirect ~ baseline_physAct_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + treatment + post_commitIndirect)
med1 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med1)

# Effect size
bayes_R2(med1)

#### treatment -> commitment (direct measure) -> physical activity ####

f1 <- bf(post_commitDirect ~ baseline_physAct_centered + baseline_sociocog_commitDirect_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + baseline_sociocog_commitDirect_centered + treatment + post_commitDirect)
med2 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med2)

# Effect size
bayes_R2(med2)

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

#### Hypothesis 3-1: Goal Commitment - Direct Measure ####

# Data preparation
dirComData <- cbind(preregDat[, 33:35], preregDat[, 42:44], treatment=preregDat$treatment, ID_person=seq(1, nrow(preregDat)))
dirComData_long <-  reshape2::melt(dirComData, id.vars = c("ID_person", "treatment"))
dirComData_long$time <- as.numeric(grepl("follow", dirComData_long$variable))
colnames(dirComData_long)[4] <- c("dirCommitment")
dirComData_long$treatment <- factor(dirComData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
dirComData_long$item <- parse_number(as.character(dirComData_long$variable))

# Model fitting
mod3_1_r <- brm(dirCommitment ~ 1 + time*treatment + (1 | ID_person) + (1 | item),
                data = dirComData_long, 
                family = cumulative("probit"),
                sample_prior = "yes",
                save_pars = save_pars(all = TRUE))

# Parameter estimation
summary(mod3_1_r)

# Model comparison to initial analysis
bayes_factor(mod3_1, mod2_r)

# Effect size
bayes_R2(mod3_1_r)

#### Hypothesis 3-2: Goal Commitment - Indirect Measure ####

indComData <- cbind(preregDat[, 45:47], treatment=preregDat$treatment, ID_person=seq(1, nrow(preregDat)))
indComData_long <-  reshape2::melt(indComData, id.vars = c("ID_person", "treatment"))
colnames(IndComData_long)[4] <- c("commitIndirect")
indComData_long$treatment <- factor(indComData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
indComData_long$item <- parse_number(as.character(indComData_long$variable))

# Model fitting
mod3_2_r <- brm(commitIndirect ~ 1 + treatment + (1 | ID_person) + (1 | item),
                data = indComData_long,
                family = cumulative("probit"),
                sample_prior = "yes",
                save_pars = save_pars(all = TRUE))

# Parameter estimation
summary(mod3_2_r)

# Model comparison to initial analysis
bayes_factor(mod3_2, mod3_2_r)

# Effect size
bayes_R2(mod3_2_r)

