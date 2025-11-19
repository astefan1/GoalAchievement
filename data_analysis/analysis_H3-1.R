# ==============================================================================
# HYPOTHESIS 3-1 (Goal Commitment - direct measure)
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(brms)           # Bayesian regression
library(readr)          # parse character vectors

############################# Load data ########################################

GAData <- read.csv("GAData_preprocessed.csv")
GAData_ITT <- GAData # no exclusions based on follow-up bc comparison is in Stage 1
GAData_PPA <- GAData_ITT[GAData_ITT$PerProtocolAnalyses == "Yes",]

########################### Intention To Treat Analyses ########################

# Data preparation
GAData_ITT$baseline_commitDirect_centered <- GAData_ITT$baseline_commitDirect - mean(GAData_ITT$baseline_commitDirect)
GAData_ITT$treatment <- factor(GAData_ITT$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma", lb=0))

# Model fitting
hyp31_1_ITT <- brm(post_commitDirect ~ baseline_commitDirect_centered + treatment,
                   data = GAData_ITT, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-1_1_ITT")
               
hyp31_2_ITT <- brm(post_commitDirect ~ baseline_commitDirect_centered * treatment,
                   data = GAData_ITT, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-1_2_ITT") 

bayes_factor(hyp31_1_ITT, hyp31_2_ITT)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp31_1_ITT)

# Hypothesis testing
H31a <- hypothesis(hyp31_1_ITT, "treatmentcombiTreat - treatmentmentCont = 0")
H31a
H31b <- hypothesis(hyp31_1_ITT, "treatmentcombiTreat - treatmentimpInt = 0")
H31b
H31c <- hypothesis(hyp31_1_ITT, "treatmentcombiTreat = 0")
H31c
H31d <- hypothesis(hyp31_1_ITT, "treatmentimpInt - treatmentmentCont = 0")
H31d
H31e <- hypothesis(hyp31_1_ITT, "treatmentmentCont = 0")
H31e
H31f <- hypothesis(hyp31_1_ITT, "treatmentimpInt = 0")
H31f

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp31_1_ITT)

########################### Per Protocol Analyses ##############################

# Data preparation
GAData_PPA$baseline_commitDirect_centered <- GAData_PPA$baseline_commitDirect - mean(GAData_PPA$baseline_commitDirect)
GAData_PPA$treatment <- factor(GAData_PPA$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma", lb=0))

# Model fitting
hyp31_1_PPA <- brm(post_commitDirect ~ baseline_commitDirect_centered + treatment,
                   data = GAData_PPA, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-1_1_PPA")

hyp31_2_PPA <- brm(post_commitDirect ~ baseline_commitDirect_centered * treatment,
                   data = GAData_PPA, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-1_2_PPA") 

bayes_factor(hyp31_1_PPA, hyp31_2_PPA)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp31_1_PPA)

# Hypothesis testing
H31a <- hypothesis(hyp31_1_PPA, "treatmentcombiTreat - treatmentmentCont = 0")
H31a
H31b <- hypothesis(hyp31_1_PPA, "treatmentcombiTreat - treatmentimpInt = 0")
H31b
H31c <- hypothesis(hyp31_1_PPA, "treatmentcombiTreat = 0")
H31c
H31d <- hypothesis(hyp31_1_PPA, "treatmentimpInt - treatmentmentCont = 0")
H31d
H31e <- hypothesis(hyp31_1_PPA, "treatmentmentCont = 0")
H31e
H31f <- hypothesis(hyp31_1_PPA, "treatmentimpInt = 0")
H31f

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp31_1_PPA)

################### Exploratory analyses: Intention To Treat ###################

#### Model comparison to a model without treatment effect ####

hyp31_0_ITT <- brm(post_commitDirect ~ baseline_commitDirect_centered,
                   data = GAData_ITT, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-1_0_ITT")

bayes_factor(hyp31_0_ITT, hyp31_1_ITT)

#### Ordinal Regression model ####

# Data preparation
commitData <- GAData_ITT[, c("ID", "treatment", "baseline_commitDirect1", "baseline_commitDirect2", "baseline_commitDirect3", "post_commitDirect1", "post_commitDirect2", "post_commitDirect3")]
commitData_long <-  reshape2::melt(commitData, id.vars = c("ID", "treatment"))
commitData_long$time <- as.numeric(grepl("post", commitData_long$variable))
colnames(commitData_long)[4] <- c("commitmentDirect")
commitData_long$treatment <- factor(commitData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
commitData_long$item <- parse_number(as.character(commitData_long$variable))

# Model fitting
hyp31_1_ITT_Ordinal <- brm(commitmentDirect ~ 1 + time*treatment + (1 | ID) + (1 | item),
                           data = commitData_long, 
                           family = cumulative("probit"),
                           sample_prior = "yes",
                           iter = 12000,
                           chains = 5,
                           warmup = 2000,
                           save_pars = save_pars(all = TRUE),
                           file = "../model_fits/hyp31_1_ITT_Ordinal")
                
# Parameter estimation
summary(hyp31_1_ITT_Ordinal)

posterior_draws <- brms::as_draws_matrix(hyp31_1_ITT_Ordinal)[,c("b_Intercept[1]", "b_Intercept[2]", "b_Intercept[3]", "b_Intercept[4]", "b_Intercept[5]", "b_Intercept[6]", "b_time", "b_treatmentimpInt", "b_treatmentmentCont", "b_treatmentcombiTreat", "b_time:treatmentimpInt", "b_time:treatmentmentCont", "b_time:treatmentcombiTreat")]
bayesplot::mcmc_areas(posterior_draws)

# Model comparison to initial analysis
bayes_factor(hyp31_1_ITT, hyp31_1_ITT_Ordinal)

# Effect size
bayes_R2(hyp31_1_ITT_Ordinal)

################### Exploratory analyses: Per Protocol #########################

#### Model comparison to a model without treatment effect ####

hyp31_0_PPA <- brm(post_commitDirect ~ baseline_commitDirect_centered,
                   data = GAData_PPA, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-1_0_PPA")

bayes_factor(hyp31_0_PPA, hyp31_1_PPA)

#### Ordinal Regression model ####

# Data preparation
commitData <- GAData_PPA[, c("ID", "treatment", "baseline_commitDirect1", "baseline_commitDirect2", "baseline_commitDirect3", "post_commitDirect1", "post_commitDirect2", "post_commitDirect3")]
commitData_long <-  reshape2::melt(commitData, id.vars = c("ID", "treatment"))
commitData_long$time <- as.numeric(grepl("post", commitData_long$variable))
colnames(commitData_long)[4] <- c("commitmentDirect")
commitData_long$treatment <- factor(commitData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
commitData_long$item <- parse_number(as.character(commitData_long$variable))

# Model fitting
hyp31_1_PPA_Ordinal <- brm(commitmentDirect ~ 1 + time*treatment + (1 | ID) + (1 | item),
                           data = commitData_long, 
                           family = cumulative("probit"),
                           sample_prior = "yes",
                           iter = 12000,
                           chains = 5,
                           warmup = 2000,
                           save_pars = save_pars(all = TRUE),
                           control = list(adapt_delta = 0.95, stepsize = 0.2, max_treedepth = 15),
                           file = "../model_fits/hyp31_1_PPA_Ordinal")

# Parameter estimation
summary(hyp31_1_PPA_Ordinal)

posterior_draws <- brms::as_draws_matrix(hyp31_1_PPA_Ordinal)[,c("b_Intercept[1]", "b_Intercept[2]", "b_Intercept[3]", "b_Intercept[4]", "b_Intercept[5]", "b_Intercept[6]", "b_time", "b_treatmentimpInt", "b_treatmentmentCont", "b_treatmentcombiTreat", "b_time:treatmentimpInt", "b_time:treatmentmentCont", "b_time:treatmentcombiTreat")]
bayesplot::mcmc_areas(posterior_draws)

# Model comparison to initial analysis
bayes_factor(hyp31_1_PPA, hyp31_1_PPA_Ordinal)

# Effect size
bayes_R2(hyp31_1_PPA_Ordinal)

