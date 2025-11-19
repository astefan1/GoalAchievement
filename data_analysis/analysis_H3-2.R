# ==============================================================================
# HYPOTHESIS 3-2 (Goal Commitment - indirect measure)
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
GAData_ITT$baseline_commitIndirect_centered <- GAData_ITT$baseline_commitIndirect - mean(GAData_ITT$baseline_commitIndirect)
GAData_ITT$treatment <- factor(GAData_ITT$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma", lb=0))

# Model fitting
hyp32_1_ITT <- brm(post_commitIndirect ~ baseline_commitIndirect_centered + treatment,
                   data = GAData_ITT, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-2_1_ITT")

hyp32_2_ITT <- brm(post_commitIndirect ~ baseline_commitIndirect_centered * treatment,
                   data = GAData_ITT, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-2_2_ITT") 

bayes_factor(hyp32_1_ITT, hyp32_2_ITT)

# The Bayes factor shows strong evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp32_1_ITT)

# Hypothesis testing
H32a <- hypothesis(hyp32_1_ITT, "treatmentcombiTreat - treatmentmentCont = 0")
H32a
H32b <- hypothesis(hyp32_1_ITT, "treatmentcombiTreat - treatmentimpInt = 0")
H32b
H32c <- hypothesis(hyp32_1_ITT, "treatmentcombiTreat = 0")
H32c
1/H32c$hypothesis$Evid.Ratio

H32d <- hypothesis(hyp32_1_ITT, "treatmentimpInt - treatmentmentCont = 0")
H32d
H32e <- hypothesis(hyp32_1_ITT, "treatmentmentCont = 0")
H32e
H32f <- hypothesis(hyp32_1_ITT, "treatmentimpInt = 0")
H32f

# Posterior probability if BF > 6
H32c_postprob <- hypothesis(hyp32_1_ITT, "treatmentcombiTreat > 0")
H32c_postprob$hypothesis$Post.Prob

# Effect size
bayes_R2(hyp32_1_ITT)

# Obtaining standardized coefficient estimates
GAData_ITT$post_commitIndirectZ <- scale(GAData_ITT$post_commitIndirect)
GAData_ITT$baseline_commitIndirectZ <- scale(GAData_ITT$baseline_commitIndirect)


hyp32_1_ITT_Z <- brm(post_commitIndirectZ ~ baseline_commitIndirectZ + treatment,
                     data = GAData_ITT, 
                     family = "gaussian",
                     sample_prior = "yes",
                     iter = 12000,
                     chains = 5,
                     warmup = 2000,
                     save_pars = save_pars(all = TRUE),
                     file = "../model_fits/hyp3-2_1_ITT_Z") # Analysis conducted with z standardized predictor and criterion because weakly informative priors were specified on unstandardized model coefficients

summary(hyp32_1_ITT_Z)
hypothesis(hyp32_1_ITT_Z, "treatmentcombiTreat = 0")

########################### Per Protocol Analyses ##############################

# Data preparation
GAData_PPA$baseline_commitIndirect_centered <- GAData_PPA$baseline_commitIndirect - mean(GAData_PPA$baseline_commitIndirect)
GAData_PPA$treatment <- factor(GAData_PPA$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma", lb=0))

# Model fitting
hyp32_1_PPA <- brm(post_commitIndirect ~ baseline_commitIndirect_centered + treatment,
                   data = GAData_PPA, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-2_1_PPA")

hyp32_2_PPA <- brm(post_commitIndirect ~ baseline_commitIndirect_centered * treatment,
                   data = GAData_PPA, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-2_2_PPA") 

bayes_factor(hyp32_1_PPA, hyp32_2_PPA)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp32_1_PPA)

# Hypothesis testing
H32a <- hypothesis(hyp32_1_PPA, "treatmentcombiTreat - treatmentmentCont = 0")
H32a
H32b <- hypothesis(hyp32_1_PPA, "treatmentcombiTreat - treatmentimpInt = 0")
H32b
H32c <- hypothesis(hyp32_1_PPA, "treatmentcombiTreat = 0")
H32c
1/H32c$hypothesis$Evid.Ratio

H32d <- hypothesis(hyp32_1_PPA, "treatmentimpInt - treatmentmentCont = 0")
H32d
H32e <- hypothesis(hyp32_1_PPA, "treatmentmentCont = 0")
H32e
H32f <- hypothesis(hyp32_1_PPA, "treatmentimpInt = 0")
H32f

# Posterior probability if BF > 6
H32c_postprob <- hypothesis(hyp32_1_PPA, "treatmentcombiTreat > 0")
H32c_postprob$hypothesis$Post.Prob

# Effect size
bayes_R2(hyp32_1_PPA)

# Obtaining standardized coefficient estimates
GAData_PPA$post_commitIndirectZ <- scale(GAData_PPA$post_commitIndirect)
GAData_PPA$baseline_commitIndirectZ <- scale(GAData_PPA$baseline_commitIndirect)

hyp32_1_PPA_Z <- brm(post_commitIndirectZ ~ baseline_commitIndirectZ + treatment,
                     data = GAData_PPA, 
                     family = "gaussian",
                     sample_prior = "yes",
                     iter = 12000,
                     chains = 5,
                     warmup = 2000,
                     save_pars = save_pars(all = TRUE),
                     file = "../model_fits/hyp3-2_1_PPA_Z") # Analysis conducted with z standardized predictor and criterion because weakly informative priors were specified on unstandardized model coefficients

summary(hyp32_1_PPA_Z)
hypothesis(hyp32_1_PPA_Z, "treatmentcombiTreat = 0")


################### Exploratory analyses: Intention To Treat ###################

#### Model comparison to a model without treatment effect ####

hyp32_0_ITT <- brm(post_commitIndirect ~ baseline_commitIndirect_centered,
                   data = GAData_ITT, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-2_0_ITT")

bayes_factor(hyp32_0_ITT, hyp32_1_ITT)

#### Ordinal Regression model ####

# Data preparation
commitData <- GAData_ITT[, c("ID", "treatment", "baseline_commitIndirect1", "baseline_commitIndirect2", "baseline_commitIndirect3", "baseline_commitIndirect4", "post_commitIndirect1", "post_commitIndirect2", "post_commitIndirect3", "post_commitIndirect4")]
commitData_long <-  reshape2::melt(commitData, id.vars = c("ID", "treatment"))
commitData_long$time <- as.numeric(grepl("post", commitData_long$variable))
colnames(commitData_long)[4] <- c("commitmentIndirect")
commitData_long$treatment <- factor(commitData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
commitData_long$item <- parse_number(as.character(commitData_long$variable))

# Model fitting
hyp32_1_ITT_Ordinal <- brm(commitmentIndirect ~ 1 + time*treatment + (1 | ID) + (1 | item),
                           data = commitData_long, 
                           family = cumulative("probit"),
                           sample_prior = "yes",
                           iter = 12000,
                           chains = 5,
                           warmup = 2000,
                           save_pars = save_pars(all = TRUE),
                           file = "../model_fits/hyp32_1_ITT_Ordinal")

# Parameter estimation
summary(hyp32_1_ITT_Ordinal)

posterior_draws <- brms::as_draws_matrix(hyp32_1_ITT_Ordinal)[,c("b_Intercept[1]", "b_Intercept[2]", "b_Intercept[3]", "b_Intercept[4]", "b_Intercept[5]", "b_Intercept[6]", "b_time", "b_treatmentimpInt", "b_treatmentmentCont", "b_treatmentcombiTreat", "b_time:treatmentimpInt", "b_time:treatmentmentCont", "b_time:treatmentcombiTreat")]
bayesplot::mcmc_areas(posterior_draws)

################### Exploratory analyses: Per Protocol #########################

#### Model comparison to a model without treatment effect ####

hyp32_0_PPA <- brm(post_commitIndirect ~ baseline_commitIndirect_centered,
                   data = GAData_PPA, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp3-2_0_PPA")

bayes_factor(hyp32_0_PPA, hyp32_1_PPA)

#### Ordinal Regression model ####

# Data preparation
commitData <- GAData_PPA[, c("ID", "treatment", "baseline_commitIndirect1", "baseline_commitIndirect2", "baseline_commitIndirect3", "baseline_commitIndirect4", "post_commitIndirect1", "post_commitIndirect2", "post_commitIndirect3", "post_commitIndirect4")]
commitData_long <-  reshape2::melt(commitData, id.vars = c("ID", "treatment"))
commitData_long$time <- as.numeric(grepl("post", commitData_long$variable))
colnames(commitData_long)[4] <- c("commitmentIndirect")
commitData_long$treatment <- factor(commitData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
commitData_long$item <- parse_number(as.character(commitData_long$variable))

# Model fitting
hyp32_1_PPA_Ordinal <- brm(commitmentIndirect ~ 1 + time*treatment + (1 | ID) + (1 | item),
                           data = commitData_long, 
                           family = cumulative("probit"),
                           sample_prior = "yes",
                           iter = 12000,
                           chains = 5,
                           warmup = 2000,
                           save_pars = save_pars(all = TRUE),
                           control = list(adapt_delta = 0.95, stepsize = 0.2, max_treedepth = 15),
                           file = "../model_fits/hyp32_1_PPA_Ordinal")

# Parameter estimation
summary(hyp32_1_PPA_Ordinal)

posterior_draws <- brms::as_draws_matrix(hyp32_1_PPA_Ordinal)[,c("b_Intercept[1]", "b_Intercept[2]", "b_Intercept[3]", "b_Intercept[4]", "b_Intercept[5]", "b_Intercept[6]", "b_time", "b_treatmentimpInt", "b_treatmentmentCont", "b_treatmentcombiTreat", "b_time:treatmentimpInt", "b_time:treatmentmentCont", "b_time:treatmentcombiTreat")]
bayesplot::mcmc_areas(posterior_draws)
