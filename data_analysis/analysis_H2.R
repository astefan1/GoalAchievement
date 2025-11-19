# ==============================================================================
# HYPOTHESIS 2
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(brms)           # Bayesian regression
library(readr)          # parse character vectors
library(bayesplot)      # plot posterior distributions

############################# Load data ########################################

GAData <- read.csv("GAData_preprocessed.csv")
GAData_ITT <- GAData[GAData$NoDataInPhase2 == FALSE, ] 
GAData_PerProtocol <- GAData_ITT[GAData_ITT$PerProtocolAnalyses == "Yes",]

########################### Intention To Treat Analyses ########################

# Data preparation
GAData_ITT$baseline_automaticity_centered <- GAData_ITT$baseline_automaticity - mean(GAData_ITT$baseline_automaticity)
GAData_ITT$treatment <- factor(GAData_ITT$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Prior distributions
modelpriors <- c(set_prior("normal(0, 1.5)", class = "b"),
                 set_prior("normal(2.3, 1)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.7)", class = "sigma", lb=0))

# Model fitting
hyp2_1_ITT <- brm(follow_automaticity ~ baseline_automaticity_centered + treatment,
                  data = GAData_ITT, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp2_1_ITT")

hyp2_2_ITT <- brm(follow_automaticity ~ baseline_automaticity_centered * treatment,
                  data = GAData_ITT, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp2_2_ITT")

bayes_factor(hyp2_1_ITT, hyp2_2_ITT)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp2_1_ITT)

# Hypothesis testing
H2a <- hypothesis(hyp2_1_ITT, "treatmentcombiTreat - treatmentmentCont = 0")
H2a

H2b <- hypothesis(hyp2_1_ITT, "treatmentcombiTreat - treatmentimpInt = 0")
H2b

H2c <- hypothesis(hyp2_1_ITT, "treatmentcombiTreat = 0")
H2c

H2d <- hypothesis(hyp2_1_ITT, "treatmentimpInt - treatmentmentCont = 0")
H2d

H2e <- hypothesis(hyp2_1_ITT, "treatmentmentCont = 0")
H2e

H2f <- hypothesis(hyp2_1_ITT, "treatmentimpInt = 0")
H2f

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp2_1_ITT)

########################### Per-Protocol Analyses ##############################

# Data preparation
GAData_PerProtocol$baseline_automaticity_centered <- GAData_PerProtocol$baseline_automaticity - mean(GAData_PerProtocol$baseline_automaticity)
GAData_PerProtocol$treatment <- factor(GAData_PerProtocol$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Prior distributions
modelpriors <- c(set_prior("normal(0, 1.5)", class = "b"),
                 set_prior("normal(2.3, 1)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.7)", class = "sigma", lb=0))

# Model fitting
hyp2_1_PPA <- brm(follow_automaticity ~ baseline_automaticity_centered + treatment,
                  data = GAData_PerProtocol, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp2_1_PPA")

hyp2_2_PPA <- brm(follow_automaticity ~ baseline_automaticity_centered * treatment,
                  data = GAData_PerProtocol, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp2_2_PPA")

bayes_factor(hyp2_1_PPA, hyp2_2_PPA)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp2_1_PPA)

# Hypothesis testing
H2a <- hypothesis(hyp2_1_PPA, "treatmentcombiTreat - treatmentmentCont = 0")
H2a

H2b <- hypothesis(hyp2_1_PPA, "treatmentcombiTreat - treatmentimpInt = 0")
H2b

H2c <- hypothesis(hyp2_1_PPA, "treatmentcombiTreat = 0")
H2c

H2d <- hypothesis(hyp2_1_PPA, "treatmentimpInt - treatmentmentCont = 0")
H2d

H2e <- hypothesis(hyp2_1_PPA, "treatmentmentCont = 0")
H2e

H2f <- hypothesis(hyp2_1_PPA, "treatmentimpInt = 0")
H2f

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp2_1_PPA)


################### Exploratory analyses: Intention To Treat ###################

#### Model comparison to a model without treatment effect ####

hyp2_0_ITT <- brm(follow_automaticity ~ baseline_automaticity_centered,
                  data = GAData_ITT, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp2_0_ITT")

bayes_factor(hyp2_0_ITT, hyp2_1_ITT)

#### Ordinal Regression model ####

# Data preparation
autoData <- GAData_ITT[, c("ID", "treatment", "baseline_automaticity1", "baseline_automaticity2", "baseline_automaticity3", "baseline_automaticity4", "follow_automaticity1", "follow_automaticity2", "follow_automaticity3", "follow_automaticity4")]
autoData_long <-  reshape2::melt(autoData, id.vars = c("ID", "treatment"))
autoData_long$time <- as.numeric(grepl("follow", autoData_long$variable))
colnames(autoData_long)[4] <- c("automaticity")
autoData_long$treatment <- factor(autoData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
autoData_long$item <- parse_number(as.character(autoData_long$variable))

# Model fitting
hyp2_1_ITT_Ordinal <- brm(automaticity ~ 1 + time*treatment + (1 | ID) + (1 | item),
                          data = autoData_long, 
                          family = cumulative("probit"),
                          iter = 12000,
                          chains = 5,
                          warmup = 2000,
                          save_pars = save_pars(all = TRUE),
                          file = "../model_fits/hyp2_1_ITT_Ordinal")
              
# Parameter estimation
summary(hyp2_1_ITT_Ordinal)

posterior_draws <- brms::as_draws_matrix(hyp2_1_ITT_Ordinal)[,c("b_Intercept[1]", "b_Intercept[2]", "b_Intercept[3]", "b_Intercept[4]", "b_Intercept[5]", "b_Intercept[6]", "b_time", "b_treatmentimpInt", "b_treatmentmentCont", "b_treatmentcombiTreat", "b_time:treatmentimpInt", "b_time:treatmentmentCont", "b_time:treatmentcombiTreat")]
bayesplot::mcmc_areas(posterior_draws)

################### Exploratory analyses: Per Protocol #########################

#### Model comparison to a model without treatment effect ####

hyp2_0_PPA <- brm(follow_automaticity ~ baseline_automaticity_centered,
                  data = GAData_PerProtocol, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp2_0_PPA")

bayes_factor(hyp2_0_PPA, hyp2_1_PPA)

#### Discrete Regression model ####

# Data preparation
autoData <- GAData_PerProtocol[, c("ID", "treatment", "baseline_automaticity1", "baseline_automaticity2", "baseline_automaticity3", "baseline_automaticity4", "follow_automaticity1", "follow_automaticity2", "follow_automaticity3", "follow_automaticity4")]
autoData_long <-  reshape2::melt(autoData, id.vars = c("ID", "treatment"))
autoData_long$time <- as.numeric(grepl("follow", autoData_long$variable))
colnames(autoData_long)[4] <- c("automaticity")
autoData_long$treatment <- factor(autoData_long$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))
autoData_long$item <- parse_number(as.character(autoData_long$variable))

# Model fitting
hyp2_1_PPA_Ordinal <- brm(automaticity ~ 1 + time*treatment + (1 | ID) + (1 | item),
                          data = autoData_long, 
                          family = cumulative("probit"),
                          sample_prior = "yes",
                          iter = 12000,
                          chains = 5,
                          warmup = 2000,
                          save_pars = save_pars(all = TRUE),
                          control = list(adapt_delta = 0.95, stepsize = 0.2, max_treedepth = 15),
                          file = "../model_fits/hyp2_1_PPA_Ordinal")

# Parameter estimation
summary(hyp2_1_PPA_Ordinal)

posterior_draws <- brms::as_draws_matrix(hyp2_1_PPA_Ordinal)[,c("b_Intercept[1]", "b_Intercept[2]", "b_Intercept[3]", "b_Intercept[4]", "b_Intercept[5]", "b_Intercept[6]", "b_time", "b_treatmentimpInt", "b_treatmentmentCont", "b_treatmentcombiTreat", "b_time:treatmentimpInt", "b_time:treatmentmentCont", "b_time:treatmentcombiTreat")]
bayesplot::mcmc_areas(posterior_draws)
