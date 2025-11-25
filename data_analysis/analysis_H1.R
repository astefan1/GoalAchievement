# ==============================================================================
# HYPOTHESIS 1
# ==============================================================================

rm(list = ls())

###########################  Load packages #####################################

library(brms)

############################# Load data ########################################

GAData <- read.csv("GAData_preprocessed.csv")
GAData_ITT <- GAData[GAData$NoDataInPhase2 == FALSE, ] 
GAData_PerProtocol <- GAData_ITT[GAData_ITT$PerProtocolAnalyses == "Yes",]

########################### Intention To Treat Analyses ########################

# Data preparation
GAData_ITT$baseline_physAct_centered <- GAData_ITT$baseline_physAct-mean(GAData_ITT$baseline_physAct)
GAData_ITT$treatment <- factor(GAData_ITT$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Prior distributions
modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma", lb=0))
# Model fitting
hyp1_1_ITT <- brm(follow_physAct ~ baseline_physAct_centered + treatment, 
                  data = GAData_ITT, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp1_1_ITT")


hyp1_2_ITT <- brm(follow_physAct ~ baseline_physAct_centered * treatment, 
                  data = GAData_ITT, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp1_2_ITT")

bayes_factor(hyp1_1_ITT, hyp1_2_ITT)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp1_1_ITT)

# Hypothesis testing
H1a <- hypothesis(hyp1_1_ITT, "treatmentcombiTreat - treatmentmentCont = 0")
H1a

H1b <- hypothesis(hyp1_1_ITT, "treatmentcombiTreat - treatmentimpInt = 0")
H1b
1/H1b$hypothesis$Evid.Ratio

H1c <- hypothesis(hyp1_1_ITT, "treatmentcombiTreat = 0")
H1c

H1d <- hypothesis(hyp1_1_ITT, "treatmentimpInt - treatmentmentCont = 0")
H1d

H1e <- hypothesis(hyp1_1_ITT, "treatmentmentCont = 0")
H1e

H1f <- hypothesis(hyp1_1_ITT, "treatmentimpInt = 0")
H1f
1/H1f$hypothesis$Evid.Ratio

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp1_1_ITT)

############################## Per-Protocol Analyses ###########################

# Data preparation
GAData_PerProtocol$baseline_physAct_centered <- GAData_PerProtocol$baseline_physAct-mean(GAData_PerProtocol$baseline_physAct)
GAData_PerProtocol$treatment <- factor(GAData_PerProtocol$treatment, levels = c("control", "impInt", "mentCont", "combiTreat"))

# Prior distributions
modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma", lb=0))
# Model fitting
hyp1_1_PPA  <- brm(follow_physAct ~ baseline_physAct_centered + treatment,
                   data = GAData_PerProtocol, 
                   family = "gaussian",
                   prior = modelpriors,
                   sample_prior = "yes",
                   iter = 12000,
                   chains = 5,
                   warmup = 2000,
                   save_pars = save_pars(all = TRUE),
                   file = "../model_fits/hyp1_1_PPA")
            
hyp1_2_PPA <- brm(follow_physAct ~ baseline_physAct_centered * treatment,
                  data = GAData_PerProtocol, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp1_2_PPA")
              
bayes_factor(hyp1_1_PPA, hyp1_2_PPA)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp1_1_PPA)

# Hypothesis testing
H1a <- hypothesis(hyp1_1_PPA, "treatmentcombiTreat - treatmentmentCont = 0")
H1a

H1b <- hypothesis(hyp1_1_PPA, "treatmentcombiTreat - treatmentimpInt = 0")
H1b
1/H1b$hypothesis$Evid.Ratio

H1c <- hypothesis(hyp1_1_PPA, "treatmentcombiTreat = 0")
H1c

H1d <- hypothesis(hyp1_1_PPA, "treatmentimpInt - treatmentmentCont = 0")
H1d

H1e <- hypothesis(hyp1_1_PPA, "treatmentmentCont = 0")
H1e

H1f <- hypothesis(hyp1_1_PPA, "treatmentimpInt = 0")
H1f
1/H1f$hypothesis$Evid.Ratio

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp1_1_PPA)

################### Exploratory analyses: Intention To Treat ###################

#### Model comparison to a model without treatment effect ####

hyp1_0_ITT <- brm(follow_physAct ~ baseline_physAct_centered,
                  data = GAData_ITT, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp1_0_ITT")
            
bayes_factor(hyp1_0_ITT, hyp1_1_ITT)

#### Robustness analyses with unplanned exclusions ####

# Repeat analysis without people who have a GLTEQ score > 190

GAData_ITT_ExclGLTEQ <- GAData_ITT[GAData_ITT$follow_physAct < 190, ]

# Prior distributions
modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma", lb=0))
# Model fitting
hyp1_1_ITT_ExclGLTEQ <- brm(follow_physAct ~ baseline_physAct_centered + treatment, 
                            data = GAData_ITT_ExclGLTEQ, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_1_ITT_ExclGLTEQ")

hyp1_2_ITT_ExclGLTEQ <- brm(follow_physAct ~ baseline_physAct_centered * treatment, 
                            data = GAData_ITT_ExclGLTEQ, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_2_ITT_ExclGLTEQ")

bayes_factor(hyp1_1_ITT_ExclGLTEQ, hyp1_2_ITT_ExclGLTEQ)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp1_1_ITT_ExclGLTEQ)

# Hypothesis testing
H1a <- hypothesis(hyp1_1_ITT_ExclGLTEQ, "treatmentcombiTreat - treatmentmentCont = 0")
H1a

H1b <- hypothesis(hyp1_1_ITT_ExclGLTEQ, "treatmentcombiTreat - treatmentimpInt = 0")
H1b

H1c <- hypothesis(hyp1_1_ITT_ExclGLTEQ, "treatmentcombiTreat = 0")
H1c

H1d <- hypothesis(hyp1_1_ITT_ExclGLTEQ, "treatmentimpInt - treatmentmentCont = 0")
H1d

H1e <- hypothesis(hyp1_1_ITT_ExclGLTEQ, "treatmentmentCont = 0")
H1e

H1f <- hypothesis(hyp1_1_ITT_ExclGLTEQ, "treatmentimpInt = 0")
H1f

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp1_1_ITT_ExclGLTEQ)

# Omnibus test: Null model without treatment effect vs treatment model
hyp1_0_ITT_ExclGLTEQ <- brm(follow_physAct ~ baseline_physAct_centered, 
                            data = GAData_ITT_ExclGLTEQ, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_0_ITT_ExclGLTEQ")

bayes_factor(hyp1_0_ITT_ExclGLTEQ, hyp1_1_ITT_ExclGLTEQ)

# Repeat analysis without people who have more than 40h/week at follow-up 

hoursPerWeek <- rowSums(GAData_ITT[, c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild")])/2
GAData_ITT[which(hoursPerWeek > 40), c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild", "follow_physAct", "SocioProfessionalCat")]

GAData_ITT_ExclHours <- GAData_ITT[hoursPerWeek <= 40, ]

# Prior distributions
modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma", lb=0))
# Model fitting
hyp1_1_ITT_ExclHours <- brm(follow_physAct ~ baseline_physAct_centered + treatment, 
                            data = GAData_ITT_ExclHours, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_1_ITT_ExclHours")

hyp1_2_ITT_ExclHours <- brm(follow_physAct ~ baseline_physAct_centered * treatment, 
                            data = GAData_ITT_ExclHours, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_2_ITT_ExclHours")

bayes_factor(hyp1_1_ITT_ExclHours, hyp1_2_ITT_ExclHours)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp1_1_ITT_ExclHours)

# Hypothesis testing
H1a <- hypothesis(hyp1_1_ITT_ExclHours, "treatmentcombiTreat - treatmentmentCont = 0")
H1a

H1b <- hypothesis(hyp1_1_ITT_ExclHours, "treatmentcombiTreat - treatmentimpInt = 0")
H1b

H1c <- hypothesis(hyp1_1_ITT_ExclHours, "treatmentcombiTreat = 0")
H1c

H1d <- hypothesis(hyp1_1_ITT_ExclHours, "treatmentimpInt - treatmentmentCont = 0")
H1d

H1e <- hypothesis(hyp1_1_ITT_ExclHours, "treatmentmentCont = 0")
H1e

H1f <- hypothesis(hyp1_1_ITT_ExclHours, "treatmentimpInt = 0")
H1f

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp1_1_ITT_ExclHours)

# Omnibus test: Null model without treatment effect vs treatment model
hyp1_0_ITT_ExclHours <- brm(follow_physAct ~ baseline_physAct_centered, 
                            data = GAData_ITT_ExclHours, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_0_ITT_ExclHours")

bayes_factor(hyp1_0_ITT_ExclHours, hyp1_1_ITT_ExclHours)

#### Robustness analysis with Poisson model ####

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2.3)", class = "b"),
                 set_prior("normal(2.6, 2.7)", class = "Intercept", lb=0))
GAData_ITT$follow_physActInteger <- round(GAData_ITT$follow_physAct)

# Model fitting
hyp1_1_ITT_Poisson <- brm(follow_physActInteger ~ baseline_physAct + treatment,
                          data = GAData_ITT, 
                          family = "poisson",
                          prior = modelpriors,
                          sample_prior = "yes",
                          iter = 12000,
                          chains = 5,
                          warmup = 2000,
                          save_pars = save_pars(all = TRUE),
                          file = "../model_fits/hyp1_1_ITT_Poisson")
              
# Parameter estimation
summary(hyp1_1_ITT_Poisson)

# Model comparison to initial analysis
bayes_factor(hyp1_1_ITT, hyp1_1_ITT_Poisson)

# Effect size
bayes_R2(hyp1_1_ITT_Poisson)
  
################### Exploratory analyses: Per Protocol #########################

#### Model comparison to a model without treatment effect ####

hyp1_0_PPA <- brm(follow_physAct ~ baseline_physAct_centered,
                  data = GAData_PerProtocol, 
                  family = "gaussian",
                  prior = modelpriors,
                  sample_prior = "yes",
                  iter = 12000,
                  chains = 5,
                  warmup = 2000,
                  save_pars = save_pars(all = TRUE),
                  file = "../model_fits/hyp1_0_PPA")

bayes_factor(hyp1_0_PPA, hyp1_1_PPA)

#### Robustness analysis with Poisson model ####

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2.3)", class = "b"),
                 set_prior("normal(2.6, 2.7)", class = "Intercept", lb=0))
GAData_PerProtocol$follow_physActInteger <- round(GAData_PerProtocol$follow_physAct)

# Model fitting
hyp1_1_PPA_Poisson <- brm(follow_physActInteger ~ baseline_physAct + treatment,
                          data = GAData_PerProtocol, 
                          family = "poisson",
                          prior = modelpriors,
                          sample_prior = "yes",
                          iter = 12000,
                          chains = 5,
                          warmup = 2000,
                          save_pars = save_pars(all = TRUE),
                          file = "../model_fits/hyp1_1_PPA_Poisson")

# Parameter estimation
summary(hyp1_1_PPA_Poisson)

# Model comparison to initial analysis
bayes_factor(hyp1_1_PPA, hyp1_1_PPA_Poisson)

# Effect size
bayes_R2(hyp1_1_PPA_Poisson)

#### Robustness analyses with unplanned exclusions ####

# Repeat analysis without people who have a GLTEQ score > 190

GAData_PPA_ExclGLTEQ <- GAData_PerProtocol[GAData_PerProtocol$follow_physAct < 190, ]

# Prior distributions
modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma", lb=0))
# Model fitting
hyp1_1_PPA_ExclGLTEQ <- brm(follow_physAct ~ baseline_physAct_centered + treatment, 
                            data = GAData_PPA_ExclGLTEQ, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_1_PPA_ExclGLTEQ")

hyp1_2_PPA_ExclGLTEQ <- brm(follow_physAct ~ baseline_physAct_centered * treatment, 
                            data = GAData_PPA_ExclGLTEQ, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_2_PPA_ExclGLTEQ")

bayes_factor(hyp1_1_PPA_ExclGLTEQ, hyp1_2_PPA_ExclGLTEQ)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp1_1_PPA_ExclGLTEQ)

# Hypothesis testing
H1a <- hypothesis(hyp1_1_PPA_ExclGLTEQ, "treatmentcombiTreat - treatmentmentCont = 0")
H1a

H1b <- hypothesis(hyp1_1_PPA_ExclGLTEQ, "treatmentcombiTreat - treatmentimpInt = 0")
H1b

H1c <- hypothesis(hyp1_1_PPA_ExclGLTEQ, "treatmentcombiTreat = 0")
H1c

H1d <- hypothesis(hyp1_1_PPA_ExclGLTEQ, "treatmentimpInt - treatmentmentCont = 0")
H1d

H1e <- hypothesis(hyp1_1_PPA_ExclGLTEQ, "treatmentmentCont = 0")
H1e

H1f <- hypothesis(hyp1_1_PPA_ExclGLTEQ, "treatmentimpInt = 0")
H1f

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp1_1_PPA_ExclGLTEQ)

# Omnibus test: Null model without treatment effect vs treatment model
hyp1_0_PPA_ExclGLTEQ <- brm(follow_physAct ~ baseline_physAct_centered, 
                            data = GAData_PPA_ExclGLTEQ, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_0_PPA_ExclGLTEQ")

bayes_factor(hyp1_0_PPA_ExclGLTEQ, hyp1_1_PPA_ExclGLTEQ)

# Repeat analysis without people who have more than 40h/week at follow-up 

hoursPerWeek <- rowSums(GAData_PerProtocol[, c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild")])/2
GAData_PerProtocol[which(hoursPerWeek > 40), c("follow_physActStrenuous", "follow_physActModerate", "follow_physActMild", "follow_physAct", "SocioProfessionalCat")]

GAData_PPA_ExclHours <- GAData_PerProtocol[hoursPerWeek <= 40, ]

# Prior distributions
modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma", lb=0))
# Model fitting
hyp1_1_PPA_ExclHours <- brm(follow_physAct ~ baseline_physAct_centered + treatment, 
                            data = GAData_PPA_ExclHours, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_1_PPA_ExclHours")

hyp1_2_PPA_ExclHours <- brm(follow_physAct ~ baseline_physAct_centered * treatment, 
                            data = GAData_PPA_ExclHours, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_2_PPA_ExclHours")

bayes_factor(hyp1_1_PPA_ExclHours, hyp1_2_PPA_ExclHours)

# The Bayes factor shows extreme evidence in favor of the ANCOVA model (without
# interactions). Therefore, further results will be interpreted under this model.

# Parameter estimation
summary(hyp1_1_PPA_ExclHours)

# Hypothesis testing
H1a <- hypothesis(hyp1_1_PPA_ExclHours, "treatmentcombiTreat - treatmentmentCont = 0")
H1a

H1b <- hypothesis(hyp1_1_PPA_ExclHours, "treatmentcombiTreat - treatmentimpInt = 0")
H1b

H1c <- hypothesis(hyp1_1_PPA_ExclHours, "treatmentcombiTreat = 0")
H1c

H1d <- hypothesis(hyp1_1_PPA_ExclHours, "treatmentimpInt - treatmentmentCont = 0")
H1d

H1e <- hypothesis(hyp1_1_PPA_ExclHours, "treatmentmentCont = 0")
H1e

H1f <- hypothesis(hyp1_1_PPA_ExclHours, "treatmentimpInt = 0")
H1f

# Posterior probability if BF > 6
# Not computed as no BF was > 6

# Effect size
bayes_R2(hyp1_1_PPA_ExclHours)

# Omnibus test: Null model without treatment effect vs treatment model
hyp1_0_PPA_ExclHours <- brm(follow_physAct ~ baseline_physAct_centered, 
                            data = GAData_PPA_ExclHours, 
                            family = "gaussian",
                            prior = modelpriors,
                            sample_prior = "yes",
                            iter = 12000,
                            chains = 5,
                            warmup = 2000,
                            save_pars = save_pars(all = TRUE),
                            file = "../model_fits/hyp1_0_PPA_ExclHours")

bayes_factor(hyp1_0_PPA_ExclHours, hyp1_1_PPA_ExclHours)
