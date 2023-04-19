# ==============================================================================
# Preregistered Analysis Plan
# ==============================================================================

# In the following, we will compute all analyses proposed in the Stage II
# report based on the simulated dataset. Drop-out rates are assumed to be 
# recorded outside the main dataset.

###########################  Load packages #####################################

library(multibridge)  # Bayesian multinomial test
library(Bayesrel)     # Bayesian reliability estimation
library(brms)         # Bayesian regression
library(bayestestR)   # Bayesian mediation analysis

############################# Load data ########################################

load("./generated_data/preregDat.RData")
set.seed(1234)
dropout <- sample(0:15, size=4, replace=FALSE)

###################### Drop-Out Rates Comparison ###############################

ss_per_group <- as.vector(table(preregDat$treatment))
mult_bf_equality(x = dropout, a = rep(1, 4))

############ Compute score values and add them to new dataset ##################

datscores <- data.frame(matrix(NA, nrow=350, ncol=0))
datscores$baseline_physAct <- preregDat[, 1]
datscores$baseline_automaticity <- rowMeans(preregDat[, 2:5])
datscores$baseline_selfdet_amot <- rowMeans(preregDat[, 6:8])
datscores$baseline_selfdet_extReg <- rowMeans(preregDat[, 9:11])
datscores$baseline_selfdet_intro <- rowMeans(preregDat[, 12:14])
datscores$baseline_selfdet_idReg <- rowMeans(preregDat[, 15:17])
datscores$baseline_selfdet_intReg <- rowMeans(preregDat[, 18:20])
datscores$baseline_selfdet_intMot <- rowMeans(preregDat[, 21:23])
datscores$baseline_sociocog_att <- rowMeans(preregDat[, 24:28])
datscores$baseline_sociocog_subjnorm <- preregDat[, 29]
datscores$baseline_sociocog_behControl <- rowMeans(preregDat[, 30:32])
datscores$baseline_sociocog_intStrength <- rowMeans(preregDat[, 33:35])
datscores$baseline_sociocog_incentValue <- preregDat[, 36]
datscores$baseline_sociocog_expSuccess <- preregDat[, 37]
datscores$treatment <- preregDat[, 38]
datscores$post_energization <- rowMeans(preregDat[, 39:43])
datscores$post_commitment <- rowMeans(preregDat[, 44:46])
datscores$post_affcommitment <- rowMeans(preregDat[, 47:49])
datscores$gender <- preregDat[, 50]
datscores$age <- preregDat[,51]
datscores$follow_physAct <- preregDat[,54]
datscores$follow_automaticity <- rowMeans(preregDat[, 55:58])
datscores$follow_selfdet_amot <- rowMeans(preregDat[, 59:61])
datscores$follow_selfdet_extReg <- rowMeans(preregDat[, 62:64])
datscores$follow_selfdet_intro <- rowMeans(preregDat[, 65:67])
datscores$follow_selfdet_idReg <- rowMeans(preregDat[, 68:70])
datscores$follow_selfdet_intReg <- rowMeans(preregDat[, 71:73])
datscores$follow_selfdet_intMot <- rowMeans(preregDat[, 74:76])

####### Compute McDonalds Omega for all scales consisting of multiple items ####

omega_baseline_automaticity <- strel(preregDat[, 2:5], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_amot <- strel(preregDat[, 6:8], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_extReg <- strel(preregDat[, 9:11], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_intro <- strel(preregDat[, 12:14], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_idReg <- strel(preregDat[, 15:17], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_intReg <- strel(preregDat[, 18:20], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_selfdet_intMot <- strel(preregDat[, 21:23], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_sociocog_att <- strel(preregDat[, 24:28], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_sociocog_behControl <- strel(preregDat[, 30:32], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_baseline_sociocog_intStrength <- strel(preregDat[, 33:35], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_post_energization <- strel(preregDat[, 39:43], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_post_commitment <- strel(preregDat[, 44:46], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_post_affcommitment <- strel(preregDat[, 47:49], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_follow_automaticity <- strel(preregDat[, 55:58], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_follow_selfdet_amot <- strel(preregDat[, 59:61], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_follow_selfdet_extReg <- strel(preregDat[, 62:64], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_follow_selfdet_intro <- strel(preregDat[, 65:67], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_follow_selfdet_idReg <- strel(preregDat[, 68:70], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_follow_selfdet_intReg <- strel(preregDat[, 71:73], estimates = "omega", Bayes=TRUE, freq = FALSE)
omega_follow_selfdet_intMot <- strel(preregDat[, 74:76], estimates = "omega", Bayes=TRUE, freq = FALSE)

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
            sample_prior = "yes")

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
if(H1d$hypothesis$Evid.Ratio < 1/6) H1e_postprob <- hypothesis(mod1, "treatmentmentCont > 0")
if(H1e$hypothesis$Evid.Ratio < 1/6) H1f_postprob <- hypothesis(mod1, "treatmentimpInt > 0")

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
            sample_prior = "yes")

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
if(H2b$hypothesis$Evid.Ratio < 1/6) H2b_postprob <- hypothesis(mod2, "treatmentcombiTreat - treatmentimpInt > 0")
if(H2c$hypothesis$Evid.Ratio < 1/6) H2c_postprob <- hypothesis(mod2, "treatmentcombiTreat > 0")
if(H2d$hypothesis$Evid.Ratio < 1/6) H2d_postprob <- hypothesis(mod2, "treatmentimpInt - treatmentmentCont > 0")
if(H2e$hypothesis$Evid.Ratio < 1/6) H2e_postprob <- hypothesis(mod2, "treatmentmentCont > 0")
if(H2f$hypothesis$Evid.Ratio < 1/6) H2f_postprob <- hypothesis(mod2, "treatmentimpInt > 0")

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
              sample_prior = "yes")
            
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

#### H3_2: Affective Goal Commitment ####

# Model fitting
mod3_2 <- brm(post_affcommitment ~ treatment,
              data = datscores, 
              family = "gaussian",
              prior = modelpriors,
              sample_prior = "yes")

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

############ H4: Hypothesis testing and parameter estimation ###################

# Prior distributions
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4.3, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma"))

# Model fitting
mod4 <- brm(post_energization ~ treatment, 
            data = datscores, 
            family = "gaussian",
            prior = modelpriors,
            sample_prior = "yes")

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

################# Exploratory Moderation Hypotheses ############################

# Data preparation
datscores$baseline_sociocog_att_centered <- datscores$baseline_sociocog_att-mean(datscores$baseline_sociocog_att)
datscores$baseline_sociocog_behControl_centered <- datscores$baseline_sociocog_behControl-mean(datscores$baseline_sociocog_behControl)
datscores$baseline_sociocog_expSuccess_centered <- datscores$baseline_sociocog_expSuccess-mean(datscores$baseline_sociocog_expSuccess)
datscores$baseline_sociocog_incentValue_centered <- datscores$baseline_sociocog_incentValue-mean(datscores$baseline_sociocog_incentValue)
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

#### Expected success at baseline ####

# Model fitting
modexp_expSuccess <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_expSuccess_centered,
                         data = datscores, 
                         family = "gaussian",
                         prior = modelpriors,
                         sample_prior = "yes") 

# Parameter estimation
summary(modexp_expSuccess)

# Hypothesis testing
H_expSuccess1 <- hypothesis(modexp_att, "treatmentimpInt:baseline_sociocog_expSuccess_centered = 0")
H_expSuccess2 <- hypothesis(modexp_att, "treatmentmentCont:baseline_sociocog_expSuccess_centered = 0")
H_expSuccess3 <- hypothesis(modexp_att, "treatmentcombiTreat:baseline_sociocog_expSuccess_centered  = 0")

#### Incentive value at baseline ####

# Model fitting
modexp_incentValue <- brm(follow_physAct ~ baseline_physAct_centered + treatment*baseline_sociocog_incentValue_centered,
                          data = datscores, 
                          family = "gaussian",
                          prior = modelpriors,
                          sample_prior = "yes") 

# Parameter estimation
summary(modexp_incentValue)

# Hypothesis testing
H_incentValue1 <- hypothesis(modexp_incentValue, "treatmentimpInt:baseline_sociocog_incentValue_centered = 0")
H_incentValue2 <- hypothesis(modexp_incentValue, "treatmentmentCont:baseline_sociocog_incentValue_centered = 0")
H_incentValue3 <- hypothesis(modexp_incentValue, "treatmentcombiTreat:baseline_sociocog_incentValue_centered  = 0")

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

################# Exploratory Mediation Hypotheses #############################

# Use default prior distributions in brms
rm(modelpriors)

#### treatment -> affective commitment -> physical activity ####

f1 <- bf(post_affcommitment ~ baseline_physAct_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + treatment + post_affcommitment)
med1 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med1)

#### treatment -> motivational commitment -> physical activity ####

f1 <- bf(post_commitment ~ baseline_physAct_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + treatment + post_commitment)
med2 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med2)

#### treatment -> energization -> physical activity ####

f1 <- bf(post_energization ~ baseline_physAct_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + treatment + post_energization)
med3 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med3)

#### treatment -> experienced automaticity -> physical activity ####

f1 <- bf(follow_automaticity ~ baseline_physAct_centered + treatment)
f2 <- bf(follow_physAct ~ baseline_physAct_centered + treatment + follow_automaticity)
med4 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med4)

#### treatment -> physical activity -> amotivation ####

f1 <- bf(follow_physAct ~ treatment)
f2 <- bf(follow_selfdet_amot ~ treatment + follow_physAct)
med5 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med5)

#### treatment -> physical activity -> external regulation ####

f1 <- bf(follow_physAct ~ treatment)
f2 <- bf(follow_selfdet_extReg ~ treatment + follow_physAct)
med6 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med6)

#### treatment -> physical activity -> introjection ####

f1 <- bf(follow_physAct ~ treatment)
f2 <- bf(follow_selfdet_intro ~ treatment + follow_physAct)
med7 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med7)

#### treatment -> physical activity -> identified regulation ####

f1 <- bf(follow_physAct ~ treatment)
f2 <- bf(follow_selfdet_idReg ~ treatment + follow_physAct)
med8 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med8)

#### treatment -> physical activity -> integrated regulation ####

f1 <- bf(follow_physAct ~ treatment)
f2 <- bf(follow_selfdet_intReg ~ treatment + follow_physAct)
med9 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med9)

#### treatment -> physical activity -> intrinsic motivation ####

f1 <- bf(follow_physAct ~ treatment)
f2 <- bf(follow_selfdet_intMot ~ treatment + follow_physAct)
med10 <- brm(f1+f2+set_rescor(FALSE), data = datscores)

mediation(med10)

