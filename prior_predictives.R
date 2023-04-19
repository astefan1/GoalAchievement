# ==============================================================================
# PRIORS AND PRIOR PREDICTIVE DISTRIBUTIONS
# ==============================================================================

library(brms)
library(MASS)
source("generateData_ANCOVA.R")
source("generateData_ANOVA.R")

#### H1: Plot Prior Distributions ####

par(mfrow=c(3,1))
curve(dnorm(x, mean=13.4, sd=15), xlim=c(-10, 50), xlab="Intercept", ylab="", main="Prior Distributions", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)
curve(dnorm(x, mean=0, sd=10), xlim=c(-50, 50), xlab="Regression Coefficients", ylab="", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)
curve(brms::dstudent_t(x, df=3, mu=0, sigma=7.5), xlim=c(0, 50), xlab="Standard Deviation", ylab="", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)

#### H1: Plot Prior Predictive Distributions ####

# Generate one dataset to obtain the right data structure
dat <- simANCOVA(iter=1, N = 350, meanControl = 13.4, sdWithinGroup = 7.2, effSingle = 0, effCombi = 0, retestReliability = 0.75)[[1]]
dat$physAct_Pre_centered <-  dat$physAct_Pre-mean(dat$physAct_Pre)

# Set model priors
modelpriors <- c(set_prior("normal(0, 10)", class = "b"),
                 set_prior("normal(13.4, 15)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 7.5)", class = "sigma"))

# Generate draws from prior distributions
priorSamples <- brm(physAct_Post ~ physAct_Pre_centered + treatment,
                    data = dat, 
                    family = "gaussian", 
                    prior = modelpriors,
                    sample_prior = "only",
                    verbose = FALSE)

newdata <- expand.grid(treatment = c("control", "impInt", "mentCont", "combiTreat"), physAct_Pre_centered = 0)
priorPred <- posterior_predict(priorSamples, newdata = newdata)

# Plot prior predictions for average pre-treatment score
priorPred_long <- reshape2::melt(priorPred)
priorPred_long$Var2 <- as.factor(priorPred_long$Var2)

ggplot(priorPred_long, aes(x = Var2, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha=0.03) +
  theme_classic() +
  geom_hline(yintercept = c(-25, -50, 0, 25, 50), colour = "grey", linetype = "dashed") +
  xlab("Treatment Group") +
  ylab("Physical Activity (Godin Score)") +
  scale_x_discrete(labels=c("Control", "Imp. Intentions", "M. Contrasting", "Combined")) +
  ylim(-100,100) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16))

#### H2: Plot Prior Distributions ####

par(mfrow=c(3,1))
curve(dnorm(x, mean=2.3, sd=1), xlim=c(-1, 10), xlab="Intercept", ylab="", main="Prior Distributions", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)
curve(dnorm(x, mean=0, sd=1.5), xlim=c(-5, 5), xlab="Regression Coefficients", ylab="", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)
curve(brms::dstudent_t(x, df=3, mu=0, sigma=0.7), xlim=c(0, 10), xlab="Standard Deviation", ylab="", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)


#### H2: Plot Prior Predictive Distributions ####

# Generate one dataset to obtain the right data structure
dat <- simANCOVA(iter=1, N = 350, meanControl = 2.3, sdWithinGroup = 1.2, effSingle = 0, effCombi = 0, retestReliability = 0.75)[[1]]
dat$physAct_Pre_centered <-  dat$physAct_Pre-mean(dat$physAct_Pre)

# Set model priors
modelpriors <- c(set_prior("normal(0, 1.5)", class = "b"),
                 set_prior("normal(2.3, 1)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.7)", class = "sigma"))

# Generate draws from prior distributions
priorSamples <- brm(physAct_Post ~ physAct_Pre_centered + treatment,
                    data = dat, 
                    family = "gaussian", 
                    prior = modelpriors,
                    sample_prior = "only",
                    verbose = FALSE)

newdata <- expand.grid(treatment = c("control", "impInt", "mentCont", "combiTreat"), physAct_Pre_centered = 0)
priorPred <- posterior_predict(priorSamples, newdata = newdata)

# Plot prior predictions for average pre-treatment score
priorPred_long <- reshape2::melt(priorPred)
priorPred_long$Var2 <- as.factor(priorPred_long$Var2)

ggplot(priorPred_long, aes(x = Var2, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha=0.03) +
  theme_classic() +
  geom_hline(yintercept = c(-2, 0, 2, 4, 6), colour = "grey", linetype = "dashed") +
  xlab("Treatment Group") +
  ylab("Physical Activity (Godin Score)") +
  scale_x_discrete(labels=c("Control", "Imp. Intentions", "M. Contrasting", "Combined")) +
  ylim(-3,7) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16))

#### H3: Prior Distributions ####

par(mfrow=c(3,1))
curve(dnorm(x, mean=4, sd=1.5), xlim=c(-1, 10), xlab="Intercept", ylab="", main="Prior Distributions", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)
curve(dnorm(x, mean=0, sd=2), xlim=c(-5, 5), xlab="Regression Coefficients", ylab="", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)
curve(brms::dstudent_t(x, df=3, mu=0, sigma=0.5), xlim=c(0, 10), xlab="Standard Deviation", ylab="", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)

#### H3: Plot Prior Predictive Distributions ####

# Generate one dataset to obtain the right data structure
dat <- simANOVA(iter=1, N = 350, meanControl = 2.3, sdWithinGroup = 1.2, effSingle = 0, effCombi = 0)[[1]]

# Set model priors
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma"))

# Generate draws from prior distributions
priorSamples <- brm(DV ~ treatment,
                    data = dat, 
                    family = "gaussian", 
                    prior = modelpriors,
                    sample_prior = "only",
                    verbose = FALSE)

newdata <- expand.grid(treatment = c("control", "impInt", "mentCont", "combiTreat"))
priorPred <- posterior_predict(priorSamples, newdata = newdata)

# Plot prior predictions for average pre-treatment score
priorPred_long <- reshape2::melt(priorPred)
priorPred_long$Var2 <- as.factor(priorPred_long$Var2)

ggplot(priorPred_long, aes(x = Var2, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha=0.03) +
  theme_classic() +
  geom_hline(yintercept = c(0, 2, 4, 6), colour = "grey", linetype = "dashed") +
  xlab("Treatment Group") +
  ylab("Physical Activity (Godin Score)") +
  scale_x_discrete(labels=c("Control", "Imp. Intentions", "M. Contrasting", "Combined")) +
  ylim(-3,12) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16))

#### H4: Prior Distributions ####

par(mfrow=c(3,1))
curve(dnorm(x, mean=4.3, sd=1.5), xlim=c(-1, 10), xlab="Intercept", ylab="", main="Prior Distributions", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)
curve(dnorm(x, mean=0, sd=2), xlim=c(-5, 5), xlab="Regression Coefficients", ylab="", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)
curve(brms::dstudent_t(x, df=3, mu=0, sigma=0.5), xlim=c(0, 10), xlab="Standard Deviation", ylab="", yaxt="n", cex.lab=2, cex.axis=2, cex.main=2)
mtext("Density", side=2, line = 2, cex=1.5)

#### H4: Plot Prior Predictive Distributions ####

# Generate one dataset to obtain the right data structure
dat <- simANOVA(iter=1, N = 350, meanControl = 4.3, sdWithinGroup = 1.5, effSingle = 0, effCombi = 0)[[1]]

# Set model priors
modelpriors <- c(set_prior("normal(0, 2)", class = "b"),
                 set_prior("normal(4.3, 1.5)", class = "Intercept", lb=0),
                 set_prior("student_t(3, 0, 0.5)", class = "sigma"))

# Generate draws from prior distributions
priorSamples <- brm(DV ~ treatment,
                    data = dat, 
                    family = "gaussian", 
                    prior = modelpriors,
                    sample_prior = "only",
                    verbose = FALSE)

newdata <- expand.grid(treatment = c("control", "impInt", "mentCont", "combiTreat"))
priorPred <- posterior_predict(priorSamples, newdata = newdata)

# Plot prior predictions for average pre-treatment score
priorPred_long <- reshape2::melt(priorPred)
priorPred_long$Var2 <- as.factor(priorPred_long$Var2)

ggplot(priorPred_long, aes(x = Var2, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha=0.03) +
  theme_classic() +
  geom_hline(yintercept = c(0, 2, 4, 6), colour = "grey", linetype = "dashed") +
  xlab("Treatment Group") +
  ylab("Physical Activity (Godin Score)") +
  scale_x_discrete(labels=c("Control", "Imp. Intentions", "M. Contrasting", "Combined")) +
  ylim(-3,12) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16))


