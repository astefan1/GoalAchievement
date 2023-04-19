# ==============================================================================
# ANALYZE THE RESULTS OF THE ANCOVA DESIGN ANALYSIS
# ==============================================================================

# Load design analysis objects
load("./generated_data/DA_H0_rr75.RData")
load("./generated_data/DA_H0_rr60.RData")
load("./generated_data/DA_H1_rr75.RData")
load("./generated_data/DA_H1_rr60.RData")

# Load functions and packages
source("analyzeDA_ANCOVA.R")

#### Parameter recovery ####

analyze_DA_postMeans(DA_H0_rr75, 
                     trueIntercept=13.4, 
                     trueRR = 0.75,
                     trueSingleEffect=0, 
                     trueCombiEffect=0, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(-3,3), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(3.5,6))

analyze_DA_postMeans(DA_H0_rr60, 
                     trueIntercept=13.4, 
                     trueRR = 0.6,
                     trueSingleEffect=0, 
                     trueCombiEffect=0, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(-3,3), 
                     xlimBeta1=c(0.4,0.8), 
                     xlimSigma=c(5,7))

analyze_DA_postMeans(DA_H1_rr75, 
                     trueIntercept=13.4, 
                     trueRR = 0.75,
                     trueSingleEffect=2.5, 
                     trueCombiEffect=5, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(0, 8), 
                     xlimBeta1=c(0.6,1), 
                     xlimSigma=c(3.5,6))

analyze_DA_postMeans(DA_H1_rr60, 
                     trueIntercept=13.4, 
                     trueRR = 0.6,
                     trueSingleEffect=2.5, 
                     trueCombiEffect=5, 
                     xlimIntercept=c(11,16), 
                     xlimBeta=c(0,8), 
                     xlimBeta1=c(0.4,0.8), 
                     xlimSigma=c(5,7))

#### Hypothesis evaluation ####

DAH0_hyp_rr75 <- analyze_DA_Effects(DA_H0_rr75)
DAH0_hyp_rr60 <- analyze_DA_Effects(DA_H0_rr60)
DAH1_hyp_rr75 <- analyze_DA_Effects(DA_H1_rr75)
DAH1_hyp_rr60 <- analyze_DA_Effects(DA_H1_rr60)

# Bayes factor distributions
dev.off()
# H0, Retest reliability = 0.6
plotBFdist(DAH0_hyp_rr60, hypothesis = "H1a")
plotBFdist(DAH0_hyp_rr60, hypothesis = "H1b")
plotBFdist(DAH0_hyp_rr60, hypothesis = "H1c")
plotBFdist(DAH0_hyp_rr60, hypothesis = "H1d")
plotBFdist(DAH0_hyp_rr60, hypothesis = "H1e")
plotBFdist(DAH0_hyp_rr60, hypothesis = "H1f")

# H0, Retest reliability = 0.75
plotBFdist(DAH0_hyp_rr75, hypothesis = "H1a")
plotBFdist(DAH0_hyp_rr75, hypothesis = "H1b")
plotBFdist(DAH0_hyp_rr75, hypothesis = "H1c")
plotBFdist(DAH0_hyp_rr75, hypothesis = "H1d")
plotBFdist(DAH0_hyp_rr75, hypothesis = "H1e")
plotBFdist(DAH0_hyp_rr75, hypothesis = "H1f")

# H1, Retest reliability = 0.6
plotBFdist(DAH1_hyp_rr60, hypothesis = "H1a")
plotBFdist(DAH1_hyp_rr60, hypothesis = "H1b")
plotBFdist(DAH1_hyp_rr60, hypothesis = "H1c")
plotBFdist(DAH1_hyp_rr60, hypothesis = "H1d")
plotBFdist(DAH1_hyp_rr60, hypothesis = "H1e")
plotBFdist(DAH1_hyp_rr60, hypothesis = "H1f")

# H1, Retest reliability = 0.75
plotBFdist(DAH1_hyp_rr75, hypothesis = "H1a")
plotBFdist(DAH1_hyp_rr75, hypothesis = "H1b")
plotBFdist(DAH1_hyp_rr75, hypothesis = "H1c")
plotBFdist(DAH1_hyp_rr75, hypothesis = "H1d")
plotBFdist(DAH1_hyp_rr75, hypothesis = "H1e")
plotBFdist(DAH1_hyp_rr75, hypothesis = "H1f")

# Conditional posterior probability for inequality
plotPostProb(DAH0_hyp_rr60)
plotPostProb(DAH0_hyp_rr75)
plotPostProb(DAH1_hyp_rr60)
plotPostProb(DAH1_hyp_rr75)


