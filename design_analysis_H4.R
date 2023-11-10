# ==============================================================================
# DESIGN ANALYSIS FOR ANCOVA MODEL (HYPOTHESIS 4)
# ==============================================================================

#### Load functions & packages ####
library(brms)
library(MASS)
library(xtable)
source("generateData_ANOVA.R")
source("conductDA_ANOVA.R")
source("analyzeDA_H3H4.R")

############################### H0, N = 500 ####################################

set.seed(1234)
DAT_H0_N500_H4 <- simANOVA(iter = 1000,
                           N = 500, 
                           meanControl = 3.5, 
                           sdWithinGroup = 1, 
                           effimpInt = 0,
                           effmentCont = 0,
                           effCombi = 0)

DA_H0_N500_H4 <- DA_ANOVA(simData = DAT_H0_N500_H4,
                          priorCoef = "normal(0, 2)", 
                          priorIntercept = "normal(4, 1.5)", 
                          priorSigma = "student_t(3, 0, 0.5)")

save(DA_H0_N500_H4, file = "./generated_data/DA_H0_N500_H4.RData")

res_H0_H4_N500 <- analyze_DA_Effects_H3H4(DA_H0_N500_H4)
save(res_H0_H4_N500, file = "./generated_data/res_H0_H4_N500.RData")

BFtable_H3H4(res_H0_H4_N500, whichhyp = "H4")
postprobcat_H3H4(res_H0_H4_N500, whichhyp = "H4")

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H0_N500_H4, 
                     trueIntercept=3.5,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0,
                     trueSigma=1,
                     xlimIntercept=c(2.5,4.5), 
                     xlimBeta=c(-1, 1),
                     xlimSigma=c(0, 2))
mtext("H0, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

############################### H0, N = 600 ####################################

set.seed(1234)
DAT_H0_N600_H4 <- simANOVA(iter = 1000,
                           N = 600, 
                           meanControl = 3.5, 
                           sdWithinGroup = 1, 
                           effimpInt = 0,
                           effmentCont = 0,
                           effCombi = 0)

DA_H0_N600_H4 <- DA_ANOVA(simData = DAT_H0_N600_H4,
                          priorCoef = "normal(0, 2)", 
                          priorIntercept = "normal(4, 1.5)", 
                          priorSigma = "student_t(3, 0, 0.5)")

save(DA_H0_N600_H4, file = "./generated_data/DA_H0_N600_H4.RData")

res_H0_H4_N600 <- analyze_DA_Effects_H3H4(DA_H0_N600_H4)
save(res_H0_H4_N600, file = "./generated_data/res_H0_H4_N600.RData")

BFtable_H3H4(res_H0_H4_N600, whichhyp = "H4")
postprobcat_H3H4(res_H0_H4_N600, whichhyp = "H4")

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H0_N600_H4, 
                     trueIntercept=3.5,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0,
                     trueSigma=1,
                     xlimIntercept=c(2.5,4.5), 
                     xlimBeta=c(-1, 1),
                     xlimSigma=c(0, 2))
mtext("H0, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

############################### H1, N = 500 ####################################

set.seed(1234)
DAT_H1_N500_H4 <- simANOVA(iter = 1000,
                           N = 500, 
                           meanControl = 3.5, 
                           sdWithinGroup = 1,  
                           effimpInt = 0,
                           effmentCont = 0.35,
                           effCombi = 0.35)

DA_H1_N500_H4 <- DA_ANOVA(simData = DAT_H1_N500_H4,
                          priorCoef = "normal(0, 2)", 
                          priorIntercept = "normal(4, 1.5)", 
                          priorSigma = "student_t(3, 0, 0.5)")

save(DA_H1_N500_H4, file = "./generated_data/DA_H1_N500_H4.RData")

res_H1_H4_N500 <- analyze_DA_Effects_H3H4(DA_H1_N500_H4)
save(res_H1_H4_N500, file = "./generated_data/res_H1_H4_N500.RData")

BFtable_H3H4(res_H1_H4_N500, whichhyp = "H4")
postprobcat_H3H4(res_H1_H4_N500, whichhyp = "H4")

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H1_N500_H4, 
                     trueIntercept=3.5,
                     trueImpIntEffect=0,
                     trueMentContEffect=0.35,
                     trueCombiEffect=0.35,
                     trueSigma=1,
                     xlimIntercept=c(2.5,4.5), 
                     xlimBeta=c(-1, 1),
                     xlimSigma=c(0, 2))
mtext("H1, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

############################### H1, N = 600 ####################################

set.seed(1234)
DAT_H1_N600_H4 <- simANOVA(iter = 1000,
                           N = 600, 
                           meanControl = 3.5, 
                           sdWithinGroup = 1,  
                           effimpInt = 0,
                           effmentCont = 0.35,
                           effCombi = 0.35)

DA_H1_N600_H4 <- DA_ANOVA(simData = DAT_H1_N600_H4,
                          priorCoef = "normal(0, 2)", 
                          priorIntercept = "normal(4, 1.5)", 
                          priorSigma = "student_t(3, 0, 0.5)")

save(DA_H1_N600_H4, file = "./generated_data/DA_H1_N600_H4.RData")

res_H1_H4_N600 <- analyze_DA_Effects_H3H4(DA_H1_N600_H4)
save(res_H1_H4_N600, file = "./generated_data/res_H1_H4_N600.RData")

BFtable_H3H4(res_H1_H4_N600, whichhyp = "H4")
postprobcat_H3H4(res_H1_H4_N600, whichhyp = "H4")

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H1_N600_H4, 
                     trueIntercept=3.5,
                     trueImpIntEffect=0,
                     trueMentContEffect=0.35,
                     trueCombiEffect=0.35,
                     trueSigma=1,
                     xlimIntercept=c(2.5,4.5), 
                     xlimBeta=c(-1, 1),
                     xlimSigma=c(0, 2))
mtext("H1, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)
