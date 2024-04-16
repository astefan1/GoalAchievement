# ==============================================================================
# DESIGN ANALYSIS FOR ANOVA MODEL (HYPOTHESIS 3_2)
# ==============================================================================

#### Load functions & packages ####
library(brms)
library(MASS)
library(xtable)
source("generateData_ANOVA.R")
source("conductDA_ANOVA.R")
source("analyzeDA_H3.R")

############################### H0, N = 500 ####################################

set.seed(1234)
DAT_H0_N500_H3 <- simANOVA(iter = 1000,
                           N = 500, 
                           meanControl = 4, 
                           sdWithinGroup = 1.5, 
                           effimpInt = 0,
                           effmentCont = 0,
                           effCombi = 0)
                                 
DA_H0_N500_H3 <- DA_ANOVA(simData = DAT_H0_N500_H3,
                          priorCoef = "normal(0, 2)", 
                          priorIntercept = "normal(4, 1.5)", 
                          priorSigma = "student_t(3, 0, 0.5)")

save(DA_H0_N500_H3, file = "./generated_data/DA_H0_N500_H3.RData")

res_H0_H3_N500 <- analyze_DA_Effects_H3(DA_H0_N500_H3)
save(res_H0_H3_N500, file = "./generated_data/res_H0_H3_N500.RData")

BFtable_H3(res_H0_H3_N500)
postprobcat_H3(res_H0_H3_N500)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H0_N500_H3, 
                     trueIntercept=4,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0,
                     trueSigma=1.5,
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1),
                     xlimSigma=c(1, 2))
mtext("H0, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

############################### H0, N = 600 ####################################

set.seed(1234)
DAT_H0_N600_H3 <- simANOVA(iter = 1000,
                           N = 600, 
                           meanControl = 4, 
                           sdWithinGroup = 1.5, 
                           effimpInt = 0,
                           effmentCont = 0,
                           effCombi = 0)

DA_H0_N600_H3 <- DA_ANOVA(simData = DAT_H0_N600_H3,
                          priorCoef = "normal(0, 2)", 
                          priorIntercept = "normal(4, 1.5)", 
                          priorSigma = "student_t(3, 0, 0.5)")

save(DA_H0_N600_H3, file = "./generated_data/DA_H0_N600_H3.RData")

res_H0_H3_N600 <- analyze_DA_Effects_H3(DA_H0_N600_H3)
save(res_H0_H3_N600, file = "./generated_data/res_H0_H3_N600.RData")

BFtable_H3(res_H0_H3_N600)
postprobcat_H3(res_H0_H3_N600)


par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H0_N600_H3, 
                     trueIntercept=4,
                     trueImpIntEffect=0,
                     trueMentContEffect=0,
                     trueCombiEffect=0,
                     trueSigma=1.5,
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1),
                     xlimSigma=c(1, 2))
mtext("H0, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)

############################### H1, N = 500 ####################################

set.seed(1234)
DAT_H1_N500_H3 <- simANOVA(iter = 1000,
                           N = 500, 
                           meanControl = 4, 
                           sdWithinGroup = 1.5, 
                           effimpInt = 0,
                           effmentCont = 0.5,
                           effCombi = 0.5)

DA_H1_N500_H3 <- DA_ANOVA(simData = DAT_H1_N500_H3,
                          priorCoef = "normal(0, 2)", 
                          priorIntercept = "normal(4, 1.5)", 
                          priorSigma = "student_t(3, 0, 0.5)")

save(DA_H1_N500_H3, file = "./generated_data/DA_H1_N500_H3.RData")

res_H1_H3_N500 <- analyze_DA_Effects_H3(DA_H1_N500_H3)
save(res_H1_H3_N500, file = "./generated_data/res_H1_H3_N500.RData")

BFtable_H3(res_H1_H3_N500)
postprobcat_H3(res_H1_H3_N500)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H1_N500_H3, 
                     trueIntercept=4,
                     trueImpIntEffect=0,
                     trueMentContEffect=0.5,
                     trueCombiEffect=0.5,
                     trueSigma=1.5,
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1),
                     xlimSigma=c(1, 2))
mtext("H1, N = 500", side=3, outer=TRUE, cex = 2, line = 1.7)

############################### H1, N = 600 ####################################

set.seed(1234)
DAT_H1_N600_H3 <- simANOVA(iter = 1000,
                           N = 600, 
                           meanControl = 4, 
                           sdWithinGroup = 1.5, 
                           effimpInt = 0,
                           effmentCont = 0.5,
                           effCombi = 0.5)

DA_H1_N600_H3 <- DA_ANOVA(simData = DAT_H1_N600_H3,
                          priorCoef = "normal(0, 2)", 
                          priorIntercept = "normal(4, 1.5)", 
                          priorSigma = "student_t(3, 0, 0.5)")

save(DA_H1_N600_H3, file = "./generated_data/DA_H1_N600_H3.RData")

res_H1_H3_N600 <- analyze_DA_Effects_H3(DA_H1_N600_H3)
save(res_H1_H3_N600, file = "./generated_data/res_H1_H3_N600.RData")

BFtable_H3(res_H1_H3_N600)
postprobcat_H3(res_H1_H3_N600)

par(oma=c(0,0,6,0))
analyze_DA_postMeans(DA_H1_N600_H3, 
                     trueIntercept=4,
                     trueImpIntEffect=0,
                     trueMentContEffect=0.5,
                     trueCombiEffect=0.5,
                     trueSigma=1.5,
                     xlimIntercept=c(3,5), 
                     xlimBeta=c(-1, 1),
                     xlimSigma=c(1, 2))
mtext("H1, N = 600", side=3, outer=TRUE, cex = 2, line = 1.7)
